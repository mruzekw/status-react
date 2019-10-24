(ns status-im.chat.db
  (:require [clojure.set :as clojure.set]
            [clojure.string :as string]
            [status-im.chat.commands.core :as commands]
            [status-im.chat.commands.input :as commands.input]
            [status-im.multiaccounts.core :as multiaccounts]
            [status-im.contact.db :as contact.db]
            [status-im.group-chats.db :as group-chats.db]
            [status-im.mailserver.constants :as mailserver.constants]
            [status-im.utils.gfycat.core :as gfycat]))

(defn group-chat-name
  [{:keys [public? name]}]
  (str (when public? "#") name))

(defn enrich-active-chat
  [contacts {:keys [chat-id public? group-chat name] :as chat} current-public-key]
  (if group-chat
    (let [pending-invite-inviter-name
          (group-chats.db/get-pending-invite-inviter-name contacts
                                                          chat
                                                          current-public-key)
          inviter-name
          (group-chats.db/get-inviter-name contacts
                                           chat
                                           current-public-key)]
      (cond-> chat
        pending-invite-inviter-name
        (assoc :pending-invite-inviter-name pending-invite-inviter-name)
        inviter-name
        (assoc :inviter-name inviter-name)
        :always
        (assoc :chat-name (group-chat-name chat))))
    (let [{contact-name :name :as contact}
          (get contacts chat-id
               (contact.db/public-key->new-contact chat-id))
          random-name (gfycat/generate-gfy chat-id)]
      (-> chat
          (assoc :contact contact
                 :chat-name (multiaccounts/displayed-name contact)
                 :name contact-name
                 :random-name random-name)
          (update :tags clojure.set/union (:tags contact))))))

(defn active-chats
  [contacts chats {:keys [public-key]}]
  (reduce-kv (fn [acc chat-id {:keys [is-active] :as chat}]
               (if is-active
                 (assoc acc
                        chat-id
                        (enrich-active-chat contacts chat public-key))
                 acc))
             {}
             chats))

(defn quoted-message-data
  "Selects certain data from quoted message which must be available in the view"
  [message-id messages]
  (when-let [{:keys [from content]} (get messages message-id)]
    {:from from
     :text (:text content)}))

(defn datemark? [{:keys [type]}]
  (= type :datemark))

(defn gap? [{:keys [type]}]
  (= type :gap))

(defn check-gap
  [gaps previous-message next-message]
  (let [previous-timestamp     (:whisper-timestamp previous-message)
        next-whisper-timestamp (:whisper-timestamp next-message)
        next-timestamp         (:timestamp next-message)
        ignore-next-message?   (> (js/Math.abs
                                   (- next-whisper-timestamp next-timestamp))
                                  120000)]
    (if (or ignore-next-message?
            (not next-message))
      {:gaps-number 0
       :gap nil}
      (reduce
       (fn [acc {:keys [from to id]}]
         (if (or
              (and (nil? previous-timestamp)
                   (> from next-whisper-timestamp))
              (and
               (> previous-timestamp from)
               (> to next-whisper-timestamp))
              (and
               (> from previous-timestamp)
               (> to next-whisper-timestamp)))
           (-> acc
               (update :gaps-number inc)
               (update-in [:gap :ids] conj id))
           (reduced acc)))
       {:gaps-number 0
        :gap         nil}
       gaps))))

(defn add-gap [messages gaps]
  (conj messages
        {:type  :gap
         :value (clojure.string/join (:ids gaps))
         :gaps gaps}))

(defn add-gaps
  "Add gaps to a message-stream"
  [message-list messages-gaps
   {:keys [highest-request-to lowest-request-from]} all-loaded? public?]
  (transduce
   (map identity)
   (fn
     ([]
      (let [acc {:messages         []
                 :previous-message nil
                 :gaps             messages-gaps}]
        acc))
     ([{:keys [messages datemark-reference previous-message gaps]} message]
      (let [{:keys [gaps-number gap]}
            (check-gap gaps previous-message message)
            add-gap?      (pos? gaps-number)]
        {:messages           (cond-> (conj messages message)

                               add-gap?
                               (add-gap gap))
         :previous-message   message
         :gaps               (if add-gap?
                               (drop gaps-number gaps)
                               gaps)}))
     ([{:keys [messages gaps]}]
      (cond-> messages
        (and
         public?
         all-loaded?
         (not (nil? highest-request-to))
         (not (nil? lowest-request-from))
         (< (- highest-request-to lowest-request-from)
            mailserver.constants/max-gaps-range))
        (conj {:type       :gap
               :value      (str :first-gap)
               :first-gap? true})
        (seq gaps)
        (add-gap {:ids (map :id gaps)}))))
   message-list))

(defn- set-previous-message-info [stream]
  (let [{:keys [display-photo? message-type] :as previous-message} (peek stream)]
    (conj (pop stream) (assoc previous-message
                              :display-username? (and display-photo?
                                                      (not= :system-message message-type))
                              :first-in-group?   true))))

(defn display-photo? [{:keys [outgoing message-type]}]
  (or (= :system-message message-type)
      (and (not outgoing)
           (not (= :user-message message-type)))))

;; any message that comes after this amount of ms will be grouped separately
(def ^:private group-ms 60000)

(defn add-positional-metadata
  "Reduce step which adds positional metadata to a message and conditionally
  update the previous message with :first-in-group?."
  [{:keys [stream last-outgoing-seen]}
   {:keys [type message-type from datemark outgoing timestamp] :as message}]
  (let [previous-message         (peek stream)
        ;; Was the previous message from a different author or this message
        ;; comes after x ms
        last-in-group?           (or (= :system-message message-type)
                                     (not= from (:from previous-message))
                                     (> (- (:timestamp previous-message) timestamp) group-ms))
        ;; Have we seen an outgoing message already?
        last-outgoing?           (and (not last-outgoing-seen)
                                      outgoing)
        datemark?                (= :datemark (:type message))
        ;; If this is a datemark or this is the last-message of a group,
        ;; then the previous message was the first
        previous-first-in-group? (or datemark?
                                     last-in-group?)
        new-message              (assoc message
                                        :display-photo?  (display-photo? message)
                                        :last-in-group?  last-in-group?
                                        :last-outgoing?  last-outgoing?)]
    {:stream             (cond-> stream
                           previous-first-in-group?
                           ;; update previous message if necessary
                           set-previous-message-info

                           :always
                           (conj new-message))
     ;; mark the last message sent by the user
     :last-outgoing-seen (or last-outgoing-seen last-outgoing?)}))

(defn messages-stream
  "Enhances the messages in message sequence interspersed with datemarks
  with derived stream context information, like:
  `:first-in-group?`, `last-in-group?`, `:last?` and `:last-outgoing?` flags."
  [ordered-messages]
  (when (seq ordered-messages)
    (let [initial-message (first ordered-messages)
          message-with-metadata (assoc initial-message
                                       :last-in-group? true
                                       :last? true
                                       :display-photo? (display-photo? initial-message)
                                       :last-outgoing? (:outgoing initial-message))]
      (->> (rest ordered-messages)
           (reduce add-positional-metadata
                   {:stream             [message-with-metadata]
                    :last-outgoing-seen (:last-outgoing? message-with-metadata)})
           :stream))))

(def map->sorted-seq
  (comp (partial map second) (partial sort-by first)))

(defn available-commands
  [commands {:keys [input-text]}]
  (->> commands
       map->sorted-seq
       (filter (fn [{:keys [type]}]
                 (when (commands.input/starts-as-command? input-text)
                   (string/includes? (commands/command-name type) input-text))))))
