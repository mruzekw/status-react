(ns status-im.chat.models.message-list
  (:require
   [status-im.chat.models.linked-list :as linked]
   [status-im.utils.datetime :as time]))

(defn- intersperse-datemark
  "Reduce step which expects the input list of messages to be sorted by clock value.
  It makes best effort to group them by day.
  We cannot sort them by :timestamp, as that represents the clock of the sender
  and we have no guarantees on the order.
  We naively and arbitrarly group them assuming that out-of-order timestamps
  fall in the previous bucket.
  A sends M1 to B with timestamp 2000-01-01T00:00:00
  B replies M2 with timestamp    1999-12-31-23:59:59
  M1 needs to be displayed before M2
  so we bucket both in 1999-12-31"
  [{:keys [acc last-timestamp last-datemark]} {:keys [whisper-timestamp datemark] :as msg}]
  (cond (empty? acc)                                     ; initial element
        {:last-timestamp whisper-timestamp
         :last-datemark  datemark
         :acc            (conj acc msg)}

        (and (not= last-datemark datemark)               ; not the same day
             (< whisper-timestamp last-timestamp))               ; not out-of-order
        {:last-timestamp whisper-timestamp
         :last-datemark  datemark
         :acc            (conj acc {:value last-datemark ; intersperse datemark message
                                    :clock-value (:clock-value msg)
                                    :type  :datemark}
                               msg)}
        :else
        {:last-timestamp (max whisper-timestamp last-timestamp)  ; use last datemark
         :last-datemark  last-datemark
         :acc            (conj acc (assoc msg :datemark last-datemark))}))

(defn- add-datemark [{:keys [whisper-timestamp] :as msg}]
  (assoc msg :datemark (time/day-relative whisper-timestamp)))

(defn- add-timestamp [{:keys [whisper-timestamp] :as msg}]
  (assoc msg :timestamp-str (time/timestamp->time whisper-timestamp)))

(defn prepare-message [message]
  (-> message
      add-datemark
      add-timestamp))

(defn intersperse-datemarks
  "Add a datemark in between an ordered seq of messages when two datemarks are not
  the same. Ignore messages with out-of-order timestamps"
  [messages]
  (when (seq messages)
    (let [messages-with-datemarks (transduce (map prepare-message)
                                             (completing intersperse-datemark :acc)
                                             {:acc []}
                                             messages)]
      ; Append last datemark
      (conj messages-with-datemarks {:value (:datemark (peek messages-with-datemarks))
                                     :clock-value (:clock-value (peek messages-with-datemarks))
                                     :type  :datemark}))))

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

(defn add-positional-metadata-to-message [{:keys [type message-type from datemark outgoing whisper-timestamp] :as message}
                                          {:keys [last-outgoing-seen?] :as previous-message}]
  (let [;; Was the previous message from a different author or this message
        ;; comes after x ms
        last-in-group?           (or (= :system-message message-type)
                                     (not= from (:from previous-message))
                                     (> (- (:whisper-timestamp previous-message) whisper-timestamp) group-ms))
        ;; Have we seen an outgoing message already?
        last-outgoing?           (and (not last-outgoing-seen?)
                                      outgoing)]
    (assoc message
           :last?           false
           :display-photo?  (display-photo? message)
           :last-in-group?  last-in-group?
           :last-outgoing-seen? last-outgoing-seen?
           :last-outgoing?  last-outgoing?)))

(defn add-positional-metadata
  "Reduce step which adds positional metadata to a message and conditionally
  update the previous message with :first-in-group?."
  [stream
   {:keys [type message-type from datemark outgoing whisper-timestamp] :as message}]
  (let [previous-message         (peek stream)
        {:keys [last-outgoing?
                last-in-group?]
         :as new-message}         (add-positional-metadata-to-message message previous-message)
        datemark?                (= :datemark type)
        ;; If this is a datemark or this is the last-message of a group,
        ;; then the previous message was the first
        previous-first-in-group? (or datemark?
                                     last-in-group?)]
    (cond-> stream
      previous-first-in-group?
      ;; update previous message if necessary
      set-previous-message-info
      :always
      (conj new-message))))

(defn messages-stream
  "Enhances the messages in message sequence interspersed with datemarks
  with derived stream context information, like:
  `:first-in-group?`, `last-in-group?`, `:last?` and `:last-outgoing?` flags."
  [ordered-messages]
  (when (seq ordered-messages)
    (let [initial-message (first ordered-messages)
          last-outgoing? (:outgoing initial-message)
          message-with-metadata (assoc initial-message
                                       :last-in-group? true
                                       :last? true
                                       :display-photo? (display-photo? initial-message)
                                       :last-outgoing-seen? last-outgoing?
                                       :last-outgoing? last-outgoing?)]
      (->> (rest ordered-messages)
           (reduce add-positional-metadata
                   [message-with-metadata])))))

(defn compare-fn [a b]
  (let [initial-comparison (compare (:clock-value b) (:clock-value a))]
    (if (= initial-comparison 0)
      (compare (:message-id a) (:message-id b))
      initial-comparison)))

(defn initialize [prepared-message]
  (conj (linked/build compare-fn)
        prepared-message))

(defn insert-message [old-stream prepared-message]
  (let [[stream inserted-at previous-at next-at]
        (linked/add old-stream
                    prepared-message)
        previous-message (linked/get-at stream previous-at)
        next-message (linked/get-at stream next-at)]
    ;; HEAD
    (cond (nil? previous-at)
          (linked/update-at stream inserted-at #(assoc % :last-in-group? true
                                                       :last? true
                                                       :display-photo? (display-photo? prepared-message)
                                                       :last-outgoing? (:outgoing prepared-message)))

        ;; Appending, nothing to do
          (nil? next-at)
          stream

        ;; inserted in the middle
          :else
          (let [message-with-pos-data (add-positional-metadata-to-message prepared-message previous-message)]
            (linked/update-at stream next-at #(add-positional-metadata-to-message % message-with-pos-data))))))

(defn add-message [stream message]
  (let [prepared-message (prepare-message message)]
    ;; Outside the range, we don't add it
    (if (= (count stream) 0)
      (initialize prepared-message)
      (insert-message stream prepared-message))))

(defn concat-streams [stream messages]
  (reduce add-message stream messages))

(defn build [messages]
  (reduce add-message (linked/build compare-fn) messages))

