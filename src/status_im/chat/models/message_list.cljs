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

;; any message that comes after this amount of ms will be grouped separately
(def ^:private group-ms 60000)

(defn same-group? [a b]
  (and
   (= (:from a) (:from b))
   (<= (js/Math.abs (- (:whisper-timestamp a) (:whisper-timestamp b))) group-ms)))

(defn display-photo? [{:keys [outgoing message-type]}]
  (or (= :system-message message-type)
      (and (not outgoing)
           (not (= :user-message message-type)))))

(defn compare-fn [a b]
  (let [initial-comparison (compare (:clock-value b) (:clock-value a))]
    (if (= initial-comparison 0)
      (compare (:message-id a) (:message-id b))
      initial-comparison)))

(defn add-group-info [current-message previous-message next-message]
  (assoc current-message
         :first?          (nil? previous-message)
         :first-in-group? (or (nil? previous-message)
                              (not (same-group? current-message previous-message)))
         :last-in-group?  (or (nil? next-message)
                              (not (same-group? current-message next-message)))
         :display-photo? (display-photo? current-message)))

(defn update-next-message [current-message next-message]
  (assoc next-message :first-in-group? (not (same-group? current-message next-message))))

(defn update-previous-message [current-message previous-message]
  (assoc previous-message
         :first?          false
         :last-in-group?  (not (same-group? current-message previous-message))))

(defn insert-message [old-stream prepared-message]
  (let [[stream inserted-at previous-at next-at]
        (linked/add old-stream
                    prepared-message)
        previous-message (linked/get-at stream previous-at)
        next-message (linked/get-at stream next-at)
        message-with-pos-data (add-group-info prepared-message previous-message next-message)]
    (cond-> (linked/update-at stream inserted-at (constantly message-with-pos-data))
      next-message
      (linked/update-at next-at #(update-next-message message-with-pos-data %))
      previous-message
      (linked/update-at previous-at #(update-previous-message message-with-pos-data %)))))

(defn add-message [stream message]
  (insert-message (or stream (linked/build compare-fn))
                  (prepare-message message)))

(defn concat-streams [stream messages]
  (reduce add-message stream messages))

(defn build [messages]
  (reduce add-message (linked/build compare-fn) messages))

