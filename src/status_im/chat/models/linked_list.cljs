(ns status-im.chat.models.linked-list)

(def value-index 0)
(def previous-index 1)
(def next-index 2)

(defprotocol ILinkedList
  (add [this v])
  (get-at [this k])
  (update-at [this k f])
  (last-added [this]))

(deftype LinkedListIterator [obj ^:mutable n]
  Object
  (hasNext [_]
    (seq n))
  (next [_]
    (let [ret (get obj n)]
      (when ret
        (set! n (.-n ret)))
      ret)))

(deftype LinkedList [compare-fn obj head tail meta ^:mutable __hash]
  ILinkedList
  (get-at [coll i]
    (get-in obj [i value-index]))
  (update-at [coll i f]
    (LinkedList.
     compare-fn
     (update-in obj [i value-index] f)
     head
     tail
     meta
     __hash))
  (add [coll v]
    (let [c (conj coll v)
          [_ previous-at next-at] (last-added c)]
      [c
       (count obj)
       previous-at
       next-at]))
  (last-added [coll]
    (peek obj))
  Object
  (toString [coll]
    (println obj))
  (equiv [this other]
    (-equiv this other))
  IPrintWithWriter
  (-pr-writer [coll writer opts]
    (println (reduce (fn [s element]
                       (str s element "-->"))
                     ""
                     coll)))
  ISeq
  (-first [_]
    ((obj head) value-index))
  (-rest [this]
    (rest (seq this)))
  ISeqable
  (-seq [this]
    (loop [acc []
           e head]
      (if e
        (let [element (obj e)]
          (recur (conj acc (element value-index))
                 (element next-index)))
        acc)))
  ICollection
  (-conj [coll v]
    (cond

      (zero? (count obj))
      (LinkedList. compare-fn (conj obj [v nil nil]) 0 0 meta __hash)

     ;; Prepend
      (= 1 (compare-fn ((obj head) value-index) v))
      (LinkedList.
       compare-fn
       (-> obj
           (update head assoc previous-index (count obj))
           (conj [v nil head]))
       (count obj)
       tail
       meta
       __hash)

     ;; Append
      (= -1 (compare-fn ((obj tail) value-index) v))
      (LinkedList.
       compare-fn
       (-> (conj obj [v tail nil])
           (update tail assoc next-index (count obj)))
       head
       (count obj)
       meta
       __hash)

      :else
     ;; INsert in the middle
      (let [[previous-element next-element] (loop [current-element head
                                                   previous-element nil]

                                              (let [result (compare-fn v ((obj current-element) value-index))]
                                                (if (or (= result -1)
                                                        (= result 0))
                                                  [previous-element current-element]
                                                  (recur ((obj current-element) next-index)
                                                         current-element))))]
        (LinkedList. compare-fn
                     (-> (conj obj [v previous-element next-element])
                         (update previous-element assoc next-index (count obj))
                         (update next-element assoc previous-index (count obj)))
                     head
                     tail
                     meta
                     __hash))))
  ICounted
  (-count [coll] (count obj)))

(set! (.-EMPTY LinkedList) (LinkedList. compare [] nil nil nil empty-unordered-hash))

(defn build
  ([]
   (LinkedList. compare [] nil nil nil empty-unordered-hash))
  ([compare-fn]
   (LinkedList. compare-fn [] nil nil nil empty-unordered-hash)))

