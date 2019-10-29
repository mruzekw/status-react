(ns status-im.test.chat.models.message-list
  (:require [cljs.test :refer-macros [deftest is testing]]
            [status-im.chat.models.linked-list :as linked]
            [status-im.constants :as const]
            [taoensso.tufte :as tufte :refer-macros (defnp p profiled profile)]
            [status-im.chat.models.loading :as l]

            [status-im.chat.models.message-list :as s]))

#_(deftest test-message-datemark-groups
    (testing "it orders a map of messages by clock-values desc, breaking ties by message-id asc and removing hidden messages"
      (let [message-1 {:show? true
                       :message-id "doesn't matter 1"
                       :clock-value 1}
            message-2 {:show? true
                       :message-id "doesn't matter 2"
                       :clock-value 2}
            message-3 {:show? true
                       :message-id "does matter 2"
                       :clock-value 3}
            message-4 {:show? true
                       :message-id "does matter 1"
                       :clock-value 3}
            hidden-message {:show? false
                            :clock-value 1}
            unordered-messages (->> [message-1
                                     message-2
                                     message-3
                                     message-4
                                     hidden-message]
                                    (map (juxt :message-id identity))
                                    shuffle ; clojure maps are sorted for n <= 32
                                    (into {}))]
        (is (= [message-4
                message-3
                message-2
                message-1] (s/sort-messages unordered-messages))))))

#_(deftest intersperse-datemarks
    (testing "it mantains the order even when timestamps are across days"
      (let [message-1 {:whisper-timestamp 946641600000} ; 1999}
            message-2 {:whisper-timestamp 946728000000} ; 2000 this will displayed in 1999
            message-3 {:whisper-timestamp 946641600000} ; 1999
            message-4 {:whisper-timestamp 946728000000} ; 2000
            ordered-messages [message-4
                              message-3
                              message-2
                              message-1]
            [m1 d1 m2 m3 m4 d2] (s/intersperse-datemarks ordered-messages)]
        (is (= "Jan 1, 2000"
               (:datemark m1)))
        (is (= {:type :datemark
                :value "Jan 1, 2000"} d1))
        (is (= "Dec 31, 1999"
               (:datemark m2)
               (:datemark m3)
               (:datemark m4)))
        (is (= {:type :datemark
                :value "Dec 31, 1999"} d2)))))

(deftest message-stream-tests
  (testing "messages with no interspersed datemarks"
    (let [m1 {:from "1"
              :clock-value 1
              :message-id "1"
              :whisper-timestamp 1
              :datemark "a"
              :outgoing false}
          m2   {:from "2"
                :clock-value 2
                :message-id "2"
                :whisper-timestamp 2
                :datemark "a"
                :outgoing true}
          m3    {:from "2"
                 :clock-value 3
                 :message-id "3"
                 :whisper-timestamp 3
                 :datemark "a"
                 :outgoing true}
          messages [m1 m2 m3]
          [actual-m1
           actual-m2
           actual-m3] (vals (s/build messages))]
      (println (s/build messages))
      (testing "it sorts them correclty"
        (is (= "3" (:message-id actual-m1)))
        (is (= "2" (:message-id actual-m2)))
        (is (= "1" (:message-id actual-m3))))

      (testing "it marks only the first message as :first?"
        (is (:first? actual-m1))
        (is (not (:first? actual-m2)))
        (is (not (:first? actual-m3))))
      (testing "it marks the first outgoing message as :last-outgoing?"
        (is (not (:last-outgoing? actual-m1)))
        (is (:last-outgoing? actual-m2))
        (is (not (:last-outgoing? actual-m3))))
      (testing "it marks messages from the same author next to another with :first-in-group?"
        (is (:first-in-group? actual-m1))
        (is (not (:first-in-group? actual-m2)))
        (is (:first-in-group? actual-m3)))
      (testing "it marks messages with display-photo? when they are not outgoing and we are in a group chat"
        (is (:display-photo? actual-m1))
        (is (not (:display-photo? actual-m2)))
        (is (not (:display-photo? actual-m3))))
      (testing "it marks messages with display-username? when we display the photo and are the first in a group"
        (is (:display-username? actual-m1))
        (is (not (:display-username? actual-m2)))
        (is (not (:display-username? actual-m3))))
      (testing "it marks the last message from the same author with :last-in-group?"
        (is (:last-in-group? actual-m1))
        (is (:last-in-group? actual-m2))
        (is (not (:last-in-group? actual-m3))))))
  (testing "messages with interspersed datemarks"
    (let [m1 {:from "2"          ; first & last in group
              :whisper-timestamp 63000
              :outgoing true}
          m2  {:from "2"         ; first & last in group as more than 1 minute after previous message
               :whisper-timestamp 62000
               :outgoing false}
          m3  {:from "2"         ; last in group
               :whisper-timestamp 1
               :outgoing false}
          m4  {:from "2"         ; first in group
               :whisper-timestamp 0
               :outgoing false}
          messages [m1 m2 m3 m4]
          [actual-m1
           actual-m2
           actual-m3
           actual-m4] (vals (s/build messages))]
      (testing "it marks the first outgoing message as :last-outgoing?"
        (is (:last-outgoing? actual-m1))
        (is (not (:last-outgoing? actual-m2)))
        (is (not (:last-outgoing? actual-m3)))
        (is (not (:last-outgoing? actual-m4))))
      (testing "it sets :first-in-group? after a datemark"
        (is (:first-in-group? actual-m1))
        (is (:first-in-group? actual-m4)))
      (testing "it sets :first-in-group? if more than 60s have passed since last message"
        (is (:first-in-group? actual-m2)))
      (testing "it sets :last-in-group? after a datemark"
        (is (:last-in-group? actual-m1))
        (is (:last-in-group? actual-m2))
        (is (:last-in-group? actual-m3))
        (is (not (:last-in-group? actual-m4)))))))

(def ascending-range (mapv
                      #(let [i (+ 100000 %)]
                         {:clock-value i
                          :whisper-timestamp i
                          :timestamp i
                          :message-id (str i)})
                      (range 2000)))

(def descending-range (reverse ascending-range))

(def random-range (shuffle ascending-range))

(defnp build-message-list [messages]
  (s/build messages))

(defnp append-to-message-list [l message]
  (s/add-message l message))

(defnp prepend-to-message-list [l message]
  (s/add-message l message))

(defnp insert-close-to-head-message-list [l message]
  (s/add-message l message))

(defnp insert-middle-message-list [l message]
  (s/add-message l message))

(defnp append-many-to-message-list []
  (reduce (fn [acc e]
            (s/add-message acc e))
          (s/build [])
          ascending-range))

(defnp prepend-many-to-message-list []
  (reduce (fn [acc e]
            (s/add-message acc e))
          (s/build [])
          descending-range))

(defnp random-many-to-message-list []
  (reduce (fn [acc e]
            (s/add-message acc e))
          (s/build [])
          random-range))

(tufte/add-basic-println-handler! {:format-pstats-opts {:columns [:n-calls :mean :min :max :clock :total]
                                                        :format-id-fn name}})

#_(deftest benchmark-list
    (let [messages (sort-by :timestamp (mapv (fn [i] (let [i (+ 100000 i 1)] {:timestamp i :clock-value i :message-id (str i) :whisper-timestamp i})) (range 10000)))
          built-list (s/build messages)]
      #_(testing "testing building a list from scratch"
          (profile {} (dotimes [_ 10] (build-message-list messages))))
      (testing "prepending to list"
        (profile {} (dotimes [_ 10] (prepend-to-message-list
                                     built-list
                                     {:clock-value 200000
                                      :message-id "200000"
                                      :whisper-timestamp 21
                                      :timestamp 21}))))
      (testing "append to list"
        (profile {} (dotimes [_ 10] (append-to-message-list
                                     built-list
                                     {:clock-value 100000
                                      :message-id "100000"
                                      :whisper-timestamp 100000
                                      :timestamp 100000}))))
      (testing "insert close to head"
        (profile {} (dotimes [_ 10] (insert-close-to-head-message-list
                                     built-list
                                     {:clock-value 109970
                                      :message-id "109970"
                                      :whisper-timestamp 1000
                                      :timestamp 1000}))))
      (testing "insert into the middle list"
        (profile {} (dotimes [_ 10] (insert-middle-message-list
                                     built-list
                                     {:clock-value 105000
                                      :message-id "10500"
                                      :whisper-timestamp 1000
                                      :timestamp 1000}))))
      (testing "append many to message list"
        (profile {} (dotimes [_ 10] (append-many-to-message-list))))
      (testing "prepend many to message list"
        (profile {} (dotimes [_ 10] (prepend-many-to-message-list))))
      #_(testing "random many to message list"
          (profile {} (dotimes [_ 10] (random-many-to-message-list))))))

(deftest message-list
  (let [current-messages [{:clock-value 109
                           :message-id "109"
                           :timestamp 9
                           :whisper-timestamp 9}
                          {:clock-value 106
                           :message-id "106"
                           :timestamp 6
                           :whisper-timestamp 6}
                          {:clock-value 103
                           :message-id "103"
                           :timestamp 3
                           :whisper-timestamp 3}]
        current-list (s/build current-messages)]
    (testing "inserting a newer message"
      (let [new-message {:timestamp 12
                         :clock-value 112
                         :message-id "112"
                         :whisper-timestamp 12}]
        (is (= 112
               (-> (s/add-message current-list new-message)
                   first
                   :clock-value)))))
    (testing "inserting an older message"
      (let [new-message {:timestamp 0
                         :clock-value 100
                         :message-id "100"
                         :whisper-timestamp 0}]
        (is (= 100
               (-> (s/add-message current-list new-message)
                   last
                   :clock-value)))))
    (testing "inserting in the middle of the list"
      (let [new-message {:timestamp 7
                         :clock-value 107
                         :message-id "107"
                         :whisper-timestamp 7}]
        (is (= 107
               (-> (s/add-message current-list new-message)
                   (nth 1)
                   :clock-value)))))
    (testing "inserting in the middle of the list, clock-value clash"
      (let [new-message {:timestamp 6
                         :clock-value 106
                         :message-id "106a"
                         :whisper-timestamp 6}]
        (is (= "106a"
               (-> (s/add-message current-list new-message)
                   (nth 2)
                   :message-id)))))))

#_(deftest test-implementation
    (testing "mapping over?"
      (let [l (-> (linked/build)
                  (conj 3)
                  (conj 7)
                  (conj 1)
                  (conj 5))]
        (is (= [1 3 5 7] (reduce (fn [acc el]
                                   (conj acc el)) [] l)))))
    (testing "add middle"
      (let [l (-> (linked/build)
                  (conj 3)
                  (conj 7)
                  (conj 1)
                  (conj 5))]
        (let [[_ last-added] (linked/add l 2)]
          (= [2 2 0]
             last-added))))
    (testing "add tail"
      (let [l (-> (linked/build)
                  (conj 3)
                  (conj 7)
                  (conj 1)
                  (conj 5))]
        (let [[_ last-added] (linked/add l 9)]
          (= [9 9 nil]
             last-added)))))

(def built-list (reduce (fn [acc e] (conj acc e)) (linked/build s/compare-fn) ascending-range))

(defnp test-hash-map-ascending []
  (reduce (fn [acc e] (assoc acc (:message-id e) e)) {} ascending-range))

(defnp test-hash-map-random []
  (reduce (fn [acc e] (assoc acc (:message-id e) e)) {} random-range))

(defnp test-sorted-hash-map-ascending []
  (reduce (fn [acc e] (assoc acc (:message-id e) e)) (sorted-map) ascending-range))

(defnp test-sorted-hash-map-random []
  (reduce (fn [acc e] (assoc acc (:message-id e) e)) (sorted-map) random-range))

(defnp test-linked-list-ascending []
  (reduce (fn [acc e] (conj acc e)) (linked/build s/compare-fn) ascending-range))

(defnp test-linked-list-descending []
  (reduce (fn [acc e] (conj acc e)) (linked/build s/compare-fn) descending-range))

(defnp test-linked-list-random []
  (reduce (fn [acc e] (conj acc e)) (linked/build s/compare-fn) random-range))

(defnp test-linked-list-iterate []
  (mapv identity built-list))

(defnp test-vector-iterate []
  (mapv identity ascending-range))

#_(deftest profile-linked-list
    (testing "linked list performance"
      (profile {} (dotimes [_ 10] (test-hash-map-ascending)))
      (profile {} (dotimes [_ 10] (test-hash-map-random)))
      (profile {} (dotimes [_ 10] (test-sorted-hash-map-ascending)))
      (profile {} (dotimes [_ 10] (test-sorted-hash-map-random)))
      (profile {} (dotimes [_ 10] (test-linked-list-ascending)))
      (profile {} (dotimes [_ 10] (test-linked-list-descending)))
    ;(profile {} (dotimes [_ 10] (test-linked-list-random)))
      (profile {} (dotimes [_ 10] (test-vector-iterate)))
      (profile {} (dotimes [_ 10] (test-linked-list-iterate)))))
