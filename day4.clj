(ns day4
  (:require [clojure.java.io :refer [reader]]
            [clojure.set :refer [union]]))

;; Input format:
;;
;; [YYYYY-MM-DD HH:MM] Guard #<id> begins shift
;; [YYYYY-MM-DD HH:MM] falls asleep
;; [YYYYY-MM-DD HH:MM] wakes up
;;

(defn sorted-records
  "The input records, sorted chronologically."
  []
  (with-open [rdr (reader "day4.txt")]
    (->> rdr
         line-seq
         (map (fn [s]
                (let [parts (re-find #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (.+)$" s)
                      date (->> parts rest butlast (mapv #(Integer/parseInt %)))
                      action (case (last parts)
                               "falls asleep" :asleep
                               "wakes up"     :awake
                               (->> (last parts)
                                    (re-find #"#(\d+)")
                                    second
                                    Integer/parseInt))]
                  [date action])))
         (sort-by first))))

(defn records->schedules
  "A transducer to transform the sorted records into daily schedules."
  []
  (fn [xf]
    (let [current-schedule (volatile! nil)]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result [date-time action]]
         (if (number? action)
           (let [[id _ & minutes] @current-schedule]
             (vreset! current-schedule [action date-time])
             (when id
               (xf result [id (->> minutes
                                   (partition 2)
                                   (mapcat #(apply range %)))])))
           (vswap! current-schedule conj (last date-time))))))))

(defn by-guard
  "Return a map of guard -> schedules from a seq of schedules."
  [schedules]
  (->> schedules
       (group-by first)
       (map (fn [[id schedules]]
              [id (map second schedules)]))))

;; Part One

(defn minute-most-asleep
  "The minute the guard was asleep most."
  [[id schedules]]
  (->> schedules
       (apply concat)
       frequencies
       (sort-by val)
       last
       first))

(defn total-minutes-slept
  "The total number of minutes slept by a guard."
  [[id schedules]]
  (->> schedules (apply concat) count))

(def sleepy-guard
  (->> (sorted-records)
       (sequence (records->schedules))
       by-guard
       (sort-by total-minutes-slept)
       last))

(* (first sleepy-guard)
   (minute-most-asleep sleepy-guard))

;; Part Two

(def sleepy-guard
  (->> (sorted-records)
       (sequence (records->schedules))
       by-guard
       (mapcat (fn [[id schedules]]
                 (map #(apply vector id %) (->> schedules (apply concat) frequencies))))
       (sort-by last)
       last))

(* (first sleepy-guard)
   (second sleepy-guard))
