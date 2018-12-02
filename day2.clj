(ns day2
  (:require [clojure.java.io :refer [reader]]
            [clojure.data :refer [diff]]
            [clojure.set :refer [map-invert]]))

;; Part One

(defn box-ids
  []
  (with-open [rdr (reader "day2.txt")]
    (->> rdr
         line-seq
         doall)))

(defn exactly-n
  "Return a predicate to test whether a string contains
  a character that appears exactly n times."
  [n]
  (fn [s]
    (-> s
        frequencies
        map-invert
        (contains? n))))

(defn checksum
  [box-ids]
  (* (count (filter (exactly-n 2) box-ids))
     (count (filter (exactly-n 3) box-ids))))

(def test-box-ids
  ["abcdef"
   "bababc"
   "abbcde"
   "abcccd"
   "aabcdd"
   "abcdee"
   "ababab"])

(assert (= 12 (checksum test-box-ids)))

(checksum (box-ids))

;; Part Two

(defn similar?
  "Whether the two box ids differ by exactly 1 character."
  [s1 s2]
  (loop [[h1 & t1] (seq s1)
         [h2 & t2] (seq s2)
         different? false]
    (cond
      (= h1 h2 nil) different?
      (= h1 h2)     (recur t1 t2 different?)
      different?    false
      :else         (recur t1 t2 true))))

(defn similar-box-ids
  "A seq of seq of similar box ids."
  [box-ids]
  (loop [[head & tail] box-ids
         res #{}]
    (if-not head
      res
      (recur tail
             (if-let [similars (->> tail
                                    (filter #(similar? % head))
                                    seq)]
               (conj res (set (into [head] similars)))
               res)))))

(defn common-letters
  [similar-box-ids]
  (->> similar-box-ids
       (map seq)
       (apply diff)
       last
       (remove nil?)
       (apply str)))

(def test-box-ids
  ["abcde"
   "fghij"
   "klmno"
   "pqrst"
   "fguij"
   "axcye"
   "wvxyz"])

(assert (= #{#{"fguij" "fghij"}}
           (similar-box-ids test-box-ids)))

(similar-box-ids (box-ids))

(common-letters (first (similar-box-ids (box-ids))))
