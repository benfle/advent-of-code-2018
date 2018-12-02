(ns day2
  (:require [clojure.java.io :refer [reader]]
            [clojure.set :refer [map-invert]]
            [day1]))

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

(defn delete-char-at
  [s idx]
  (str (subs s 0 idx)
       (subs s (inc idx))))

(defn common-letters
  [box-ids]
  (some (fn [idx]
          (->> box-ids
               (map #(delete-char-at % idx))
               (sequence (day1/duplicates))
               first))
        (range)))

(def test-box-ids
  ["abcde"
   "fghij"
   "klmno"
   "pqrst"
   "fguij"
   "axcye"
   "wvxyz"])

(assert (= "fgij"
           (common-letters test-box-ids)))

(common-letters (box-ids))
