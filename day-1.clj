(ns day-1)

(defn frequency-change-list
  []
  (with-open [rdr (clojure.java.io/reader "day-1.txt")]
    (->> rdr
         line-seq
         (mapv #(Integer/parseInt %)))))

;; Part One

(reduce + (frequency-change-list))

;; Part Two

(defn duplicate?
  "A stateful predicate to keep duplicates in a sequence."
  []
  (let [seen (volatile! #{})]
    (fn [input]
      (if (contains? @seen input)
        input
        (do
          (vswap! seen conj input)
          nil)))))

(defn first-frequency-reached-twice
  "The first frequency reached twice when repeatedly applying
  the change list.

  Beware! Might not return for some change lists."
  [frequency-change-list]
  (->> frequency-change-list
       repeat
       (sequence cat)
       (reductions + 0)
       (keep (duplicate?))
       first))

(def tests
  [[[1 -1]          0]
   [[3 3 4 -2 -4]  10]
   [[-6 3 8 5 -6]   5]
   [[7 7 -2 -7 -4] 14]])

(doseq [[change-list freq] tests]
  (assert (= (first-frequency-reached-twice change-list)
             freq)))

(first-frequency-reached-twice (frequency-change-list))
