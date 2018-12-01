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

(defn duplicates
  "A stateful transducer to only return duplicates in a sequence."
  []
  (fn [xf]
    (let [seen (volatile! #{})]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
         (if (contains? @seen input)
           (xf result input)
           (vswap! seen conj input)))))))

(defn first-frequency-reached-twice
  "The first frequency reached twice when repeatedly applying
  the change list.

  Beware! Might not return for some change lists."
  [frequency-change-list]
  (->> frequency-change-list
       cycle
       (reductions + 0)
       (sequence (duplicates))
       first))

(def tests
  [[[1 -1]          0]
   [[3 3 4 -2 -4]  10]
   [[-6 3 8 5 -6]   5]
   [[7 7 -2 -7 -4] 14]])

(doseq [[change-list freq] tests]
  (assert (= (first-frequency-reached-twice change-list)
             freq)))

(time (first-frequency-reached-twice (frequency-change-list)))
