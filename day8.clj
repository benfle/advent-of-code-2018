(ns day8
  (:require [clojure.string :as str]
            [clojure.zip :as zip]))

(def license (->> (str/split (slurp "day8.txt") #"\s")
                  (mapv #(Integer/parseInt %))))

(def test-license [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])

(declare read-node)

(defn read-license
  [license]
  (let [v (volatile! license)
        read-n (fn [n] (let [res (take n @v)] (vswap! v #(drop n %)) res))
        read-n-node (fn read-n-node [n]
                      (when-not (zero? n)
                        (cons (let [[n-children n-metadata] (read-n 2)
                                    children (read-n-node n-children)
                                    metadata (read-n n-metadata)]
                                (cond-> {}
                                  (seq children) (assoc :children children)
                                  (seq metadata) (assoc :metadata metadata)))
                              (read-n-node (dec n)))))]
    (first (read-n-node 1))))

(defn check-license
  [license check-node]
  (check-node (read-license license)))

;; Part One

(defn check1
  [{:keys [metadata children]}]
  (reduce +
          (reduce + 0 metadata)
          (map check1 children)))

(assert (= 138 (check-license test-license check1)))

(check-license license check1)

;; Part Two

(defn check2
  [{:keys [metadata children]}]
  (if (seq children)
    (reduce + 0 (map #(check2 (nth children (dec %) 0)) metadata))
    (reduce + 0 metadata)))

(assert (= 66 (check-license test-license check2)))

(check-license license check2)
