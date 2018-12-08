(ns day8
  (:require [clojure.string :as str]))

(def license (->> (str/split (slurp "day8.txt") #"\s")
                  (mapv #(Integer/parseInt %))))

(def test-license [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])

(defn read-license
  [license]
  (let [;; For clarity we will use local state to read the license
        v (volatile! license)
        ;; Fn to consume n elements from the license
        read! (fn [n] (let [elts (take n @v)] (vswap! v #(drop n %)) elts))
        ;; Fn to consume n nodes from the license
        read-nodes! (fn read-nodes! [n]
                      (when-not (zero? n)
                        (cons (let [[n-children n-metadata] (read! 2)]
                                {:children (read-nodes! n-children)
                                 :metadata (read! n-metadata)})
                              (read-nodes! (dec n)))))]
    (first (read-nodes! 1))))

(defn check-license
  "Check the license with the given check function."
  [license check-node]
  (check-node (read-license license)))

;; Part One

(defn check1
  [{:keys [children metadata]}]
  (reduce +
          (reduce + 0 metadata)
          (map check1 children)))

(assert (= 138 (check-license test-license check1)))

(check-license license check1)

;; Part Two

(defn check2
  [{:keys [children metadata]}]
  (if (seq children)
    (reduce + 0 (map #(check2 (nth children (dec %) 0)) metadata))
    (reduce + 0 metadata)))

(assert (= 66 (check-license test-license check2)))

(check-license license check2)
