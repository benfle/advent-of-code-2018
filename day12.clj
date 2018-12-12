(ns day12
  (:require [clojure.java.io :as io]
            [clojure.set :refer [union]]))

(defn parse
  [file]
  (with-open [rdr (io/reader file)]
    (let [[state _ & rules] (line-seq rdr)]
      {:plants (->> (drop 15 (seq state))
                    (map-indexed vector)
                    (filter #(= \# (second %)))
                    (map first)
                    set)
       :rules (->> rules
                   (map (fn [s]
                          (let [[p1 p2 p3 p4 p5 _ _ _ _ p] (seq s)]
                            [[p1 p2 p3 p4 p5] p])))
                   (into {}))})))

(defn pattern
  [plants idx]
  (map #(if (contains? plants %) \# \.)
       (range (- idx 2)
              (+ idx 3))))

(declare display)

(defn keep-plant-at?
  [{:keys [plants rules] :as gen} idx]
  (let [pattern (pattern plants idx)]
    (when-let [v (rules pattern)]
      (when (= v \#)
        idx))))

(defn first-plant [gen] (apply min (:plants gen)))
(defn last-plant  [gen] (apply max (:plants gen)))

(defn next-gen
  [gen]
  (assoc gen
         :plants
         (->> (range (- (first-plant gen) 2)
                     (+ (last-plant gen)  3))
              (map (partial keep-plant-at? gen))
              (remove nil?)
              set)))

(defn display
  [{:keys [plants] :as gen}]
  (apply str (map #(if (plants %) \# \.)
                  (range (first-plant gen)
                         (inc (last-plant gen))))))

(defn sum-plants
  [gen]
  (reduce + 0 (:plants gen)))

(defn generation-at
  [file n]
  (loop [n n
         gen (parse file)]
    (if (zero? n)
      gen
      (recur (dec n) (next-gen gen)))))

;; Part One

(assert (= 325 (sum-plants (generation-at "day12-test.txt" 20))))

(time (sum-plants (generation-at "day12.txt" 20)))

;; Part Two

(defn stable-generation
  [file]
  (->> (iterate next-gen (parse file))
       (map-indexed vector)
       (partition 2)
       (some (fn [[[i1 g1] [i2 g2]]]
               (if (= (display g1) (display g2))
                 (let [s1 (sum-plants g1)
                       s2 (sum-plants g2)]
                   [i1 s1 (- s2 s1)]))))))

(stable-generation "day12.txt")

(defn sum-at
  [file n]
  (let [[i s d] (stable-generation file)]
    (+ s (* d (- n i)))))

(sum-at "day12.txt" 50000000000)
