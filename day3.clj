(ns day3
  (:require [clojure.java.io :refer [reader]]
            [clojure.string :as str]
            [clojure.set :refer [difference]]))

;; Input format:
;;
;; #<claim ID> @ <left>,<top>: <width>x<height>
;;

(defn claims
  []
  (with-open [rdr (reader "day3.txt")]
    (->> rdr
         line-seq
         (mapv (fn [s]
                 (->> (str/split s #"[ ,:x@#]")
                      (remove str/blank?)
                      (map #(Integer/parseInt %))
                      (zipmap [:id :left :top :width :height])))))))

;; Part One

(defn overlaps
  " A vec of vec of overlapping claim ids."
  [claims]
  (reduce (fn [overlaps {:keys [id left top width height]}]
            (reduce (fn [overlaps pos]
                      (update-in overlaps pos conj id))
                    overlaps
                    (for [x (range left (+ left width))
                          y (range top (+ top height))]
                      [x y])))
          (vec (repeat 1000 (vec (repeat 1000 '()))))
          claims))

(defn overlapping-surface
  [claims]
  (->> claims
       overlaps
       (mapcat seq)
       (map count)
       (filter #(<= 2 %))
       count))

(time (overlapping-surface (claims)))

;; Part Two

(defn intact-claims
  [claims]
  (->> claims
       overlaps
       (mapcat seq)
       (reduce (fn [intact-claims overlaps]
                 (if (<= 2 (count overlaps))
                   (difference intact-claims (set overlaps))
                   intact-claims))
               (set (map :id claims)))))

(time (intact-claims (claims)))
