(ns day6
  (:require [clojure.java.io :refer [reader]]))

;; input format: <x>, <y>

(defn parse-coordinates
  [line]
  (->> line
       (re-find #"(\d+), (\d+)")
       rest
       (map #(Integer/parseInt %))
       (zipmap [:x :y])))

(defn coordinates
  []
  (with-open [rdr (reader "day6.txt")]
    (->> rdr
         line-seq
         (mapv parse-coordinates))))

(defn manhattan-distance
  [p1 p2]
  (+ (Math/abs (- (:x p1) (:x p2)))
     (Math/abs (- (:y p1) (:y p2)))))

(defn closest
  "Return the point closest to p or nil if equidistant."
  [distance points p]
  (let [closest (->> points
                     (group-by (partial distance p))
                     (sort-by key)
                     first
                     second)]
    (when (= 1 (count closest))
      (first closest))))

(defn bounding-box
  "The bounding box of the coordinates as a seq of points."
  [coordinates]
  {:origin {:x (apply min (map :x coordinates))
            :y (apply min (map :y coordinates))}
   :corner {:x (apply max (map :x coordinates))
            :y (apply max (map :y coordinates))}})

(defn on-bounding-box?
  "Whether the point is on the bound box."
  [{:keys [origin corner]} {:keys [x y]}]
  (or (#{(:x origin) (:x corner)} x)
      (#{(:y origin) (:y corner)} y)))

(defn box-points
  "A seq of the points in the box."
  [{:keys [origin corner]}]
  (for [x (range (:x origin) (inc (:x corner)))
        y (range (:y origin) (inc (:y corner)))]
    {:x x :y y}))

(defn distance-map
  [distance box coordinates]
  (->> (box-points box)
       (reduce (fn [m p]
                 (update m (closest distance coordinates p) conj p))
               {})))

;; Part One

(defn largest-area
  [distance coordinates]
  (let [box (bounding-box coordinates)]
    (->> (distance-map manhattan-distance box coordinates)
         (remove #(some (partial on-bounding-box? box) (val %)))
         (remove #(nil? (first %)))
         (map #(count (second %)))
         (apply max))))

(def test-coordinates
  (mapv parse-coordinates
        ["1, 1" "1, 6" "8, 3" "3, 4" "5, 5" "8, 9"]))

(assert (= 17
           (largest-area manhattan-distance
                         test-coordinates)))

(time
 (largest-area manhattan-distance (coordinates)))

;; Part Two

(defn region
  [distance coordinates max-distance]
  (->> coordinates
       bounding-box
       box-points
       (filter #(< (reduce + (map (partial distance %) coordinates))
                   max-distance))
       count))

(assert (= 16
           (region manhattan-distance test-coordinates 32)))

(time
 (region manhattan-distance (coordinates) 10000))
