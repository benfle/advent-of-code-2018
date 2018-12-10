(ns day10.clj
  (:require [clojure.string :as str]))

;; input format: "position=<px,  py> velocity=< vx, vy>

(defn parse-vector
  [s]
  (->> (str/split s #",")
       (map #(Integer/parseInt (str/trim %)))
       (zipmap [:x :y])))

(defn parse-point
  [s]
  (let [[_ position _ velocity] (str/split s #"[<>]")]
    {:position (parse-vector position)
     :velocity (parse-vector velocity)}))

(defn next-point
  [{:keys [velocity] :as point}]
  (update point :position #(merge-with + % velocity)))

(defn positions
  [point]
  (iterate next-point point))

(defn bounding-box
  [points]
  (let [positions (map :position points)]
    {:origin {:x (apply min (map :x positions))
              :y (apply min (map :y positions))}
     :corner {:x (apply max (map :x positions))
              :y (apply max (map :y positions))}}))

(defn area
  [{:keys [origin corner]}]
  (* (- (:y corner) (:y origin))
     (- (:x corner) (:x origin))))

(defn first-min-key
  [k coll]
  (loop [[y & more] (rest coll)
         x (first coll)
         kx (k (first coll))]
    (let [ky (k y)]
      (if (< kx ky)
        x
        (if more (recur more y ky) y)))))

(defn display-configuration
  [configuration]
  (let [{:keys [origin corner]} (bounding-box configuration)]
    (println "")
    (println (str/join "\n"
                       (for [y (range (:y origin) (inc (:y corner)))]
                         (apply str
                                (for [x (range (:x origin) (inc (:x corner)))]
                                  (if (some #(and (= x (get-in % [:position :x]))
                                                  (= y (get-in % [:position :y])))
                                            configuration)
                                    \X
                                    \.))))))))

(def configurations
  (->> (slurp "day10.txt")
       str/split-lines
       (map parse-point)
       (iterate #(map next-point %))))

;; Part One

(->> configurations
     (first-min-key #(area (bounding-box %)))
     display-configuration)

;; Part Two

(->> configurations
     (map-indexed vector)
     (first-min-key #(area (bounding-box (second %))))
     first)
