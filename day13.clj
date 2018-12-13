(ns day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def cart? #{\^ \v \> \<})

(defn system
  []
  (with-open [rdr (io/reader "day13.txt")]
    (let [tracks (->> (line-seq rdr)
                      (map-indexed #(->> (seq %2)
                                         (map-indexed vector)
                                         (into (sorted-map))
                                         (vector %1)))
                      (into (sorted-map)))]
      (reduce (fn [system [y line]]
                (reduce (fn [system [x track]]
                          (if-not (cart? track)
                            system
                            (-> system
                                (assoc-in [:tracks y x] (case track
                                                          (\v \^) \|
                                                          (\> \<) \-))
                                (update :carts assoc [y x] {:dir track
                                                            :turns (cycle [:left :straight :right])}))))
                        system
                        line))
              {:tracks tracks
               :carts (sorted-map)
               :collisions #{}}
              tracks))))

(def directions
  {\^ {:left \< :right \> :straight \^}
   \v {:left \> :right \< :straight \v}
   \< {:left \v :right \^ :straight \<}
   \> {:left \^ :right \v :straight \>}})

(defn move-cart
  [{:keys [carts tracks] :as system} [[y0 x0] {:keys [dir turns]}]]
  (if-not (contains? carts [y0 x0])
    ;; The cart might have been in a collision during this tick.
    system
    (let [[y1 x1] (case dir
                    \^ [(dec y0)     x0]
                    \v [(inc y0)     x0]
                    \> [     y0 (inc x0)]
                    \< [     y0 (dec x0)])]
      (if (contains? carts [y1 x1])
        ;; collision
        (-> system
            (update :carts dissoc [y0 x0] [y1 x1])
            (update :collisions conj [y1 x1]))
        ;; move cart to next track
        (update system
                :carts
                #(-> %
                     (dissoc [y0 x0])
                     (assoc [y1 x1] {:dir (case (get-in tracks [y1 x1])
                                            (\| \-) dir
                                            \+     (get-in directions [dir (first turns)])
                                            \\     (case dir
                                                     (\^ \v) (get-in directions [dir :left])
                                                     (\< \>) (get-in directions [dir :right]))
                                            \/     (case dir
                                                     (\^ \v) (get-in directions [dir :right])
                                                     (\< \>) (get-in directions [dir :left])))
                                     :turns (if (= \+ (get-in tracks [y1 x1]))
                                              (rest turns)
                                              turns)})))))))

(defn tick
  [system]
  (reduce move-cart system (:carts system)))

;; Part One

(defn run-until-first-collision
  [system]
  (loop [system system]
    (if (seq (:collisions system))
      system
      (recur (tick system)))))

(reverse (first (:collisions (run-until-first-collision (system)))))

;; Part Two

(defn run-until-one-cart-left
  [system]
  (loop [system system]
    (if (= 1 (count (:carts system)))
      system
      (recur (tick system)))))

(reverse (ffirst (:carts (run-until-one-cart-left (system)))))
