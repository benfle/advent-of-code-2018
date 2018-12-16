(ns day15
  (:require [clojure.pprint :refer [pprint]]
            [clojure.java.io :refer [reader]]
            [clojure.set :refer [union]]))

(defn contains-in?
  [m [k & ks]]
  (if-not (seq ks)
    (contains? m k)
    (and (contains? m k)
         (contains-in? (m k) ks))))

(defn dissoc-in
  [m [k & ks]]
  (if-not (seq ks)
    (dissoc m k)
    (if (contains? m k)
      (update m k dissoc-in ks)
      m)))

(defn game
  ([file]
   (game file 3))
  ([file elf-attack-power]
   (with-open [rdr (reader file)]
     (let [cavern (->> (line-seq rdr)
                       (map-indexed #(->> (seq %2)
                                          (map-indexed vector)
                                          (into (sorted-map))
                                          (vector %1)))
                       (into (sorted-map)))]
       (reduce (fn [game [y line]]
                 (reduce (fn [game [x char]]
                           (if-not (#{\E \G} char)
                             game
                             (-> game
                                 (assoc-in [:cavern y x] \.)
                                 (assoc-in [:units [y x]] {:type char
                                                           :attack-power (if (= \E char)
                                                                           elf-attack-power
                                                                           3)
                                                           :hit-points 200}))))
                         game
                         line))
               {:cavern cavern
                :units (sorted-map)
                :round 0}
               cavern)))))

(defn locations
  "A seq of all locations in the cavern."
  [game]
  (for [y (range (count (:cavern game)))
        x (range (count ((:cavern game) 0)))]
    [y x]))

(defn occupied?
  "Whether the location is occupied by a wall or unit, excluding
  the passed location."
  [{:keys [cavern units]} loc]
  (or (= \# (get-in cavern loc))
      (contains? units loc)))

(defn neighbors
  "The locations immediately up, down, left, and righ of this location."
  [[y x]]
  [[(dec y)     x]
   [(inc y)     x]
   [     y (dec x)]
   [     y (inc x)]])

(defn unoccupied-neighbors
  "The unoccupied locations adjacent to this location."
  ([game loc]
   (unoccupied-neighbors game loc nil))
  ([game loc exclude-loc]
   (->> (neighbors loc)
        (filter #(contains-in? (:cavern game) %))
        (filter #(or (= % exclude-loc)
                     (not (occupied? game %)))))))

(defn shortest-path
  "The shortest-path from the origin location to one of the set of target locations."
  [game origin targets]
  (loop [loc origin
         paths (sorted-map loc {:distance 0})
         unvisited (->> (locations game)
                        (filter #(or (= origin %)
                                     (not (occupied? game %))))
                        (apply sorted-set))]
    (when (seq unvisited)
      (let [neighbor-new-distance (inc (get-in paths [loc :distance]))
            paths (->> (unoccupied-neighbors game loc)
                       (filter unvisited)
                       (reduce (fn [paths neighbor]
                                 (if (< neighbor-new-distance (get-in paths [neighbor :distance] ##Inf))
                                   (do
                                     (assoc paths neighbor {:distance neighbor-new-distance
                                                            :previous loc}))
                                   paths))
                               paths))
            closest-loc (->> unvisited
                             reverse
                             (apply min-key #(get-in paths [% :distance] ##Inf)))]
        (when (paths closest-loc)
          (if (contains? targets closest-loc)
            (loop [closest-target-loc closest-loc]
              (let [previous (get-in paths [closest-target-loc :previous])]
                (if (= origin previous)
                  closest-target-loc
                  (recur previous))))
            (recur closest-loc paths (disj unvisited loc))))))))

(defn targets
  "The unit's target."
  [game unit]
  (->> (:units game)
       (filter #(not= (:type unit) (:type (val %))))
       (sort-by first)))

(defn next-loc
  "The next location to move the unit one step closer to its closest target if not already in its neighborhood."
  [game [loc unit]]
  (let [loc-targets (->> (targets game unit)
                         (map #(set (unoccupied-neighbors game (first %) loc)))
                         (apply union))]
    (if (contains? loc-targets loc)
      loc
      (or (shortest-path game loc loc-targets)
          loc))))

(defn attack
  "Make the unit attack its closest target in its neighborhood, if it exists."
  [game loc]
  (let [{:keys [attack-power] :as unit} (get (:units game) loc)
        reachable-targets (filter #(contains? (set (neighbors loc)) (first %))
                                  (targets game unit))]
    (if-not (seq reachable-targets)
      ;; nothing to attack
      game
      ;; attack unit with lowest hit points
      (let [[target-loc target] (apply min-key #(:hit-points (second %))
                                       (reverse reachable-targets))]
        (let [hit-points (- (:hit-points target) attack-power)]
          (if (<= hit-points 0)
            ;; die if not more hit points
            (dissoc-in game [:units target-loc])
            (assoc-in game [:units target-loc :hit-points] hit-points)))))))

(defn turn
  "Give a turn to the unit."
  [game loc]
  (let [unit (get (:units game) loc)]
    (if-not unit
      ;; the unit might have been killed during this turn
      game
      (if-not (seq (targets game unit))
        ;; no more targets, game is done
        (assoc game :done? true)
        ;; Otherwise move and attack
        (let [next-loc (next-loc game [loc unit])
              game (-> game
                       (dissoc-in [:units loc])
                       (assoc-in [:units next-loc] unit))]
          (attack game next-loc))))))

(defn round
  "Run a round of turns."
  [game]
  (-> (reduce (fn [game loc]
                (let [next-game (turn game loc)]
                  (if (:done? next-game)
                    (reduced next-game)
                    next-game)))
              game
              (keys (:units game)))
      (update :round inc)))

(defn display-game
  [game]
  (println "")
  (doseq [[y row] (:cavern game)]
    (doseq [[x c] row]
      (if-let [unit (get-in game [:units [y x]])]
        (print (:type unit))
        (print c)))
    (print "\n"))
  (when (:done? game)
    (println "GAME OVER")))

(defn stats
  [game]
  (let [units (vals (:units game))
        elves (filter #(= \E (:type %)) units)
        goblins (filter #(= \G (:type %)) units)
        total (reduce + (map :hit-points units))]
    {:round (:round game)
     :elves {:count (count elves)
             :hit-points (reduce + (map :hit-points elves))}
     :goblins {:count (count goblins)
               :hit-points (reduce + (map :hit-points goblins))}
     :total {:count (count units)
             :hit-points (reduce + (map :hit-points units))}
     :outcome (* (dec (:round game))
                 (reduce + (map :hit-points units)))}))

;; Part One

(defn play
  ([game]
   (play game nil))
  ([game stop-at]
   (loop [game game]
     (let [game (round game)]
       #_(display-game game)
       #_(println (stats game))
       (if (or (:done? game)
               (and stop-at (= stop-at (:round game))))
         game
         (recur game))))))

;; Part Two

(defn count-elves [game] (count (filter #(= \E (:type %)) (vals (:units game)))))

(defn play-until-all-elves-alive
  [file [lo hi]]
  (let [initial-elves-count (count-elves (game file))]
    (loop [[lo hi] [lo hi]]
      (let [elf-attack-power (int (/ (+ hi lo) 2))]
        (println [lo hi elf-attack-power])
        (let [game (play (game file elf-attack-power))]
          (if (= initial-elves-count (count-elves game))
            (if (= lo elf-attack-power)
              lo
              (recur [lo elf-attack-power]))
            (if (= (dec hi) elf-attack-power)
              hi
              (recur [elf-attack-power hi]))))))))

(comment

  (def g (game "day15.txt"))
  (def g (time (play g)))

  (play-until-all-elves-alive "day15.txt" [4 100])

  (stats (play (game "day15.txt" 20)))


  )
