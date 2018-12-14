(ns day14)

(set! *unchecked-math* :warn-on-boxed)

(defn next-scores
  [[^long r1 ^long r2]]
  (let [sum (+ r1 r2)
        tens (mod (int (/ sum 10)) 10)
        ones (mod sum 10)]
    (if (zero? ^long tens)
      [ones]
      [tens ones])))

(defn scoreboard
  []
  {:scores [3 7]
   :elves [0 1]})

(defn make-new-recipes
  [{:keys [scores elves]}]
  (let [current-scores (map scores elves)
        new-scores (apply conj scores (next-scores current-scores))]
    {:scores new-scores
     :elves (mapv #(mod (+ ^long % (inc ^long (scores %)))
                        (count new-scores))
                  elves)}))

(defn scores-after
  [scoreboard ^long m ^long n]
  (loop [scoreboard scoreboard]
    (if (>= (count (:scores scoreboard)) (+ m n))
      (subvec (:scores scoreboard) m (+ m n))
      (recur (make-new-recipes scoreboard)))))

;; Part One

(assert (= [5 1 5 8 9 1 6 7 7 9] (scores-after (scoreboard)    9 10)))
(assert (= [0 1 2 4 5 1 5 8 9 1] (scores-after (scoreboard)    5 10)))
(assert (= [9 2 5 1 0 7 1 0 8 5] (scores-after (scoreboard)   18 10)))
(assert (= [5 9 4 1 4 2 9 8 8 2] (scores-after (scoreboard) 2018 10)))

(def scores (time (scores-after (scoreboard) 990941 10)))

;; Part Two

(defn recipes-required-for
  [scoreboard scores-to-find]
  (let [c (count scores-to-find)]
    (loop [{:keys [scores] :as scoreboard} scoreboard]
      (or (and (< c (count scores))
               (->> [c (inc c)]
                    (map #(- (count scores) ^long %))
                    (some #(when (= (subvec scores % (+ ^long % c)) scores-to-find)
                             %))))
          (recur (make-new-recipes scoreboard))))))

(assert (=    9 (recipes-required-for (scoreboard) [5 1 5 8 9])))
(assert (=    5 (recipes-required-for (scoreboard) [0 1 2 4 5])))
(assert (=   18 (recipes-required-for (scoreboard) [9 2 5 1 0])))
(assert (= 2018 (recipes-required-for (scoreboard) [5 9 4 1 4])))

(time (recipes-required-for (scoreboard) [9 9 0 9 4 1])) ;; 34 s :(
