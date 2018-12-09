(ns day9
  (:require [clojure.zip :as zip]))

(defn loop-left
  "The loc of the n left sibling (loop if leftmost reached)."
  [loc n]
  (loop [n n
         loc loc]
    (if (zero? n)
      loc
      (recur (dec n)
             (or (zip/left loc)
                 (zip/rightmost loc))))))

(defn loop-right
  "The loc of the n right sibling (loop if rightmost reached)."
  [loc n]
  (loop [n n
         loc loc]
    (if (zero? n)
      loc
      (recur (dec n)
             (or (zip/right loc)
                 (zip/leftmost loc))))))

(defn play-marble
  [{:keys [board] :as game} player marble]
  (if-not (zero? (mod marble 23))
    (update game :board #(-> % (loop-right 1) (zip/insert-right marble) zip/right))
    (let [board (loop-left board 7)
          board-marble (zip/node board)]
      (-> game
          (update-in [:scores player] (fnil + 0) marble board-marble)
          (assoc :board (-> board zip/remove (loop-right 1)))))))

(defn play
  [{:keys [players marbles]}]
  (loop [players (cycle (range 1 (inc players)))
         marbles (range 1 (inc marbles))
         game {:board (zip/down (zip/vector-zip [0]))
               :scores {}}]
    (if-not (seq marbles)
      game
      (recur (rest players)
             (rest marbles)
             (play-marble game
                          (first players)
                          (first marbles))))))

;; Part One

(defn high-score
  [game]
  (apply max-key val (:scores (play game))))

(time (assert (=     32 (second (high-score {:players  9 :marbles   25})))))
(time (assert (=   8317 (second (high-score {:players 10 :marbles 1618})))))
(time (assert (= 146373 (second (high-score {:players 13 :marbles 7999})))))
(time (assert (=   2764 (second (high-score {:players 17 :marbles 1104})))))
(time (assert (=  54718 (second (high-score {:players 21 :marbles 6111})))))
(time (assert (=  37305 (second (high-score {:players 30 :marbles 5807})))))

(time (high-score {:players 459 :marbles 71790})) ; 200 msecs

;; Part Two

(time (high-score {:players 459 :marbles 7179000})) ; 17 s
