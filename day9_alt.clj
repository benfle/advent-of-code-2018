(ns day9-alt)

;; a poor man ring zipper

(defrecord Node [l v r])

(defn make-node [l v r] (volatile! (Node. l v r)))

(defn insert
  [loc v]
  (let [{:keys [l r]} @loc
        new-loc (make-node l v loc)]
    (vswap! l assoc :r new-loc)
    (vswap! loc assoc :l new-loc)
    new-loc))

(defn remove
  [loc]
  (let [{:keys [l r]} @loc]
    (vswap! l assoc :r r)
    (vswap! r assoc :l l)
    r))

(defn move  [loc dir n] (if (zero? n) loc (move (dir @loc) dir (dec n))))
(defn left  [loc n] (move loc :l n))
(defn right [loc n] (move loc :r n))

(defn make-ring-zipper
  [v]
  (let [loc (make-node nil v nil)]
    (vswap! loc assoc :l loc)
    (vswap! loc assoc :r loc)
    loc))

;; Game

(defn play-marble
  [{:keys [board] :as game} player marble]
  (if-not (zero? (mod marble 23))
    (assoc game :board (insert (right board 2) marble))
    (let [board (left board 7)]
      (-> game
          (update-in [:scores player] (fnil + 0) marble (:v @board))
          (assoc :board (remove board))))))

(defn play
  [{:keys [players marbles]}]
  (loop [players (cycle (range 1 (inc players)))
         marbles (range 1 (inc marbles))
         game {:board (make-ring-zipper 0)
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

(time (high-score {:players 459 :marbles 71790})) ; 50 msecs

;; Part Two

(time (high-score {:players 459 :marbles 7179000})) ; 7 s
