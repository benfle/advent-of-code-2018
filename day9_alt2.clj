(ns day9-alt2
  (:import [java.util LinkedList Deque HashMap]))

(set! *warn-on-reflection* true)

;; Game

(defn rotate [^Deque board n]
  (if (< n 0)
    (loop [n n]
      (if (zero? n)
        board
        (do
          (.addFirst board (.removeLast board))
          (recur (inc n)))))
    (loop [n n]
      (if (zero? n)
        board
        (do
          (.addLast board (.removeFirst board)) board
          (recur (dec n)))))))

(defn play
  [{:keys [players marbles]}]
  (let [board (LinkedList. [0])
        scores (long-array players)]
    (doseq [marble (range 1 (inc marbles))
            :let [player (mod marble players)]]
      (if (zero? (mod marble 23))
        (do
          (rotate board -7)
          (aset scores player ^long (+ (aget scores player) marble (.removeFirst board))))
        (do
          (rotate board 2)
          (.addFirst board marble))))
    (apply max (seq scores))))

;; Part One

(time (assert (=     32 (play {:players  9 :marbles   25}))))
(time (assert (=   8317 (play {:players 10 :marbles 1618}))))
(time (assert (= 146373 (play {:players 13 :marbles 7999}))))
(time (assert (=   2764 (play {:players 17 :marbles 1104}))))
(time (assert (=  54718 (play {:players 21 :marbles 6111}))))
(time (assert (=  37305 (play {:players 30 :marbles 5807}))))

(time (play {:players 459 :marbles 71790})) ; 4 ms

;; Part Two

(time (play {:players 459 :marbles 7179000})) ; (vary between 300 ms and 1.5 s)
