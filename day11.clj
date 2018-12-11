(ns day11)

(set! *unchecked-math* :warn-on-boxed)

(defn power-level
  [^long serial-number ^long x ^long y]
  (let [rack-id (+ x 10)]
    (- (int (mod (/ (* rack-id
                       (+ (* rack-id
                             y)
                          serial-number))
                    100)
                 10))
       5)))

(assert (=  4 (power-level  8   3   5)))
(assert (= -5 (power-level 57 122  79)))
(assert (=  0 (power-level 39 217 196)))
(assert (=  4 (power-level 71 101 153)))

(defn grid
  [serial-number]
  (reduce (fn [grid [x y]]
            (assoc-in grid [x y] (power-level serial-number x y)))
          {}
          (for [x (range 300)
                y (range 300)]
            [x y])))

(defn sums
  "The summed-area table for the grid."
  [grid]
  (reduce (fn [sums [^long x ^long y]]
            (assoc-in sums
                      [x y]
                      (+ ^long (get-in grid [x y])
                         ^long (get-in sums [(dec x) y] 0)
                         ^long (get-in sums [x (dec y)] 0)
                         (- ^long (get-in sums [(dec x) (dec y)] 0)))))
          {}
          (for [y (range (count grid))
                x (range (count grid))]
            [x y])))

(defn square-sum
  "The sum of the given square."
  [sums [^long x ^long y ^long size]]
  (let [i (dec size)]
    (+ ^long (get-in sums [(+ x i) (+ y i)])
       ^long (get-in sums [(dec x) (dec y)] 0)
       (- ^long (get-in sums [(+ x i) (dec y)] 0))
       (- ^long (get-in sums [(dec x) (+ y i)] 0)))))

(defn squares
  "All squares in the area of the given."
  [^long size ^long n]
  (for [x (range (- size n))
        y (range (- size n))]
    [x y n]))

;; Part One

(defn largest-of-size
  [serial-number n]
  (apply max-key
         (partial square-sum (sums (grid serial-number)))
         (squares 300 n)))

(assert (= [33 45 3] (largest-of-size 18 3)))

(time (largest-of-size 6303 3)) ; 400 ms

;; Part Two

(defn largest
  [serial-number]
  (apply max-key
         (partial square-sum (sums (grid serial-number)))
         (for [size (range 3 301)
               square (squares 300 size)]
           square)))

(assert (= [90 269 16] (largest 18)))
(assert (= [232 251 12] (largest 42)))

(time (largest 6303)) ; 8s
