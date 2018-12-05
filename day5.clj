(ns day5)

(def polymer (butlast (slurp "day5.txt")))

;; Part One

(defn reduce-start
  "Reduce the start of the polymer until it cannot."
  [polymer]
  (loop [[first second & rest :as polymer] polymer]
    (when first
      (if (and second (= 32 (Math/abs (- (int second) (int first)))))
        (recur rest)
        polymer))))

(defn reduce-polymer
  "Reduce the polymer."
  [polymer]
  (loop [polymer polymer
         reduced '()]
    (if-let [start-reduced (reduce-start polymer)]
      (if (or (empty? reduced)
              (= start-reduced polymer))
        ;; we did not reduce the polymer or we're just starting
        ;; => continue to the next character
        (recur (rest start-reduced)
               (conj reduced (first start-reduced)))
        ;; we reduced the polymer
        ;; => backtrack of 1 character in case the reduction cascades
        (recur (conj start-reduced (first reduced))
               (rest reduced)))
      (apply str (reverse reduced)))))

(assert (= "dabCBAcaDA"
           (reduce-polymer (seq "dabAcCaCBAcCcaDA"))))

(count polymer)

(time (count (reduce-polymer polymer)))

;; Part Two

(def unit
  (->> (range 65 91)
       (map #(set [(char %) (char (+ % 32))]))
       (apply min-key (fn [unit]
                        (->> polymer
                             (remove unit)
                             reduce-polymer
                             count)))
       time))

(count (reduce-polymer (remove unit polymer)))
