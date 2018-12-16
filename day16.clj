(ns day16
  (:require [clojure.java.io :refer [reader]]
            [clojure.string :as str]
            [clojure.set :refer [intersection]]))

(def opcodes
  {:addr (fn [registers a b] (+       (registers a) (registers b)))
   :addi (fn [registers a b] (+       (registers a)            b))
   :mulr (fn [registers a b] (*       (registers a) (registers b)))
   :muli (fn [registers a b] (*       (registers a)            b))
   :banr (fn [registers a b] (bit-and (registers a) (registers b)))
   :bani (fn [registers a b] (bit-and (registers a)            b))
   :borr (fn [registers a b] (bit-or  (registers a) (registers b)))
   :bori (fn [registers a b] (bit-or  (registers a)            b))
   :setr (fn [registers a b] (registers a))
   :seti (fn [registers a b]            a)
   :gtir (fn [registers a b] (if (>            a  (registers b)) 1 0))
   :gtri (fn [registers a b] (if (> (registers a)            b)  1 0))
   :gtrr (fn [registers a b] (if (> (registers a) (registers b)) 1 0))
   :eqir (fn [registers a b] (if (=            a  (registers b)) 1 0))
   :eqri (fn [registers a b] (if (= (registers a)            b)  1 0))
   :eqrr (fn [registers a b] (if (= (registers a) (registers b)) 1 0))})

(defn run-instruction
  [registers [opcode a b c]]
  (assoc registers c ((opcodes opcode) registers a b)))

;; Part One

(defn compatible?
  [registers-before registers-after instruction]
  (= registers-after
     (run-instruction registers-before instruction)))

(defn compatible-opcodes
  [registers-before registers-after [opcode a b c]]
  (->> (keys opcodes)
       (filter #(compatible? registers-before
                             registers-after
                             [% a b c]))
       set))

(assert (= #{:addi :seti :mulr}
           (compatible-opcodes [3 2 1 1] [3 2 2 1] [9 2 1 2])))

(defn parse-numbers
  [line]
  (->> line
       (re-seq #"\d+")
       (mapv #(Integer/parseInt %))))

(defn input
  []
  (with-open [rdr (reader "day16.txt")]
    (loop [lines (line-seq rdr)
           input {:samples []
                  :program []}]
      (cond

        (not (seq lines))
        input

        (.startsWith (first lines) "Before")
        (recur (drop 3 lines)
               (let [[before instruction after] (take 3 lines)]
                 (update input :samples conj {:before (parse-numbers before)
                                              :instruction (parse-numbers instruction)
                                              :after (parse-numbers after)})))

        (str/blank? (first lines))
        (recur (rest lines)
               input)

        :else
        (recur (rest lines)
               (update input :program conj (parse-numbers (first lines))))))))

(count (filter (fn [{:keys [before instruction after]}]
                 (<= 3 (count (compatible-opcodes before after instruction))))
               (:samples (input))))

;; Part Two

(def number->compatible-opcodes
  (reduce (fn [match {:keys [before instruction after]}]
            (update match
                    (first instruction)
                    (fnil intersection (set (keys opcodes)))
                    (compatible-opcodes before after instruction)))
          {}
          (:samples (input))))

(def number->opcode
  (loop [matches {}
         number->compatible-opcodes number->compatible-opcodes]
    (let [obvious-matches (filter #(= 1 (count (val %)))
                                  number->compatible-opcodes)]
      (if (seq obvious-matches)
        (recur (into matches obvious-matches)
               (->> number->compatible-opcodes
                    (remove #(contains? (set (map first obvious-matches)) (first %)))
                    (map (fn [[n opcodes]]
                           [n (remove (set (map #(first (second %)) obvious-matches)) opcodes)]))
                    (into {})))
        (into {} (map (fn [[n opcodes]]  [n (first opcodes)]) matches))))))

(reduce #(run-instruction %1 (into [(number->opcode (first %2))]
                                   (rest %2)))
        [0 0 0 0]
        (:program (input)))
