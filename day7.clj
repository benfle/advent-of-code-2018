(ns day7
  (:require [clojure.java.io :refer [reader]]
            [clojure.set :refer [union difference subset?]]))

(defn instructions
  []
  (with-open [rdr (reader "day7.txt")]
    (->> rdr
         line-seq
         (map #(->> %
                    (re-find #"Step (.) must be finished before step (.) can begin.")
                    rest
                    (map first)))
         (reduce (fn [m [l r]]
                   (-> m
                       (update :steps (fnil conj (sorted-set)) l r)
                       (update-in [:requires r] (fnil conj #{}) l)))
                 {}))))

;; Part One

(defn available-steps
  "The ordered list of next available steps"
  [{:keys [steps requires]} visited]
  (->> visited
       (difference steps)
       (filter #(subset? (requires %) visited))))

(defn topo-sort
  "The ordered list of completed steps."
  [{:keys [steps] :as instructions}]
  (loop [visited []]
    (if (= (count steps) (count visited))
      visited
      (recur (conj visited
                   (->> (set visited)
                        (available-steps instructions)
                        first))))))

(def test-instructions
  {:steps (sorted-set \A \B \C \D \E \F)
   :requires {\A #{\C}
              \F #{\C}
              \B #{\A}
              \D #{\A}
              \E #{\B \D \F}}})

(assert (= "CABDFE"
           (apply str (topo-sort test-instructions))))

(time (apply str (topo-sort (instructions))))

;; Part Two

(defn finish-steps
  "Remove the finished steps from the workers and add it to
  the seq of visited steps."
  [{:keys [t workers] :as instructions}]
  (reduce (fn [instructions [worker [step done-at]]]
            (if (and step (= done-at t))
              (-> instructions
                  (update :visited conj step)
                  (assoc-in [:workers worker] [nil nil]))
              instructions))
          instructions
          workers))

(defn worker-available-steps
  "The set of available steps for the workers."
  [{:keys [visited workers] :as instructions}]
  (difference (set (available-steps instructions (set visited)))
              (->> (vals workers)
                   (map first)
                   (remove nil?)
                   set)))

(defn start-steps
  "Pick the next available step if a worker is idle and
  a next step is available."
  [{:keys [t workers duration] :as instructions}]
  (reduce (fn [{:keys [available-steps] :as instructions} [worker [step]]]
            (if step
              instructions
              (if-let [step (first (worker-available-steps instructions))]
                (assoc-in instructions
                          [:workers worker]
                          [step (+ t (duration step))])
                instructions)))
          instructions
          workers))

(defn do-one-cycle
  "Run one full cycle."
  [instructions]
  (-> instructions
      (update :t (fnil inc -1))
      finish-steps
      start-steps))

(defn idle?
  "Whether all workers are idle, indicating we're done."
  [{:keys [workers]}]
  (every? (fn [[_ [step]]] (nil? step)) workers))

(defn run
  "Allocate steps to n workers as they become available."
  [instructions n-workers duration]
  (loop [state (merge instructions
                      {:duration duration
                       :workers (zipmap (range n-workers)
                                        (repeat n-workers [nil nil]))
                       :visited []})]
    (let [next-state (do-one-cycle state)]
      (if (idle? next-state)
        next-state
        (recur next-state)))))

(assert (= 15
           (:t (run test-instructions 2 #(- (int %) 64)))))

(time (:t (run (instructions) 5 #(- (int %) 4))))
