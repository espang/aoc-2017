(ns aoc.day19
  (:require
   [clojure.string :as str]))

(defn- read-in [filename]
  (->> (slurp filename)
       (str/split-lines)
       (mapv vec)))

(def t-input (read-in "./src/aoc/input_19_t.txt"))

(def input (read-in "./src/aoc/input_19.txt"))

(defn to-row [r]
  (map-indexed vector r))

(defn is-letter? [c]
  (<= (int \A) (int c) (int \Z)))

(defn value [grid [x y]]
  (get-in grid [y x]))

(def up [0 -1])
(def down [0 1])
(def left [-1 0])
(def right [1 0])

(defn first-not-space [grid xy dir]
  (let [dirs (condp = dir
               left [left up down]
               right [right up down]
               down [down left right]
               up [up left right])
        ret  (filter (fn [x] (when-let [v (value grid (mapv + xy x))]
                               (not= \space v))) dirs)]
    (when (> (count ret) 1)
      (println "more than 1 choice: " xy dir ret))
    (first ret)))

(first-not-space input [181 27] right)

(defn move-through [grid]
  (let [[start _] (first (filter (fn [[_ v]]
                                   (= v \|))
                                 (to-row (first grid))))
        max-x (count (first grid))
        max-y (count grid)]
    (loop [xy    [start 0]
           dir   [0 1]
           ret   '()
           steps 1]
      (let [xy' (mapv + xy dir)
            v   (value grid xy')]
        (if-not (and (<= 0 (first xy') max-x)
                     (<= 0 (second xy') max-y))
          [(apply str (reverse ret)) steps]
          (cond
            (is-letter? v) (recur xy' dir (conj ret v) (inc steps))

            (= \+ v)
            (recur xy'
                   (first-not-space grid xy' dir)
                   ret
                   (inc steps))

            (= \space v)
            [(apply str (reverse ret)) steps]

            :else (recur xy' dir ret (inc steps))))))))

(comment
  (move-through t-input)
  (move-through input))