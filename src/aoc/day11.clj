(ns aoc.day11
  (:require
   [clojure.string :as str]))

(defn move
  "move on a hex grid"
  [[x y] dir]
  (case dir
    "n"  [x (- y 2)]
    "s"  [x (+ y 2)]
    "nw" [(dec x) (dec y)]
    "sw" [(dec x) (inc y)]
    "ne" [(inc x) (dec y)]
    "se" [(inc x) (inc y)]))

(defn move-all
  "takes a seq of directions and returns the end position
   when following those moves starting at [0 0]"
  [dirs]
  (reduce move
          [0 0]
          dirs))

(defn move-all2
  "same as move-all, but keeps track of all visisted coordinates.
  Returns [endposition all-positions]."
  [dirs]
  (reduce (fn [[pos poss] dir]
            (let [pos' (move pos dir)]
              [pos'
               (conj poss pos')]))
          [[0 0] #{}]
          dirs))

(def dirs ["n" "s" "ne" "nw" "se" "sw"])

(defn shortest-path-to
  "BFS to find the shortest path towards target.
   The BFS implementation took a few seconds for part1
   and was too slow for part2."
  [start target]
  (loop [steps (conj (clojure.lang.PersistentQueue/EMPTY) [start 0])
         seen  #{}]
    (if (empty? steps)
      -1
      (let [[element n] (first steps)]
        (if (= element target)
          n
          (recur (if (contains? seen element)
                   (pop steps)
                   (apply conj (pop steps) (map #(vector (move element %) (inc n)) dirs)))
                 (conj seen element)))))))

(defn shortest-path-to2
  "Just calculate the shortest path with the given
   coordinates."
  [[xs ys] [xt yt]]
  (let [adjust-x (Math/abs (- xs xt))
        adjust-y (Math/abs (- ys yt))]
    (if (> adjust-y adjust-x)
      (+ adjust-x (/ (- adjust-y adjust-x) 2))
      (if (= adjust-x adjust-y)
        adjust-x
        (+ adjust-x 1)))))

(time (-> (str/split (slurp "./src/aoc/input_11.txt") #",")
    move-all
    (shortest-path-to2 [0 0])))

(time (->> (str/split (slurp "./src/aoc/input_11.txt") #",")
           move-all2
           second
           (map #(shortest-path-to2 % [0 0]))
           (apply max)))