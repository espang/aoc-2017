(ns aoc.day14
  (:require
   [aoc.day10 :refer [solve-p2]]
   [clojure.pprint :as pp]
   [clojure.set :as set]))

(def input "jxqlasbh")
(def t-input "flqrgnkx")

(defn row [i input]
  (solve-p2 (str input "-" i)))

(defn hex->bit [h]
  (let [n (if (char? h)
            (int h) h)]
    (pp/cl-format
     nil
     "~4,'0',B"
     (cond
       (<= 48 n 57)  (- 9 (- 57 n))
       (<= 97 n 102) (- 15 (- 102 n))
       :else 0))))

(defn count-1 [s]
  (count (filter #(= \1 %) s)))

(->> (range 0 128)
     (map #(row % input))
     (map #(apply str (map hex->bit %)))
     (map count-1)
     (reduce +))

(def grid
  (->> (range 0 128)
       (map #(row % input))
       (mapv #(vec (mapcat hex->bit %)))))

(defn neighbours [[x y]]
  [[(inc x) y]
   [(dec x) y]
   [x (inc y)]
   [x (dec y)]])

(defn used? [grid x y]
  (= \1 (get-in grid [x y])))

(defn expand [grid [x y]]
  (if (used? grid x y)
    (loop [to-visit (conj (clojure.lang.PersistentQueue/EMPTY) [x y])
           seen     #{}]
      (if (seq to-visit)
        (if (contains? seen (peek to-visit))
          (recur (pop to-visit) seen)
          (recur (apply conj
                        (pop to-visit)
                        (filter
                         (fn [[x' y']]
                           (used? grid x' y'))
                         (neighbours (peek to-visit))))
                 (conj seen (peek to-visit))))
        seen))
    #{}))

(defn regions [grid]
  (loop [n 0
         ones (->> (for [x (range 0 128)
                         y (range 0 128)] [x y])
                   (filter #(= \1 (get-in grid %))))
         regions #{}]
    (if (empty? ones)
      n
      (if (contains? regions (first ones))
        (recur n (rest ones) regions)
        (recur (inc n)
               (rest ones)
               (set/union regions
                          (expand grid (first ones))))))))

(regions grid)
