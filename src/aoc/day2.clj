(ns aoc.day2
  (:require
   [clojure.string :as str]))

(defn row->checksum [row]
  (let [xs (->> (str/split row #"\s")
                (map read-string))]
    (- (apply max xs)
       (apply min xs))))

(->> (slurp "./src/aoc/input_2.txt")
     (str/split-lines)
     (map row->checksum)
     (reduce +))

(defn row->checksum2 [row]
  (let [xs (->> (str/split row #"\s")
                (map read-string))
        f  (fn [x] (some #(and (zero? (mod x %))
                               (not= x %)
                               (/ x %)) xs))]
    (->> xs
         (map f)
         (filter some?)
         (first))))

(->> (slurp "./src/aoc/input_2.txt")
     (str/split-lines)
     (map row->checksum2)
     (reduce +))
