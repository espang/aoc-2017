(ns aoc.day5
  (:require
   [clojure.string :as str]))

(defn follow
  ([instructions]
   (follow 0 0 instructions))
  ([pos steps instructions]
   (if (or (neg? pos)
           (>= pos (count instructions)))
     steps
     (let [v (nth instructions pos)]
       (recur (+ pos v)
              (inc steps)
              (assoc instructions
                     pos
                     (inc v)))))))

(->> (slurp "./src/aoc/input_5.txt")
     str/split-lines
     (mapv #(Integer/parseInt %))
     follow)

(defn follow-p2
  ([instructions]
   (follow-p2 0 0 instructions))
  ([pos steps instructions]
   (if (or (neg? pos)
           (>= pos (count instructions)))
     steps
     (let [v (nth instructions pos)]
       (recur (+ pos v)
              (inc steps)
              (assoc instructions
                     pos
                     (if (>= v 3)
                      (dec v)
                      (inc v))))))))
