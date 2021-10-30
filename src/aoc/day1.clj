(ns aoc.day1)

(def input (slurp "./src/aoc/input_1.txt"))

(defn char->int [c]
  (- (int c) 48))

(defn summandOf [c1 c2]
  (if (= c1 c2)
    (char->int c1)
    0))

(defn count-repeated [chars]
  (let [first-element (first chars)]
    (loop [elements chars
           last     first-element
           sum      0]
      (if (seq (rest elements))
        (recur (rest elements)
               (first elements)
               (+ sum (summandOf last (first elements))))
        (+ sum (summandOf first-element (first elements)))))))

; part 1
(count-repeated input)

; part 2
(defn count-halfway-around [s]
  (let [lookup     (into [] s)
        n          (count lookup)
        delta      (/ n 2)
        next-index (fn [i] (mod (+ i delta) n))
        next-value (fn [i] (nth lookup (next-index i)))]
    (->> s
         (map-indexed #(vector %1 %2))
         (reduce (fn [acc [i v]]
                   (+ acc
                      (summandOf v (next-value i))))
                 0))))

(count-halfway-around input)