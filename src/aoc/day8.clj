(ns aoc.day8
  (:require
   [clojure.string :as str]))

(def t-input
  "b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10")

(defn line->instruction [line]
  (let [[_ target f v condition-target operator v2]
        (re-matches #"(\w*) (inc|dec) ([\-0-9]*) if (\w*) (<|<=|>=|>|==|!=) ([\-0-9]*)" line)
        v  (Integer/parseInt v)
        v2 (Integer/parseInt v2)
        mut-fn (case f
                 "inc" +
                 "dec" -)
        pred-fn (case operator
                  ">" >
                  "<" <
                  ">=" >=
                  "<=" <=
                  "==" =
                  "!=" not=)]
    [target
     (fn [x] (mut-fn x v))
     (fn [reg] (pred-fn (reg condition-target 0) v2))]))

(comment
  (line->instruction "b inc 5 if a > 1"))

(defn solve
  "solves part1&2 in one. Returns [[register-loc answer-part1] answer-part2]."
  [input]
  (let [instructions (->> input
                          str/split-lines
                          (map line->instruction))
        [m register] (reduce (fn [[m register] [target change pred]]
                               (if (pred register)
                                 (let [new-v (change (register target 0))]
                                   [(max m new-v) (assoc register target new-v)])
                                 [m register]))
                             [0 {}]
                             instructions)]
    [(apply max-key val register)
     m]))

(solve t-input)
(solve (slurp "./src/aoc/input_8.txt"))
