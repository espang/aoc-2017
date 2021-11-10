(ns aoc.day16
  (:require
   [clojure.string :as str]))

(defn parse [cmd]
  (case (first cmd)
    \s [:spin
        (-> (re-matches #"s(\d*)" cmd)
            second
            Integer/parseInt)]
    \x [:exchange
        (->> (re-matches #"x(\d*)/(\d*)" cmd)
             (drop 1)
             (map (fn [x] (Integer/parseInt x))))]
    \p [:partner
        (->> (re-matches #"p([a-p])/([a-p])" cmd)
             (drop 1)
             (map first))]))

(def instructions
  (->> (str/split (slurp "./src/aoc/input_16.txt") #",")
       (map parse)))

(defmulti exec2 (fn [cmd _] (first cmd)))
(defmethod exec2 :spin [[_ n] coll] (vec (take (count coll)
                                               (drop (- (count coll) n) (cycle coll)))))
(defmethod exec2 :exchange [[_ [a b]] coll]
  (-> coll
      (assoc b (nth coll a))
      (assoc a (nth coll b))))
(defmethod exec2 :partner [[_ [a b]] coll]
  (let [name->idx (into {} (map-indexed (fn [i v] [v i]) coll))
        idx-a (name->idx a)
        idx-b (name->idx b)]
    (-> coll
        (assoc idx-b (nth coll idx-a))
        (assoc idx-a (nth coll idx-b)))))

(defn solve [cmds coll]
  (loop [cmds cmds
         coll coll]
    (if (seq cmds)
      (recur (rest cmds) (exec2 (first cmds) coll))
      coll)))

(solve [[:spin 1] [:exchange [3 4]] [:partner [\e \b]]] (vec "abcde"))
(solve instructions (vec "abcdefghijklmnop"))


(defn find-period
  "Applying the instructions on the same input will result in the same output.
  This function will find the number of times the instructions have to applied
  until the output is the start."
  [instructions start]
  (let [s (vec start)]
    (loop [n   0
           cur s]
      (let [cur' (solve instructions cur)]
        (if (= cur' s)
          (inc n)
          (recur (inc n) cur'))))))

(find-period [[:spin 1] [:exchange [3 4]] [:partner [\e \b]]] (vec "abcde"))
(find-period instructions "abcdefghijklmnop")

(defn repeat
  "repeat returns the output of applying the instructions n times to
  the start."
  [n instructions start]
  (let [period (find-period instructions start)
        n'     (mod n period)]
    (loop [n n'
           s (vec start)]
      (if (zero? n)
        s
        (recur (dec n)
               (solve instructions s))))))

(apply str (repeat 1000000000 instructions "abcdefghijklmnop"))