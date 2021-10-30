(ns aoc.day12
  (:require
   [clojure.string :as str]))


(def t-input
  "0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5")

(defn parse-line [l]
  (let [[v s] (str/split l #"<->")
        vs  (str/split s #", ")]
    [x (Integer/parseInt c) (when s
                              (str/split (str/trim s) #", "))]))
