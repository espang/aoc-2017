(ns aoc.day12
  (:require
   [clojure.string :as str]
   [clojure.set :as set]))


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
    [(Integer/parseInt (str/trim v))
     (map #(Integer/parseInt (str/trim %)) vs)]))

(defn to-graph [coll]
  (reduce (fn [adjacent-list [node nodes]]
            (-> (reduce (fn [acc n]
                          (update acc n (fnil conj #{}) node))
                        adjacent-list
                        nodes)
                (assoc node (apply conj #{} nodes))))
          {}
          coll))

(defn connected-nodes [start graph]
  (loop [nodes #{}
         work  (cons start nil)]
    (if (seq work)
      (let [current (first work)
            neighbours (set/difference (graph current) nodes)]
        (recur (conj nodes current)
               (into (rest work) neighbours)))
      nodes)))

(->> (slurp "./src/aoc/input_12.txt")
     str/split-lines
     (map parse-line)
     to-graph
     (connected-nodes 0))

(defn groups [graph]
  (loop [nodes  (set (keys graph))
         groups '()]
    (if (seq nodes)
      (let [node  (first nodes)
            group (connected-nodes node graph)]
        (recur (set/difference nodes group)
               (cons group groups)))
      groups)))

(->> (slurp "./src/aoc/input_12.txt")
     str/split-lines
     (map parse-line)
     to-graph
     groups
     count)
