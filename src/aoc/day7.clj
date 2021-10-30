(ns aoc.day7
  (:require
   [clojure.string :as str]
   [clojure.zip :as zip]))

(def t-input
  "pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)")

(defn parse-line [l]
  (let [[f s] (str/split l #"->")
        [_ x c](re-matches #"(\w*) \((\d*)\).*" f)]
    [x (Integer/parseInt c) (when s
           (str/split (str/trim s) #", "))]))

(defn solve [input]
  (let [{:keys [nodes children]}
        (reduce (fn [acc [f _ vs]]
                  (-> acc
                      (update-in [:nodes] (fnil conj #{}) f)
                      (update-in [:children] (fnil into #{}) vs)))
                {}
                (->> input
                     str/split-lines
                     (map parse-line)))]
    (clojure.set/difference nodes children)))

(solve t-input)
(solve (slurp "./src/aoc/input_7.txt"))

(defn build-tree
  "given nodes of a map from label to [weight children]
   and a starting label.

   Build a tree with elements
   [label totalweight elementweight child1 ... childn]
   whereas each child node is a tree element itself."
  [nodes start]
  (let [[n vs]   (nodes start)
        children (mapv #(build-tree nodes %) vs)
        weight   (+ n
                    (->> children
                         (map second)
                         (reduce +)))]
    (into [start weight n] children)))

(defn solve-p2
  [input]
  (let [;; find the root of the tree
        root  (first (solve input))
        ;; collect all nodes from the input
        nodes (->> input
                   str/split-lines
                   (map parse-line)
                   (reduce (fn [acc [f n vs]]
                             (assoc acc f [n vs]))
                           {}))
        ;; build the tree
        tree  (build-tree nodes root)]
    tree))

(solve-p2 t-input)
(solve-p2 (slurp "./src/aoc/input_7.txt"))
