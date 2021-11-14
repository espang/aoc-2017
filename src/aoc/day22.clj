(ns aoc.day22
  (:require
   [clojure.string :as str]))

(def turn-left
  {:left :down
   :down :right
   :right :up
   :up :left})

(def turn-right
  {:left :up
   :up :right
   :right :down
   :down :left})

(def reverse
  {:up :down
   :down :up
   :left :right
   :right :left})

(def moves
  {:left  [-1 0]
   :right [1 0]
   :up    [0 -1]
   :down  [0 1]})

(defn turn [{:keys [dir] :as carrier} turn-dir]
  (assoc carrier :dir (case turn-dir
                        :left (turn-left dir)
                        :right (turn-right dir)
                        :reverse (reverse dir))))

(defn move [{:keys [pos dir] :as carrier}]
  (assoc carrier :pos (mapv + pos (moves dir))))

(defn burst [[{:keys [pos] :as carrier} infected-nodes]]
  (let [is-infected (contains? infected-nodes pos)
        turn-dir    (if is-infected :right :left)]
    (let [carrier' (turn carrier turn-dir)
          carrier' (move carrier')
          nodes'   (if is-infected
                     (disj infected-nodes pos)
                     (conj infected-nodes pos))]
      [is-infected carrier' nodes'])))

(def t-nodes
  #{[-1 0]
    [1 -1]})

(def t-carrier
  {:pos [0 0]
   :dir :up})

(defn print-grid [[x1 y1] [x2 y2] nodes]
  (doseq [y (range y1 y2)
          x (range x1 x2)]
    (if (= x x1)
      (print "\n" (if (contains? nodes [x y])
                  "#"
                  "."))
      (print (if (contains? nodes [x y])
               "#"
               ".")))))

(defn burst-n [n carrier infected-nodes]
  (loop [i     0
         caused-infections 0
         c     carrier
         nodes infected-nodes]
    (if (= i n)
      [caused-infections c nodes]
      (let [[was-infected c' nodes'] (burst [c nodes])]
        (recur (inc i)
               (if was-infected
                 caused-infections
                 (inc caused-infections))
               c'
               nodes')))))

(comment
  (def ret (burst-n 70 t-carrier t-nodes))
  (print-grid [-5 -5] [5 5] (nth ret 2))
  (first ret)
  (def ret2 (burst-n 10000 t-carrier t-nodes))
  (first ret2))

(def raw-nodes
  (->> (slurp "./src/aoc/input_22.txt")
       str/split-lines
       (mapv seq)))

(def t-raw-nodes
  (->> "..#
#..
..."
       str/split-lines
       (mapv seq)))

(defn nodes [raw]
  (->> raw
       (map-indexed (fn [y l] (map-indexed (fn [x v] [x y v]) l)))
       (apply concat)
       (filter (fn [[_ _ v]] (= v \#)))
       (map (fn [[x y _]] [x y]))
       (into #{})))

(comment
  (def ret (burst-n 10000 {:pos [12 12] :dir :up} (nodes raw-nodes)))
  (first ret))


(defn nodes2 [nodes]
  (into {} (map (fn [k] [k :infected]) nodes)))

(defn burst2 [[{:keys [pos] :as carrier} nodes]]
  (let [node-state  (get nodes pos :clean)
        carrier'    (case node-state
                      :clean (turn carrier :left)
                      :weakend carrier
                      :infected (turn carrier :right)
                      :flagged (turn carrier :reverse))
        carrier'    (move carrier')
        node-state' (case node-state
                      :clean :weakend
                      :weakend :infected
                      :infected :flagged
                      :flagged :clean)
        nodes'      (assoc nodes pos node-state')]
      [node-state' carrier' nodes']))

(defn burst2-n [n carrier nodes]
  (loop [i     0
         caused-infections 0
         c     carrier
         nodes nodes]
    (if (= i n)
      [caused-infections c nodes]
      (let [[was-infected c' nodes'] (burst [c nodes])]
        (recur (inc i)
               (if was-infected
                 caused-infections
                 (inc caused-infections))
               c'
               nodes')))))