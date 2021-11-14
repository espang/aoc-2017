(ns aoc.day20
  (:require
   [clojure.string :as str]))

(defn parse-line [l]
  (->> (re-matches #"p=<([-]?\d*),([-]?\d*),([-]?\d*)>, v=<([-]?\d*),([-]?\d*),([-]?\d*)>, a=<([-]?\d*),([-]?\d*),([-]?\d*)>" l)
       (drop 1)
       (map (fn [v] (Integer/parseInt v)))
       (partition 3)))

(comment
  (parse-line "p=<-1734,-1066,2148>, v=<-249,-149,306>, a=<20,10,-23>"))

(def t-system
  [[[3 0 0] [2 0 0] [-1 0 0]]
   [[4 0 0] [0 0 0] [-2 0 0]]])

(def system
  (->> (slurp "./src/aoc/input_20.txt")
       str/split-lines
       (map parse-line)))

(defn step-particle
  [[pos v a]]
  (let [v'   (mapv + v a)
        pos' (mapv + pos v')]
    [pos' v' a]))

(defn step-system [system]
  (mapv step-particle system))

(defn distance [xyz]
  (reduce + (map #(Math/abs %) xyz)))

(defn do-n-steps
  ([n system]
   (do-n-steps n system step-system))
  ([n system step-fn]
   (loop [s system
          c 0]
     (if (= c n)
       s
       (recur (step-fn s)
              (inc c))))))

(defn closet-to-center [system]
  (->> (map-indexed vector system)
       (apply min-key (fn [[i [p _v _a]]] (distance p)))))

(comment
  (-> (do-n-steps 100 t-system) closet-to-center)
  (-> (do-n-steps 100 system) closet-to-center)
  (-> (do-n-steps 1000 system) closet-to-center))

(defn freqs [system]
  (-> (map first system)
      frequencies))

(defn remove-collisons [system]
  (let [fr (freqs system)]
    (filter (fn [[p _v _a]] (= 1 (fr p))) system)))

(defn step-system-2 [system]
  (-> (mapv step-particle system)
      remove-collisons))

(comment
  (count (do-n-steps 1 system step-system-2))
  (count (do-n-steps 1000 system step-system-2))
  (count (do-n-steps 10000 system step-system-2)))
