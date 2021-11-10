(ns aoc.day10
  (:require
   [clojure.string :as str]))

(defn solve [coll pos skip lengths]
  (if (seq lengths)
    (let [l      (first lengths)
          sub    (into [] (reverse (take l (drop pos (cycle coll)))))
          pos'   (mod (+ pos l skip) (count coll))
          skip'  (inc skip)
          coll'  (if (> (+ pos l) (count coll))
                   ;; wrap around
                   (let [wrap-idx  (- (count coll) pos)
                         first-idx (- (+ pos l) (count coll))]
                     (-> (subvec sub wrap-idx)
                         (into (subvec coll first-idx pos))
                         (into (subvec sub 0 wrap-idx))))
                   ;; simple start - changed - end
                   (-> (subvec coll 0 pos)
                       (into sub)
                       (into (subvec coll (+ pos l)))))]
      (recur coll' pos' skip' (rest lengths)))
    coll))

(comment
  (->> (solve (vec (range 5))
              0
              0
              [3 4 1 5])
       (take 2)
       (reduce * 1)))

(def input "18,1,0,161,255,137,254,252,14,95,165,33,181,168,2,188")

(->> (solve (vec (range 256))
            0
            0
            (->> (str/split input #",")
                 (map #(Integer/parseInt %))))
     (take 2)
     (reduce * 1))

;; part 2
(defn to-ascii [s]
  (mapv int (seq s)))
(def extra [17 31 73 47 23])
(def p2 (into (to-ascii input) extra))

(defn dense [coll]
  (->> (partition 16 coll)
       (map #(apply bit-xor %))
       (map #(format "%02x" %))
       (apply str)))

(comment
  (dense (range 32)))

(defn solve-p2 [s]
  (let [lengths (into (to-ascii s) extra)
        coll (vec (range 256))
        ls' (apply concat (repeat 64 lengths))]
    (-> (solve coll 0 0 ls')
        dense)))

(solve-p2 input)
