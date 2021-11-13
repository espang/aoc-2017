(ns aoc.day17
  (:require
   [clojure.data.finger-tree :as ft]))

;; part 1
(defn step [coll steps n]
  (conj (take n (drop (inc steps) (cycle coll))) n))
(time (loop [n    1
             coll (list 0)]
        (if (>= 2017 n)
          (recur (inc n) (step coll 366 n))
          (take 5 coll))))


;; part 2 with counted-double-list
(defn- add-into [cdl step n]
  (let [i (mod step (count cdl))
        [l v r] (ft/ft-split-at cdl i)]
    (ft/conjl (ft/ft-concat r (into l [v]))
              n)))

(defn solve [step n]
  (loop [cdl (apply ft/counted-double-list [0])
         cur 1]
    (when (zero? (mod cur 1000000))
      (println "cur is: " cur))
    (if (>= n cur)
      (recur (add-into cdl step cur) (inc cur))
      cdl)))

(comment
  (time (take 3 (solve 366 2017)))
  (time (take 2 (drop-while #(not (zero? %)) (solve 366 1000000))))
  ;; "Elapsed time: 39933.288064 msecs"
  ;; (0 422543)
  ;; ~ 33 minutes for 50_000_000 if the needed time grows linear
  )

(defn val-after-0
  "When the question is value after 0 the construction of the list
  is not necessary. Keep track of th 0 index and the value after."
  [step n]
  (loop [zero-index  0
         val-after-0 0
         length      1
         next-val    1
         cur-index   0]
    (if (> next-val n)
      val-after-0
      (let [index-to-insert (mod (+ step cur-index) length)]
        (cond
          (> index-to-insert zero-index)
          (recur zero-index val-after-0 (inc length) (inc next-val) (inc index-to-insert))

          (< index-to-insert zero-index)
          (recur (inc zero-index) val-after-0 (inc length) (inc next-val) (inc index-to-insert))

          (= index-to-insert zero-index)
          (recur zero-index next-val (inc length) (inc next-val) (inc zero-index)))))))

(comment
  ;; verify the algorithm
  (take 2 (drop-while #(not (zero? %)) (solve 366 2017)))
  (val-after-0 366 2017))

(time (val-after-0 366 50000000))
;; "Elapsed time: 1587.272904 msecs"
