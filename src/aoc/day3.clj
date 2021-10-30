(ns aoc.day3)

(defn distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))))

(defn next-dir [count dir]
  (case dir
    :left [count :down]
    :down [(inc count) :right]
    :right [count :up]
    :up [(inc count) :left]))

(defn move [dir [x y]]
  (case dir
    :left [(dec x) y]
    :down [x (dec y)]
    :right [(inc x) y]
    :up [x (inc y)]))

(defn square-numbers
  "1
   4 * 1 + 4 corners
   4 * 3 + 4 corners
   4 * 5 + 4 corners"
  ([] (square-numbers 1 0 [1 :right] [0 0]))
  ([n c [count dir] [x y]]
   (if (= (inc c) count)
     ;; last step
     (lazy-seq (cons [n [x y]]
                     (square-numbers (inc n) 0 (next-dir count dir) (move dir [x y]))))
     (lazy-seq (cons [n [x y]]
                     (square-numbers (inc n) (inc c) [count dir] (move dir [x y])))))))

(defn steps-from [n]
  (let [[_ coord] (first (drop (dec n) (square-numbers)))]
    (distance [0 0] coord)))

;; Data from square 1 is carried 0 steps, since it's at the access port.
;; Data from square 12 is carried 3 steps, such as: down, left, left.
;; Data from square 23 is carried only 2 steps: up twice.
;; Data from square 1024 must be carried 31 steps.

(comment
  (steps-from 1)
  (steps-from 12)
  (steps-from 23)
  (steps-from 1024)
  (steps-from 325489)
  ,)

(defn neighbours [[x y]]
  (for [dx (range -1 2)
        dy (range -1 2)]
    [(+ x dx) (+ y dy)]))

(defn value-at [p values]
  (reduce +
          (map (fn [p] (get values p 0))
               (neighbours p))))

(defn enumerate-to [n]
  (loop [values      {[0 0] 1}
         [count dir] [1 :right]
         counter     0
         position    [0 0]]
    (let [p' (move dir position)
          v  (value-at p' values)
          vs (assoc values p' v)]
      (if (> v n)
        v
        (if (= (inc counter) count)
          (recur vs (next-dir count dir) 0 p')
          (recur vs [count dir] (inc counter) p'))))))

(comment
  (enumerate-to 325489))