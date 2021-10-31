(ns aoc.day13
  (:require
   [clojure.string :as str]))

(def test-firewall {0 {:length 3 :current 0 :direction :down}
                    1 {:length 2 :current 0 :direction :down}
                    4 {:length 4 :current 0 :direction :down}
                    6 {:length 4 :current 0 :direction :down}})

(def firewall
  (->> (slurp "./src/aoc/input_13.txt")
       str/split-lines
       (map #(str/split % #": "))
       (map (fn [[pos length]]
              [(Integer/parseInt pos)
               {:length (Integer/parseInt length)
                :current 0
                :direction :down}]))
       (into {})))

(defn step [fw]
  (update-vals fw (fn [{:keys [current length direction] :as v}]
                    (cond
                      (and (= :up direction)
                           (zero? current))
                      ;; this doesn't work for length 1 - but that doesn't exist in the input
                      (assoc v :current 1 :direction :down)

                      (= :up direction)
                      (assoc v :current (dec current))

                      (and (= :down direction)
                           (= (inc current) length))
                      (assoc v :current (dec current) :direction :up)

                      (= :down direction)
                      (assoc v :current (inc current))))))

(defn move-through
  ([fw]
   (move-through fw 0 (apply max (keys fw)) '() false))
  ([fw exit-early]
   (move-through fw 0 (apply max (keys fw)) '() exit-early))
  ([fw pos end caughts exit-early]
   (if (> pos end)
     caughts
     (let [caught (some-> (fw pos)
                          :current
                          zero?)
           caughts' (if caught (cons pos caughts) caughts)]
       (if (and caught exit-early)
         pos
         (recur (step fw) (inc pos) end caughts' exit-early))))))

(defn severity [poss fw]
  (reduce (fn [acc pos]
            (+ acc
               (* pos ((fw pos) :length))))
          0
          poss))

(-> firewall
    move-through
    (severity firewall))

(defn delay-until-not-caught [fw]
  (loop [fw    fw
         delay 0]
    (when (zero? (mod delay 100000))
      (println "dealy:" delay))
    (if (seq (move-through fw))
      (recur (step fw) (inc delay))
      delay)))

(time (delay-until-not-caught test-firewall))
;; (time (delay-until-not-caught (into {} (take 24 (sort firewall)))))
;; (delay-until-not-caught firewall)
;; too slow.

;;  0 1 2 3 4
;;          0
;;        0 1
;;      0   2
;;    0 1   3
;;0 0 1 2 - 2 0     (mod (+ 2 4) 6) -> 4
;;1 1 0 3   1
;;2 2 1 2   0w
;   1 0 1   1
;   0 1 0   2
;   1 0 1   3
; 6 2 1 2   2 <-- ok
;   1 0 3   1
;   0 1 2   0
;   1 0 1   1
;10 2 1 0   2 <-- nok
;   1 0 1   3
;   0 1 2   2
;   1 0 3   1
;   2 1 2   0
;   1 0 1   1
;   0 1 0   2
;   1 0 1   3
;18 2 1 2   2 <-- ok
;   1 0 3   1
;;  0   2   0
;;  1 0 1   -
;22 2   0   - <-- nok
;;  1 0 -   -
;;  0 - -   -
;;  - 0 -   -
;26 - - -   0 <-- nok
;   - 0 -   -
;   0   0   -
;   - 0     -
;30 - - -   - <-- ok

(def ppt (reduce (fn [acc [k {:keys [length]}]]
                   ;; length + length - 2 is the periodic length
                   ;; when the length >= 2
                   (conj acc [k (+ length length -2)]))
                 []
                 firewall))

(defn test-delay [delay ppt]
  (every? (fn [[pos pl]]
            (not (zero? (mod (+ pos delay) pl))))
          ppt))

(time (first (filter #(test-delay % ppt) (range))))
(time (first (filter #(test-delay % ppt) (take 1000000 (iterate (partial + 12) 6)))))
