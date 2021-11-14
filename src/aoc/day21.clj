(ns aoc.day21
  (:require
   [clojure.string :as str]))

(defn parse-pattern [p]
  (->> p
       str/split-lines
       (mapv #(mapv vector %))))

(def start-pattern
  (parse-pattern
   ".#.
..#
###"))

(defn size-of [p]
  (count p))

(defn process [p]
  (if (zero? (mod (size-of p) 2))
    ;; split in 2x2 and enhance
    nil
    ;; split in 3x3 and enhance
    nil))

;; (defn rotate [matrix]
;;   )

;; 1234

;; 4123

;; 3412



;; 1 2 | 2 1
;; 3 4 | 4 3

;; 3 1 | 4 2
;; 4 2 | 3 1

;; 4 3 | 3 4
;; 2 1 | 1 2

;; 2 4 | 1 3
;; 1 3 | 2 4
