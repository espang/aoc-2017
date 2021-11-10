(ns aoc.day15)

;; solved with lazy-seqs
;; (defn a [v]
;;    (lazy-seq (cons v (a (mod (* v 16807) 2147483647)))))
;; (defn b [v]
;;    (lazy-seq (cons v (b (mod (* v 48271) 2147483647)))))
;; (defn check [[v1 v2]]
;;   (= (bit-and v1 2r1111111111111111)
;;      (bit-and v2 2r1111111111111111)))
;; (time (->> (interleave (a 65) (b 8921))
;;            (partition 2)
;;            (drop 1)
;;            (take 40000000)
;;            (filter check)
;;            (count)))

(defn gen-a [v] (mod (* v 16807) 2147483647))
(defn gen-b [v] (mod (* v 48271) 2147483647))

(defn check [v1 v2]
  (= (bit-and v1 2r1111111111111111)
     (bit-and v2 2r1111111111111111)))

(defn solve [fna a fnb b total]
  (loop [a a b b n 0 c 0]
    (if (= n total)
      c
      (recur (fna a) (fnb b) (inc n) (if (check a b) (inc c) c)))))

(time (solve gen-a 65 gen-b 8921 40000000))
(solve gen-a 116 gen-b 299 40000000)

(defn gen-a' [a]
  (let [a' (gen-a a)]
    (if (zero? (mod a' 4)) a' (recur a'))))

(defn gen-b' [b]
  (let [b' (gen-b b)]
    (if (zero? (mod b' 8)) b' (recur b'))))

(solve gen-a' 65 gen-b' 8921 5000000)
(time (solve gen-a' 116 gen-b' 299 5000000))
