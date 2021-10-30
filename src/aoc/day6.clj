(ns aoc.day6)

(def input [0	5	10	0	11	14	13	4	11	8	8	7	1	4	12	11])

(defn find-max [v]
  (->> (map-indexed vector v)
       reverse
       (apply max-key second)))

(defn rotate [blocks]
  (let [[idx val] (find-max blocks)]
    (loop [n val
           i (inc idx)
           v (assoc blocks idx 0)]
      (if (zero? n)
        v
        (let [i (mod i (count blocks))]
          (recur (dec n)
                 (inc i)
                 (update v i inc)))))))

(defn find-duplicate [blocks]
  (loop [seen   {}
         steps  1
         blocks blocks]
    (let [b' (rotate blocks)]
      (println b')
      (if (contains? seen b')
        [steps (seen b')]
        (recur (conj seen [b' steps])
               (inc steps)
               b')))))

(let [[end start] (find-duplicate input)]
  [end (- end start)])
