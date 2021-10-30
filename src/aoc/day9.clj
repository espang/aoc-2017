(ns aoc.day9)

(defn consume-garbage [cs]
  (loop [cancel  false
         cs      cs
         content []]
    (if (seq cs)
      (let [c       (first cs)
            stop    (and (not cancel) (= \> c))
            cancel' (and (not cancel) (= \! c))
            content' (if (or cancel cancel')
                       content
                       (conj content c))]
        (if stop
          [(rest cs) (apply str content)]
          (recur cancel' (rest cs) content')))
      (do (println "unfinished garbage")
          nil))))

(comment
  (consume-garbage (seq "> after"))
  (consume-garbage "random> after")
  (consume-garbage "<<<> after")
  (consume-garbage "<<!><> after")
  (consume-garbage "!!> after"))

(defn consume-group
  ([cs]
   (first (consume-group cs 0)))
  ([cs lvl]
   (loop [cs      cs
          content []]
     (if (seq cs)
       (case (first cs)
         \{ (let [[inner-group cs'] (consume-group (rest cs) (inc lvl))]
              (recur cs' (conj content [:group inner-group])))
         \< (let [[cs' garbage] (consume-garbage (rest cs))]
              (recur cs' (conj content [:garbage garbage])))
         \, (recur (rest cs) content)
         \} [content (rest cs)])
       (if (or (zero? lvl) (= 1 lvl))
         content
         (do (println "unfinished group")
             nil))))))

(comment
  (consume-group "{}")
  (consume-group "{<a>}")
  (consume-group "{{}}"))

(defn value-group
  ([g]
   (value-group g 1))
  ([[t children] v]
   (case t
     :garbage 0
     :group   (let [v' (->> children
                            (map #(value-group % (inc v)))
                            (reduce +))]
                (+ v v')))))

(comment
    (-> "{{{},{},{{}}}}"
      consume-group
      value-group))

(-> "./src/aoc/input_9.txt"
    slurp
    consume-group
    value-group)

(defn count-garbage [[t props]]
  (case t
    :group   (reduce + (map count-garbage props))
    :garbage (count props)))

(-> "./src/aoc/input_9.txt"
    slurp
    consume-group
    count-garbage)
