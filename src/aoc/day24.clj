(ns aoc.day24
  (:require
   [clojure.string :as str]))

(def input "32/31
2/2
0/43
45/15
33/24
20/20
14/42
2/35
50/27
2/17
5/45
3/14
26/1
33/38
29/6
50/32
9/48
36/34
33/50
37/35
12/12
26/13
19/4
5/5
14/46
17/29
45/43
5/0
18/18
41/22
50/3
4/4
17/1
40/7
19/0
33/7
22/48
9/14
50/43
26/29
19/33
46/31
3/16
29/46
16/0
34/17
31/7
5/27
7/4
49/49
14/21
50/9
14/44
29/29
13/38
31/11")

(def components
  (->> input
       str/split-lines
       (map (fn [l] (str/split l #"/")))
       (map (fn [[v1 v2]] [(Integer/parseInt v1) (Integer/parseInt v2)]))
       (into #{})))

(defn strongest-bridge
  ([components]
   (apply max (for [[v1 v2] components
                    :when (or (zero? v1) (zero? v2))]
                (if (zero? v1)
                  (strongest-bridge (disj components [v1 v2]) [v1 v2])
                  (strongest-bridge (disj components [v1 v2]) [v2 v1])))))
  ([components bridge]
   (let [next-conn (last bridge)
         value     (reduce + bridge)
         bridges   (for [[v1 v2] components
                         :when (or (= v1 next-conn) (= v2 next-conn))]
                     (if (= v1 next-conn)
                       (strongest-bridge (disj components [v1 v2]) (conj bridge v1 v2))
                       (strongest-bridge (disj components [v1 v2]) (conj bridge v2 v1))))
         value'    (if (seq bridges)
                     (apply max bridges)
                     0)]
     (max value value'))))

;; part2
(defn best [lbridges]
  (println lbridges)
  (let [longest (first (apply max-key first lbridges))]
    (second (apply max-key second (filter (fn [[l b]] (= l longest)) lbridges)))))

(defn strongest-bridge2
  ([components]
   (apply max (for [[v1 v2] components
                    :when (or (zero? v1) (zero? v2))]
                (if (zero? v1)
                  (strongest-bridge2 (disj components [v1 v2]) [v1 v2])
                  (strongest-bridge2 (disj components [v1 v2]) [v2 v1])))))
  ([components bridge]
   (let [next-conn (last bridge)
         value     [(count bridge) (reduce + bridge)]
         bridges   (for [[v1 v2] components
                         :when (or (= v1 next-conn) (= v2 next-conn))]
                     (if (= v1 next-conn)
                       (strongest-bridge2 (disj components [v1 v2]) (conj bridge v1 v2))
                       (strongest-bridge2 (disj components [v1 v2]) (conj bridge v2 v1))))]
     (best (conj bridges value)))))

(strongest-bridge2 components)
