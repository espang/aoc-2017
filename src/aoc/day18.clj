(ns aoc.day18
  (:require
   [clojure.string :as str]))

(defn register [] {})

(defn value-of [register value-or-location]
  (if (int? value-or-location)
    value-or-location
    (register value-or-location 0)))

(defn to-value-or-location [s]
  (try
    (Integer/parseInt s)
    (catch Exception _
      (first s))))

(defn- one-int [line]
  (let [[_ x] (str/split line #" " 2)]
    (list (to-value-or-location x))))

(defn- two-int [line]
  (let [[_ x y] (str/split line #" " 3)]
    (list (to-value-or-location x) (to-value-or-location y))))

(defmulti parse #(subs % 0 3))

(defmethod parse "snd" [line]
  (conj (one-int line) :snd))

(defmethod parse "set" [line]
  (conj (two-int line) :set))

(defmethod parse "add" [line]
  (conj (two-int line) :add))

(defmethod parse "mul" [line]
  (conj (two-int line) :mul))

(defmethod parse "mod" [line]
  (conj (two-int line) :mod))

(defmethod parse "rcv" [line]
  (conj (one-int line) :rcv))

(defmethod parse "jgz" [line]
  (conj (two-int line) :jgz))

(defmulti exec (fn [[action] _] action))

(defmethod exec :snd [[_ x] register]
  [(assoc register :last-sound (value-of register x)) 1])

(defmethod exec :set [[_ x y] register]
  [(assoc register x (value-of register y)) 1])

(defmethod exec :add [[_ x y] register]
  [(assoc register x (+ (value-of register x)
                        (value-of register y))) 1])

(defmethod exec :mul [[_ x y] register]
  [(assoc register x (* (value-of register x)
                        (value-of register y))) 1])

(defmethod exec :mod [[_ x y] register]
  [(assoc register x (mod (value-of register x)
                          (value-of register y))) 1])

(defmethod exec :rcv [[_ x] register]
  (if-not (zero? (value-of register x))
    [register :recover]
    [register 1]))

(defmethod exec :jgz [[_ x y] register]
  (if (pos? (value-of register x))
    [register (value-of register y)]
    [register 1]))


(def test-instructions
  (->> "set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2"
       str/split-lines
       (mapv parse)))

(defn execute-instructions [v]
  (loop [pos 0
         reg (register)]
    (if (<= 0 pos (dec (count v)))
      (let [[reg' n] (exec (v pos) reg)]
        (if (= :recover n)
          [reg' :recover]
          (recur (+ pos n) reg')))
      [reg :outside pos])))

(execute-instructions test-instructions)

(def instructions (->> (slurp "./src/aoc/input_18.txt")
                       str/split-lines
                       (mapv parse)))

(execute-instructions instructions)

;; part 2

(defmethod exec :snd [[_ x] register]
  [(-> register
       (update :send-q conj (value-of register x))
       (update :times-send (fnil inc 0)))
       1])

(defmethod exec :rcv [[_ x] register]
  (let [q (register :receive-q)]
    (if (empty? q)
      [register :wait]
      [(-> register
           (assoc x (peek q))
           (assoc :receive-q (pop q)))
       1])))

(def test-instructions-p2
  (->> "snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d"
       str/split-lines
       (mapv parse)))

(defn valid-pos? [v pos] (<= 0 pos (dec (count v))))

(defn- run-until-wait-or-outside [v reg pos]
  (loop [pos pos
         reg reg]
    (if (valid-pos? v pos)
      (let [[reg' n] (exec (v pos) reg)]
        (if (= :wait n)
          [reg' :wait pos]
          (recur (+ pos n) reg')))
      [reg :outside pos])))

(defn reg-p2 [reg i]
  (-> reg
      (assoc \p i)
      (assoc :receive-q (clojure.lang.PersistentQueue/EMPTY))
      (assoc :send-q (clojure.lang.PersistentQueue/EMPTY))))

(defn execute-instructions-p2 [v]
  (let [reg0 (reg-p2 (register) 0)
        reg1 (reg-p2 (register) 1)]
    (loop [reg0 reg0
           pos0 0
           reg1 reg1
           pos1 0
           active 0
           start true]
      (case active
        0
        (let [[reg0' status pos0'] (run-until-wait-or-outside v reg0 pos0)]
          (case status
            :outside
            (let [[reg1' _ _] (run-until-wait-or-outside v reg1 pos1)]
              [reg0' reg1'])

            :wait
            (let [q (:send-q reg0')]
              (if (and (not start) (empty? q))
                ;; deadlock reg1 was waiting and there are no new values
                [reg0' reg1]
                (recur (assoc reg0' :send-q (clojure.lang.PersistentQueue/EMPTY))
                       pos0'
                       (assoc reg1 :receive-q q)
                       pos1
                       1
                       start)))))

        1
        (let [[reg1' status pos1'] (run-until-wait-or-outside v reg1 pos1)]
          (case status
            :outside
            (let [[reg0' _ _] (run-until-wait-or-outside v reg0 pos0)]
              [reg0' reg1'])

            :wait
            (let [q (:send-q reg1')]
              (if (and (not start) (empty? q))
                ;; deadlock reg1 was waiting and there are no new values
                [reg0 reg1']
                (recur (assoc reg0 :receive-q q)
                       pos0
                       (assoc reg1' :send-q (clojure.lang.PersistentQueue/EMPTY))
                       pos1'
                       0
                       false)))))))))

(comment
  (def t-result (execute-instructions-p2 test-instructions-p2))
  (select-keys (first t-result) [\a \b \c \d])
  (select-keys (second t-result) [\a \b \c \d])
  (def result (execute-instructions-p2 instructions))
  (select-keys (first result) [\p :times-send])
  ;; answer part2:
  (select-keys (second result) [\p :times-send]))