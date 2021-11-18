(ns aoc.day25)

(def state-machine
  {:A {0 {:write 1
          :move :right
          :next :B}
       1 {:write 0
          :move :left
          :next :C}}
   :B {0 {:write 1
          :move :left
          :next :A}
       1 {:write 1
          :move :right
          :next :C}}
   :C {0 {:write 1
          :move :right
          :next :A}
       1 {:write 0
          :move :left
          :next :D}}
   :D {0 {:write 1
          :move :left
          :next :E}
       1 {:write 1
          :move :left
          :next :C}}
   :E {0 {:write 1
          :move :right
          :next :F}
       1 {:write 1
          :move :right
          :next :A}}
   :F {0 {:write 1
          :move :right
          :next :A}
       1 {:write 1
          :move :right
          :next :E}}})

(defn computer [state-machine state n]
  (loop [state  state
         steps  0
         cursor 0
         ones   #{}]
    (if (= steps n)
      (count ones)
      (let [value (if (contains? ones cursor) 1 0)
            _ (println state ones cursor value)
            {:keys [write move state]} (get-in state-machine [state value])]

        (println write move state)
        (recur state
               (inc steps)
               (case move
                 :right (inc cursor)
                 :left (dec cursor))
               (case write
                 0 (disj ones cursor)
                 1 (conj ones cursor)))))))

(computer state-machine :A 12261543)