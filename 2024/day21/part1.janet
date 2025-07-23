(defn make-keyboard [rows]
  (def kbd @{})
  (eachp [i row] rows
    (eachp [j key] row
      (when key
        (let [ch (key 0)
              pos [i j]]
          (put kbd ch pos)
          (put kbd pos ch)))))
  kbd)

(def *numeric-keyboard*
  (make-keyboard [["7" "8" "9"]
                  ["4" "5" "6"]
                  ["1" "2" "3"]
                  [nil "0" "A"]]))
(def *directional-keyboard*
  (make-keyboard [[nil "^" "A"]
                  ["<" "v" ">"]]))

(defn solve [sequence]
  (def len (length sequence))
  (def numeric-a-pos (comptime '(*numeric-keyboard* (chr "A"))))
  (def directional-a-pos (comptime '(*directional-keyboard* (chr "A"))))
  # search state [matched-len robot1-state robot2-state robot3-state]
  # search space (len + 1) * 11 * 5 * 5 = 1375 (len = 5)
  (def init-state [0
                   numeric-a-pos
                   directional-a-pos
                   directional-a-pos])
  (def costs @{init-state 0})
  (def queue @[init-state])
  (defn apply-offset [kbd [i j] op]
    (def pos
      (case op
        (chr "^") [(- i 1) j]
        (chr "<") [i (- j 1)]
        (chr ">") [i (+ j 1)]
        (chr "v") [(+ i 1) j]
        (errorf "bad op %c" op)))
    (when (kbd pos)
      pos))
  (defn next-state [[matched-len robot1-state robot2-state robot3-state] op]
    (if (= op (chr "A"))
      # robot3 presses its current key
      (if (= robot3-state directional-a-pos)
        # robot2 presses its current key
        (if (= robot2-state directional-a-pos)
          # robot1 presses its current key
          (if (= (*numeric-keyboard* robot1-state) (sequence matched-len))
            [(+ matched-len 1) robot1-state robot2-state robot3-state])
          # robot1 moves
          (when-let [robot1-state* (apply-offset *numeric-keyboard*
                                                 robot1-state
                                                 (*directional-keyboard* robot2-state))]
            [matched-len robot1-state* robot2-state robot3-state]))
        # robot2 moves
        (when-let [robot2-state* (apply-offset *directional-keyboard*
                                               robot2-state
                                               (*directional-keyboard* robot3-state))]
          [matched-len robot1-state robot2-state* robot3-state]))
      # robot3 moves
      (when-let [robot3-state* (apply-offset *directional-keyboard* robot3-state op)]
        [matched-len robot1-state robot2-state robot3-state*])))
  (var n 0)
  (var result nil)
  (while (< n (length queue))
    (def state (queue n))
    (++ n)
    (def cost (get costs state))
    (when (= (first state) len)
      (set result cost)
      (break))
    (each op "A^<v>"
      (when-let [state* (next-state state op)]
        (unless (has-key? costs state*)
          (put costs state* (+ cost 1))
          (array/push queue state*)))))
  result)

(defn main [&]
  (var accum 0)
  (each line (file/lines stdin)
    (buffer/popn line 1)  # remove "\n"
    (def num-part (first (peg/match ~(/ ':d+ ,scan-number) line)))
    (def shortest-seq-len (solve line))
    (tracev [shortest-seq-len num-part])
    (+= accum (* shortest-seq-len num-part)))
  (print accum))
