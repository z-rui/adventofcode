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

(def *cache* @{})
(defn move-cost (level [i1 j1] [i2 j2] &opt kbd)
  (default kbd *directional-keyboard*)

  (assert (kbd [i1 j1]))
  (assert (kbd [i2 j2]))

  (if (= level 0) (break 0))

  (when-let [cache-val (*cache* [level i1 j1 i2 j2])]
    (break cache-val))
  (defn make-op [diff key]
    (cond
      (> diff 0) [(key 0) diff]
      (< diff 0) [(key 1) (- diff)]))
  (def i-op (make-op (- i2 i1) "v^"))
  (def j-op (make-op (- j2 j1) "><"))

  # Generate possible move sequences to get from (i1 j1) to (i2 j2).
  (def move-seqs
    (cond
      (= i-op j-op nil) []
      (= i-op nil) [[j-op]]
      (= j-op nil) [[i-op]]
      (= (kbd [i2 j1]) nil) [[j-op i-op]]
      (= (kbd [i1 j2]) nil) [[i-op j-op]]
      [[i-op j-op] [j-op i-op]]))

  # Calculate the cost for each sequence.
  (defn move-seq-cost [move-seq]
    (def a-pos (comptime '(*directional-keyboard* (chr "A"))))
    (var curr-pos a-pos)
    (var cost 0)
    (each [key n] move-seq
      # How to press key n times using the outer layer robot?
      (def key-pos (*directional-keyboard* key))
      (+= cost
          # 1. Aim at this key
          (move-cost (- level 1) curr-pos key-pos)
          # 2. Activate n times
          n)
      (set curr-pos key-pos))
    # Move back to "A"
    (+= cost (move-cost (- level 1) curr-pos a-pos))
    cost)

  (def costs (map move-seq-cost move-seqs))
  (def min-cost (or (min-of costs) 0))
  (set (*cache* [level i1 j1 i2 j2]) min-cost))

(defn solve [level key-seq]
  (var curr-pos (comptime '(*numeric-keyboard* (chr "A"))))
  (var cost 0)
  (each key key-seq
    (def key-pos (*numeric-keyboard* key))
    (assert key-pos)
    (+= cost
        # Aim at this key
        (move-cost level curr-pos key-pos *numeric-keyboard*)
        # Activate it
        1)
    (set curr-pos key-pos))
  cost)

(defn main [&]
  (def level 26)
  (var accum 0)
  (each line (file/lines stdin)
    (buffer/popn line 1)  # remove "\n"
    (def num-part (first (peg/match ~(/ ':d+ ,scan-number) line)))
    (def shortest-seq-len (solve level line))
    (tracev [shortest-seq-len num-part])
    (+= accum (* shortest-seq-len num-part)))
  (print accum))
