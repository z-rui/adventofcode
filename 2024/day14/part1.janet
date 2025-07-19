(defn parse-input [input]
  (peg/match
    ~{:main (* (any :line) -1)
      :line (/ (* "p=" :int "," :int " v=" :int "," :int "\n") ,tuple)
      :int (/ '(* (? "-") :d+) ,scan-number)}
    input))

(defn solve (n xsize ysize robots)
  (defn final-pos [x y dx dy]
    (def x* (mod (+ x (* n dx)) xsize))
    (def y* (mod (+ y (* n dy)) ysize))
    [x* y*])
  (def mid-x (div xsize 2))
  (def mid-y (div ysize 2))
  (defn pos-to-quadrant [x y]
    (case (cmp x mid-x)
      -1 (case (cmp y mid-y)
           -1 0
           +1 1)
      +1 (case (cmp y mid-y)
           -1 2
           +1 3)))
  (def counts (array/new-filled 4 0))
  (each robot robots
    (def pos (final-pos ;robot))
    (if-let [quadrant (pos-to-quadrant ;pos)]
      (++ (counts quadrant))))
  (* ;counts))

(let [input (parse-input (file/read stdin :all))
      solution (solve 100 101 103 input)]
  (print solution))
