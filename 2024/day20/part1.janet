(defn parse-input [input]
  (var start-pos nil)
  (var end-pos nil)
  (var cols nil)
  (var i 0)
  (for j 0 (length input)
    (var ch (input j))
    (case ch
      (chr "#") nil
      (chr ".") nil
      (chr "S") (do
                  (set start-pos i)
                  (set ch (chr ".")))
      (chr "E") (do
                  (set end-pos i)
                  (set ch (chr ".")))
      (chr "\n") (do
                   (if-not cols
                     (set cols j))
                   (set ch nil))
      (errorf "bad symbol %c at idx %d" ch i))
    (when ch
      (put input i ch)
      (++ i)))
  (buffer/popn input (- (length input) i))
  [cols input start-pos end-pos])

(defn next-pos [rows cols pos]
  (def res (array/new 4))
  (def [i j] [(div pos cols) (mod pos cols)])
  (if (> i 0) (array/push res (- pos cols)))
  (if (> j 0) (array/push res (- pos 1)))
  (if (< j (- cols 1)) (array/push res (+ pos 1)))
  (if (< i (- rows 1)) (array/push res (+ pos cols)))
  res)

(defn bfs [cols grid-map source-pos]
  (def size (length grid-map))
  (def rows (/ size cols))
  (def queue (array/new size))
  (def costs (array/new-filled size math/inf))
  (defn maybe-enqueue [cost pos]
    (when (< cost (get costs pos math/inf))
      (put costs pos cost)
      (array/push queue pos)))
  (maybe-enqueue 0 source-pos)
  (var n 0)
  (while (< n (length queue))
    (def pos (queue n))
    (++ n)
    (def cost (costs pos))
    (each pos* (next-pos rows cols pos)
      (case (grid-map pos*)
        (chr ".") (maybe-enqueue (+ cost 1) pos*))))
  costs)

# Try cheating at each wall and see how many steps can be saved.
# Saved cost is the difference of the costs at the two empty locations
# (steps that would need to go from one to the other without the cheat)
# minus 2 (the cost for the cheat).
(defn find-cheats [threshold cols grid-map costs]
  (def size (length grid-map))
  (def rows (/ size cols))
  (var accum 0)
  (for pos 0 size
    (when (= (grid-map pos) (chr "#"))
      (def neighbor-costs
        (seq [pos* :in (next-pos rows cols pos)
              :let (cost* (costs pos*))
              :when (not= cost* math/inf)]
          cost*))
      (when (>= (length neighbor-costs) 2)
        (def max-diff (- (max-of neighbor-costs)
                         (min-of neighbor-costs)))
        (when (>= max-diff (+ threshold 2))
          (++ accum)))))
  accum)

(defn main [&]
  (let [input (parse-input (file/read stdin :all))]
    (def [cols grid-map start-pos end-pos] input)
    (def costs (bfs cols grid-map start-pos))
    (print (find-cheats 100 cols grid-map costs))))
