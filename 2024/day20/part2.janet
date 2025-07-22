(use ./part1)

# Now the cheat can go through multiple locations, but have to start and
# end on an empty location.
# Think of the cheat as a "wormhole" from one empty location to another.
# The cost of the cheat is the Manhatten distance between the endpoints.
(defn find-cheats [threshold max-cheats cols grid-map costs]
  (def size (length grid-map))
  (def rows (/ size cols))
  (var accum 0)
  # Enumerate all pairs (i1 j1) (i2 j2) whose Manhattan distance
  # is within max-cheats.
  (loop [i1 :range [0 rows]
         j1 :range [0 cols]
         :let [pos1 (+ (* i1 cols) j1)
               cost1 (costs pos1)]
         :when (not= cost1 math/inf)
         di :range-to [0 max-cheats]
         :let [i2 (+ i1 di)]
         :while (< i2 rows)
         # Careful not to enumerate any pair twice.
         # If i1=i2, then j2 is (j1+1)..(j1+X).
         # Otherwise it's (j1-X)..(j1+X).
         j2 :range-to [(if (= di 0)
                         (+ j1 1)
                         (max 0
                              (- j1 (- max-cheats di))))
                       (min (- cols 1)
                            (+ j1 (- max-cheats di)))]
         :let [dj (- j2 j1)
               pos2 (+ (* i2 cols) j2)
               cost2 (costs pos2)]
         :when (not= cost2 math/inf)]
    (def diff (math/abs (- cost1 cost2)))
    (def saved (- diff (math/abs di) (math/abs dj)))
    (when (>= saved threshold)
      (++ accum)))
  accum)

(defn main [&]
  (let [input (parse-input (file/read stdin :all))]
    (def [cols grid-map start-pos end-pos] input)
    (def costs (bfs cols grid-map start-pos))
    (print (find-cheats 100 20 cols grid-map costs))))
