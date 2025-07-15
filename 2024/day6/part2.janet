(use ./part1)

# This is a bit slow.  A better approach would be memorizing the outcome
# for each [pos dir], so not all configurations need to be simulated to
# the end.
# But this brain-dead solution only takes 5 seconds on my computer, so...
(defn solve [config]
  (def first-result (simulate config))
  (def visited (first-result :exit))
  (def grid-map (config :grid-map))
  (assert visited "already a loop in the first place")
  # we only need to try positions in the path
  (var accum 0)
  (eachk pos visited
    (def saved (get-in grid-map pos))
    (put-in grid-map pos *obstacle*) # temporarily make it an obstacle
    (def trial-result (simulate config))
    (when (trial-result :loop)
      (eprintf "[%d/%d] put an obstacle at %q works"
               accum (length visited) pos)
      (++ accum))
    (put-in grid-map pos saved)) # restore to previous state
  accum)

(defn main [&]
  (let [parsed-input (read-map stdin)]
    (pp (solve parsed-input))))
