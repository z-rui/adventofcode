(use ./part1)
(defn thread-map-reduce [reduce-f init map-f ind &opt num-threads]
  (default num-threads (os/cpu-count))
  (var accum init)
  (def part-length (math/ceil (/ (length ind)
                                 num-threads)))
  (def supervisor (ev/thread-chan))
  (each part (partition part-length ind)
    (ev/thread map-f part :n supervisor))
  (repeat num-threads
    (match (ev/take supervisor)
      [:ok ret-val _] (+= accum ret-val)
      [:error err-val _] (error err-val)))
  accum)

# This is a bit slow.  A better approach would be memorizing the outcome
# for each [pos dir], so not all configurations need to be simulated to
# the end.
# But this threaded solution only takes 1 second on my computer, so...
(defn solve [config]
  (def first-result (simulate config))
  (def visited (first-result :exit))
  (def grid-map (config :grid-map))
  (assert visited "already a loop in the first place")
  # we only need to try positions in the path
  (thread-map-reduce
    + 0
    (fn [positions] (var accum 0)
      (each pos positions
        (def saved (get-in grid-map pos))
        # multi-thread safe, because data is copied
        (put-in grid-map pos *obstacle*) # temporarily make it an obstacle
        (def trial-result (simulate config))
        (when (trial-result :loop)
          (comment eprintf "[%d/%d] put an obstacle at %q works"
                   accum (length visited) pos)
          (++ accum))
        (put-in grid-map pos saved)) # restore to previous state
      accum)
    (keys visited)))

(defn main [&]
  (let [parsed-input (read-map stdin)]
    (pp (solve parsed-input))))
