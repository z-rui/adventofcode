(use ./part1)

# Sure, binary search, but why...
# Linear search runs within 4 secs on my machine.
(defn main [&]
  (let [input (parse-input (file/read stdin :all))
        [rows cols] [71 71]]
    # We know 1024 works from part1
    (def input* (array/slice input 0 1024))
    (for k 1025 (length input)
      (array/push input* (input k))
      (unless (shortest-path-length input* rows cols)
        (let [[i j] (input k)]
          (printf "%d,%d" i j))
        (break)))))
