(defn parse-rules [line]
  (def patterns
    (peg/match
      '{:main (* :rule (any (* ", " :rule)) "\n")
        :rule '(some (set "wubrg"))}
      line))
  patterns)

# Backtracking for all matches would be too slow.
# What we can do instead is to count the number of
# matches for each prefix and derive one after another.
(defn match-count [patterns s]
  (def len (length s))
  (def counts (array/new-filled (+ len 1) 0))
  (put counts 0 1)
  (for i 0 len
    (def curr-count (counts i))
    (when (> curr-count 0)
      (each pat patterns
        (def pat-len (length pat))
        (when (and (<= pat-len (- len i))
                   (= (memcmp s pat pat-len i) 0))
          (let [i* (+ i pat-len)]
            (+= (counts i*) curr-count))))))
  (last counts))

(defn main [&]
  (def patterns (let [line (file/read stdin :line)]
                  (parse-rules line)))
  (file/read stdin :line)  # discard empty line
  (var accum 0)
  (each line (file/lines stdin)
    (buffer/popn line 1)  # discard "\n"
    (+= accum (match-count patterns line)))
  (print accum))
