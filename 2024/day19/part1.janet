(defn parse-rules [line]
  (def patterns
    (peg/match
      '{:main (* :rule (any (* ", " :rule)) "\n")
        :rule '(some (set "wubrg"))}
      line))
  patterns)

(defn match? [patterns s]
  (label ret
    (defn recur [idx len]
      (if (= len 0) (return ret true))
      (each pat patterns
        (def pat-len (length pat))
        (when (and (<= pat-len len)
                   (= (memcmp s pat pat-len idx) 0))
          (recur (+ idx pat-len) (- len pat-len)))))
    (recur 0 (length s))))

(defn main [&]
  (def patterns (let [line (file/read stdin :line)]
                  (parse-rules line)))
  (file/read stdin :line)  # discard empty line
  (var accum 0)
  (each line (file/lines stdin)
    (buffer/popn line 1)  # discard "\n"
    (when (match? patterns line)
      (++ accum)))
  (print accum))
