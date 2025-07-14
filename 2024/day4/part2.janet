# Hardcoded to find "MAS"...
(defn myhash [x y] (+ (* x 256) y))

(defn count-word-in-matrix [matrix]
  (def n (length matrix))
  (def m (length (in matrix 1)))
  (defn at [i j] (in (in matrix i) j))
  (defn match-at? [i j]
    (defn m-and-s? [x y]
      (case (myhash x y)
        (comptime (myhash (chr "M") (chr "S"))) true
        (comptime (myhash (chr "S") (chr "M"))) true
        false))
    (and (= (at i j) (chr "A"))
         (m-and-s? (at (- i 1) (- j 1))
                   (at (+ i 1) (+ j 1)))
         (m-and-s? (at (+ i 1) (- j 1))
                   (at (- i 1) (+ j 1)))))
  (var result 0)
  (for i 1 (- n 1)
    (for j 1 (- m 1)
      (when (match-at? i j)
        (++ result))))
  result)

(let [test-input ["MMMSXXMASM"
                  "MSAMXMSMSA"
                  "AMXSXMAAMM"
                  "MSAMASMSMX"
                  "XMASAMXAMM"
                  "XXAMMXXAMA"
                  "SMSMSASXSS"
                  "SAXAMASAAA"
                  "MAMMMXMMMM"
                  "MXMXAXMASX"]]
  (assert (= (tracev (count-word-in-matrix test-input)) 9)))

(let [input (seq [x :in (file/lines stdin)] x)]
  (print (count-word-in-matrix input)))
