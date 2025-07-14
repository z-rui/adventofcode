(defn count-word-in-matrix [word matrix]
  (def len (length word))
  (def n (length matrix))
  (def m (length (in matrix 1)))
  (defn match-at? [i j di dj]
    (var [i j] [i j])
    (var ok true)
    (for k 0 len
      (unless (and (<= 0 i) (< i n)
                   (<= 0 j) (< j m)
                   (= (in (in matrix i) j) (in word k)))
        (set ok false)
        (break))
      (+= i di)
      (+= j dj))
    ok)
  (var result 0)
  (def all-dirs [[-1 -1] [-1 0] [-1 1]
                 [ 0 -1]        [ 0 1]
                 [ 1 -1] [ 1 0] [ 1 1]])
  (for i 0 n
    (for j 0 m
      (+= result (count |(match-at? i j ;$) all-dirs))))
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
  (assert (= (tracev (count-word-in-matrix "XMAS" test-input)) 18)))

(let [input (seq [x :in (file/lines stdin)] x)]
  (print (count-word-in-matrix "XMAS" input)))
