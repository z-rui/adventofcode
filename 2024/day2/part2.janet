(defn every-adjacent? [pred ind]
  (var prev-value (get ind 0))
  (var ok true)
  (for i 1 (length ind)
    (let [curr-value (get ind i)]
      (unless (pred prev-value curr-value)
        (set ok false)
        (break))
      (set prev-value curr-value)))
  ok)

(defn safe? [record]
  (def ordered?
    (case (cmp (get record 0) (get record 1))
      -1 |(<= 1 (- $1 $0) 3)
      1 |(<= 1 (- $0 $1) 3)
      (break false)))
  (every-adjacent? ordered? record))

# I think there's a linear-time version of this.
# However, brain-dead brute-force solution is fast enough...
(defn safe-with-one-bad? [record]
  (def n (length record))
  (def record* (array/new n))
  (var ok (safe? record))
  (unless ok
    # try removing one element...
    (for i 0 n
      (array/clear record*)
      (for j 0 i
        (array/push record* (in record j)))
      (for j (inc i) n
        (array/push record* (in record j)))
      (when (safe? record*)
        (set ok true)
        (break))))
  ok)

(assert (= (safe-with-one-bad? [7 6 4 2 1]) true))
(assert (= (safe-with-one-bad? [1 2 7 8 9]) false))
(assert (= (safe-with-one-bad? [1 3 2 4 5]) true))

(def result
  (count safe-with-one-bad?
         (map (partial peg/match ~(any (* :s* (/ ':d+ ,scan-number))))
              (file/lines stdin))))
(print result)
