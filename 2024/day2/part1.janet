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

(assert (= (safe? [7 6 4 2 1]) true))
(assert (= (safe? [1 2 7 8 9]) false))

(def result
  (count safe?
         (map (partial peg/match ~(any (* :s* (/ ':d+ ,scan-number))))
              (file/lines stdin))))
(print result)
