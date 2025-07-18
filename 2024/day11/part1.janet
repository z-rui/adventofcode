(defn next-sequence [curr-seq]
  (def next-seq @[])
  (each num curr-seq
    (if (= num 0)
      (array/push next-seq 1)
      (let [str (string num)
            digits (length str)]
        (if (even? digits)
          (let [k (math/pow 10 (/ digits 2))]
            (array/push next-seq (div num k) (mod num k)))
          (array/push next-seq (* num 2024))))))
  next-seq)

#(pp (next-sequence [0 1 10 99 999]))
# -> @[1 2024 1 0 9 9 2021976]

(defn next-n-sequence [n curr-seq]
  (var s curr-seq)
  (repeat n (set s (next-sequence s)))
  s)

(pp (length (next-n-sequence 25 [6571 0 5851763 526746 23 69822 9 989])))
