(defn next-secret-number [num]
  (def M 16777216)
  (as-> num $
        (mod (bxor $ (blshift $ 6)) M)
        (mod (bxor $ (brshift $ 5)) M)
        (mod (bxor $ (blshift $ 11)) M)))

(defn compute-sequence-values [iterations num]
  (var prev-price (% num 10))
  (var num num)
  (def sequence-values @{})
  (def seq-key (array/new 4))
  (for i 0 iterations
    (set num (next-secret-number num))
    (def price (% num 10))
    (def diff (- price prev-price))
    (set prev-price price)
    (when (>= i 4)
      (array/remove seq-key 0))
    (array/push seq-key diff)
    (when (>= i 3)
      (update sequence-values
              (tuple/slice seq-key)
              |(or $ price))))
  sequence-values)

(defn merge-sequence-values [dest src]
  (eachp [k v] src
    (put dest k (+ (get dest k 0) v)))
  dest)

(defn main [&]
  (def result @{})
  (each line (file/lines stdin)
    (buffer/popn line 1)  # remove "\n"
    (def num (scan-number line))
    (merge-sequence-values result
                           (compute-sequence-values 2000 num)))
  (print (max-of (values result))))
