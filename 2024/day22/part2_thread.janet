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
      [:ok ret-val _] (set accum (reduce-f accum ret-val))
      [:error err-val _] (error err-val)))
  accum)

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
  (var seq-key 0)
  (def M (comptime '(math/pow 19 3)))
  (for i 0 iterations
    (set num (next-secret-number num))
    (def price (% num 10))
    (def diff (- price prev-price))
    (set prev-price price)
    (set seq-key (-> seq-key (% M) (* 19) (+ diff 9)))
    (when (>= i 3)
      (update sequence-values
              seq-key
              |(or $ price))))
  sequence-values)

(defn merge-sequence-values [dest src]
  (eachp [k v] src
    (put dest k (+ (get dest k 0) v)))
  dest)

(defn main [&]
  (def numbers
    (seq [line :in (file/lines stdin)]
      (buffer/popn line 1) # remove "\n"
      (scan-number line)))
  (def result
    (thread-map-reduce
      merge-sequence-values
      @{}
      (fn [numbers]
        (reduce merge-sequence-values @{}
                (map (partial compute-sequence-values 2000)
                     numbers)))
      numbers))
  (print (max-of (values result))))
