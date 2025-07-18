# Naive solution is not fast enough for n=75
# We only need to know how long the result is
# for any number to transform n times.
# We can use memorization for that.

(defn split-even [num]
  (var digits 1)
  (var power 10)
  (while (<= power num)
    (++ digits)
    (*= power 10))
  (when (even? digits)
    (set power (math/pow 10 (/ digits 2)))
    (assert (int? power))
    [(div num power) (mod num power)]))

(def *cache* @{})
(defn num-to-seq-len [num n]
  (if (= n 0) (break 1))  # base case
  (def cache-line (or (in *cache* num)
                      (set (*cache* num) @[])))
  (if-let [cache-val (get cache-line n)]
    (break cache-val))
  (def next-seq (if (= num 0)
                  [1]
                  (or (split-even num)
                      [(* num 2024)])))
  (var accum 0)
  (each num* next-seq
    (+= accum (num-to-seq-len num* (- n 1))))
  (set (cache-line n) accum))

(defn solve [numbers n]
  (var accum 0)
  (each num numbers
    (+= accum (num-to-seq-len num n)))
  accum)

(pp (solve [6571 0 5851763 526746 23 69822 9 989] 75))
