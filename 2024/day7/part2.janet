(defn chop-suffix [number suffix]
  (var power 1)
  (while (<= power suffix)
    (*= power 10))
  # number=123, power=1000
  # number=10, power=100
  (when (= (mod (- number suffix) power) 0)
    (div number power)))

(assert (-> 12345 (chop-suffix 45) (= 123)))
(assert (-> 12310 (chop-suffix 10) (= 123)))
(assert (-> 12310 (chop-suffix 1) (= nil)))

(defn feasible? [target numbers]
  # assuming all numbers are between 0 and 2^53
  (def n (length numbers))
  (assert (> n 0))
  # O(2^n).  Hopefully n is small.
  (defn recur [target i]
    (let [number (get numbers i)]
      (if
        (= i 0) (= target number)
        (or (when (>= target number)
              (recur (- target number) (- i 1)))
            (when (= (mod target number) 0)
              (recur (div target number) (- i 1)))
            (when-let [target* (chop-suffix target number)]
              (recur target* (- i 1)))))))
  (recur target (- n 1)))

(def *test-case*
  [[190 [10 19]]
   [3267 [81 40 27]]
   [83 [17 5]]
   [156 [15 6]]
   [7290 [6 8 6 15]]
   [161011 [16 10 13]]
   [192 [17 8 14]]
   [21037 [9 7 18 13]]
   [292 [11 6 16 20]]])

(assert (= (count |(apply feasible? $) *test-case*) 6))

(put (dyn *peg-grammar*) :int ~(/ ':d+ ,scan-number))

(def *line-peg*
  (peg/compile ~(* :int ":" (/ (some (* " " :int))
                               ,tuple))))

(defn main [&]
  (var accum 0)
  (each line (file/lines stdin)
    (def [target number] (peg/match *line-peg* line))
    (when (feasible? target number)
      (+= accum target)))
  (pp accum))
