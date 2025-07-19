(defn parse-input [input]
  (peg/match
    ~{:main (* :case (any (* "\n" :case)))
      :case (/ (* :button :button :prize) ,tuple)
      :button (* "Button " (range "AZ") ": X+" :int ", Y+" :int "\n")
      :prize (* "Prize: X=" :int ", Y=" :int "\n")
      :int (/ ':d+ ,scan-number)}
    input))

(defn det [a b c d] (- (* a d) (* b c)))

(defn solve-eqn
  ``Solve the equations
  au + cv = x
  bu + dv = y
  ``
  [a b c d x y]

  (def sol @[])
  (def [D Du Dv] [(det a b c d)
                  (det x y c d)
                  (det a b x y)])
  (if (= D 0)
    (do
      (if (= 0 Du (mod x c))
        (array/push sol [0 (div x c)]))
      (if (= 0 Dv (mod x a))
        (array/push sol [(div x a) 0])))
    (if (= 0 (mod Du D) (mod Dv D))
      # Cramer's rule
      (array/push sol [(div Du D) (div Dv D)])))
  sol)

(defn solve-case [input-case]
  (defn cost [[u v]] (+ (* u 3) v))
  (def solutions (solve-eqn ;input-case))
  (min-of (map |[(cost $) $] solutions)))

(defn solve [parsed-input]
  (var accum 0)
  (each input-case parsed-input
    (if-let [[cost sol] (solve-case input-case)]
      (+= accum cost)))
  accum)

(defn main [&]
  (-> (file/read stdin :all)
      (parse-input)
      (solve)
      (print)))
