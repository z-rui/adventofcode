(defn parse-input [input]
  (peg/match
    ~{:main (* :case (any (* "\n" :case)))
      :case (/ (* :button :button :prize) ,tuple)
      :button (* "Button " (range "AZ") ": X+" :int ", Y+" :int "\n")
      :prize (* "Prize: X=" :int ", Y=" :int "\n")
      :int (/ ':d+ ,scan-number)}
    input))

(defn solve-eqn [a b c d x y]
  ``Solve the equations
  au + cv = x
  bu + dv = y
  ``
  (def D (- (* a d) (* b c)))
  (def Du (- (* x d) (* c y)))
  (def Dv (- (* a y) (* x b)))

  (if (= D 0)
    (let [sol @[]]
      (if (and (= Du 0)
               (= (mod x c) 0))
        (array/push sol [0 (div x c)]))
      (if (and (= Dv 0)
               (= (mod x a) 0))
        (array/push sol [(div x a) 0])))
    (if (and (= (mod Du D) 0)
             (= (mod Dv D) 0))
      # Cramer's rule
      [[(div Du D) (div Dv D)]]
      # no integer solution
      [])))

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
