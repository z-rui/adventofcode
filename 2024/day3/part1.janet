(put (dyn *peg-grammar*) :int ~(/ ':d+ ,scan-number))

(def mul-peg
  (peg/compile
    ~{:mul (/ (* "mul(" :int "," :int ")") ,*)
      :main (any (+ :mul 1))}))

(assert (deep= (tracev (peg/match mul-peg "mul(3,4)")) @[12]))

(defn solve [input]
  (sum (tracev (peg/match mul-peg input))))

(let [test-input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"]
  (assert (= (solve test-input) 161)))

(let [input (file/read stdin :all)]
  (print (solve input)))
