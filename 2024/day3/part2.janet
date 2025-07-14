(put (dyn *peg-grammar*) :int ~(/ ':d+ ,scan-number))

(defn solve [input]
  (var skip false)
  (def mul-peg
    ~{:mul (/ (* "mul(" :int "," :int ")") ,|(if skip 0 (* $0 $1)))
      :dont (drop (/ "don't()" ,|(set skip true)))
      :do (drop (/ "do()" ,|(set skip false)))
      :main (any (+ :mul :dont :do 1))})
  (sum (tracev (peg/match mul-peg input))))

(let [test-input "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"]
  (assert (= (solve test-input) 48)))

(let [input (file/read stdin :all)]
  (print (solve input)))
