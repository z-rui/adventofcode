(let [l1 @[]
      l2 @[]]
  (while (def line (file/read stdin :line))
    (let [pair (map scan-number (peg/match '(* ':d+ :s+ ':d+) line))]
      (map array/push [l1 l2] pair)))
  (sort l1)
  (sort l2)
  (def result (sum (map (comp math/abs -) l1 l2)))
  (print result))
