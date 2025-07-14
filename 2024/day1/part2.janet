(let [l1 @[]
      l2 @[]]
  (while (def line (file/read stdin :line))
    (let [pair (map scan-number (peg/match '(* ':d+ :s+ ':d+) line))]
      (map array/push [l1 l2] pair)))
  (def freq (frequencies l2))
  (def result (sum (map |(* $ (get freq $ 0)) l1)))
  (print result))
