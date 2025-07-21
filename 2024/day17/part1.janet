(defn parse-input [input]
  (peg/match
    ~{:main (* (repeat 3 :register) "\n" :program -1)
      :register (* "Register " 1 ": " :int "\n")
      :program (* "Program: " :int (any (* "," :int)) "\n")
      :int (/ ':d+ ,scan-number)}
    input))

(defn simulate [ra rb rc & text]
  (var [ra rb rc] [ra rb rc])
  (var pc 0)
  (def end (length text))
  (defn imm [] (text (+ pc 1)))
  (defn cimm []
    (def v (text (+ pc 1)))
    (if (<= 0 v 3)
      v
      (case v
        4 ra
        5 rb
        6 rc
        (errorf "bad combo immediate %q at pc=%d" v pc))))
  (var sep "")
  (while (< pc end)
    (case (text pc)
      # adv
      0 (set ra (brshift ra (cimm)))
      # bxl
      1 (set rb (bxor rb (imm)))
      # bst
      2 (set rb (mod (cimm) 8))
      # jnz
      3 (when (not= ra 0)
          (set pc (- (imm) 2)))
      # bxc
      4 (set rb (bxor rb rc))
      # out
      5 (do
          (prin sep (mod (cimm) 8))
          (flush)
          (set sep ","))
      # bdv
      6 (set rb (brshift ra (cimm)))
      # cdv
      7 (set rc (brshift ra (cimm))))
    (+= pc 2))
  (print))

(let [input (parse-input (file/read stdin :all))]
  (pp input)
  (apply simulate input))
