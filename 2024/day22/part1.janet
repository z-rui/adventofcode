(defn next-secret-number [num]
  (def M 16777216)
  (var num num)
  (set num (mod (bxor num (blshift num 6)) M))
  (set num (mod (bxor num (brshift num 5)) M))
  (set num (mod (bxor num (blshift num 11)) M)))

(var accum 0)
(each line (file/lines stdin)
  (buffer/popn line 1)  # remove "\n"
  (var num (scan-number line))
  (repeat 2000
    (set num (next-secret-number num)))
  (+= accum num))
(print accum)
