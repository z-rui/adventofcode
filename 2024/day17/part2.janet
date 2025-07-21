(defn parse-input [input]
  (peg/match
    ~{:main (* (repeat 3 :register) "\n" :program -1)
      :register (* "Register " 1 ": " :int "\n")
      :program (* "Program: " :int (any (* "," :int)) "\n")
      :int (/ ':d+ ,scan-number)}
    input))

# This only works for the test case that is assigned to me,
# because the algorithm is specifically made for the input.
#
# The given program is equivalent to
#
# do {
# t1 <- ra & 7
# t2 <- t1 ^ 5 ^ (ra >> (t1 ^ 2))
# output t2 & 7
# ra <- ra >> 3
# } while (ra != 0)
#
# Every 3 bits in ra produce one number in output.
# The program is 16 instructions, thus ra must have 48 bits to get
# 16 numbers in the output.
# We can derive each 3 bits based on the expected output.
# Use backtracking, since some places may have multiple possiblities.
(defn solve [text]
  (label solution
    (defn recur [ra pc]
      (if (< pc 0) (return solution ra))
      (def val (text pc))
      (for t 0 8
        (def ra* (+ (blshift ra 3) t))
        (var ok false)
        (when (= val (int/to-number
                       (band 7 (bxor t 5 (brshift ra* (bxor t 2))))))
          (recur ra* (- pc 1)))))
    # Too bad Janet can only do 32-bit bitwise operations
    # on native numbers, but the target is 48 bits.
    # Therefore, using int/u64 to handle that.
    (recur (int/u64 0) (- (length text) 1))))

(let [input (parse-input (file/read stdin :all))]
  (pp input)
  (match input
    [_ _ _ & text]
    (print (solve text))))
