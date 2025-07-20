(defn parse-input [input]
  (var robot-pos nil)
  (def [grid-map move-seq]
    (peg/match
      ~{:main (* :grid-map "\n" :move-seq -1)
        :grid-map (/ (some (* (/ '(some (set "#.O@")) ,buffer) "\n")) ,tuple)
        :move-seq (/ '(some (set "<>^v\n"))
                     ,|(string/replace-all "\n" "" $))}
      input))
  (eachp [i row] grid-map
    (if-let [j (string/find "@" row)]
      (set robot-pos [i j])))
  [grid-map move-seq robot-pos])

(defn show-map [grid-map]
  (each row grid-map
    (print row)))

(defn simulate [grid-map move-seq [i j]]
  (def ch-to-dir {(chr "^") [-1 0]
                  (chr "<") [0 -1]
                  (chr ">") [0 1]
                  (chr "v") [1 0]})
  (var [i j] [i j])
  (each move-ch move-seq
    (def [di dj] (ch-to-dir move-ch))
    (var [i* j*] [i j])
    (forever
      (+= i* di) (+= j* dj)
      # no need to do boundary check,
      # because it's always '#' at boundary.
      (when (not= ((grid-map i*) j*) (chr "O"))
        (break)))
    (case ((grid-map i*) j*)
      (chr ".") (do
                  (set ((grid-map i) j) (chr "."))
                  (+= i di) (+= j dj)
                  (set ((grid-map i) j) (chr "@"))
                  (unless (and (= i i*) (= j j*))
                    # moved a bunch of boxes
                    (set ((grid-map i*) j*) (chr "O"))))
      (chr "#") (do)
      (errorf "unexpected symbol %c at location (%d %d)"
              ((grid-map i*) j*) i* j*)))
  [i j])

(defn score [grid-map]
  (var accum 0)
  (loop [[i row] :pairs grid-map
         [j ch] :pairs row
         :when (= ch (chr "O"))]
    (+= accum (+ (* i 100) j)))
  accum)

(let [[grid-map move-seq pos] (parse-input (file/read stdin :all))]
  (show-map grid-map)
  (pp pos)
  (def pos (simulate grid-map move-seq pos))
  (show-map grid-map)
  (pp pos)
  (print (score grid-map)))
