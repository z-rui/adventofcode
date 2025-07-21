(defn parse-input [input]
  (peg/match
    ~{:main (* (any :row) -1)
      :row (/ (* :int "," :int "\n") ,tuple)
      :int (/ ':d+ ,scan-number)}
    input))

(defn shortest-path-length [obstacles rows cols]
  (def obstacle? (tabseq [pos :in obstacles] pos true))
  (def dest-pos [(dec rows) (dec cols)])
  (var answer nil)
  (def queue @[[0 [0 0]]])
  (def visited? @{[0 0] true})
  (var n 0)
  (while (< n (length queue))
    (def [steps pos] (queue n))
    (++ n)
    (when (= pos dest-pos)
      (set answer steps)
      (break))
    (def [i j] pos)
    (each [di dj] [[-1 0] [0 -1] [0 1] [1 0]]
      (def [i* j*] [(+ i di) (+ j dj)])
      (when (and (<= 0 i*) (< i* rows)
                 (<= 0 j*) (< j* cols))
        (def pos* [i* j*])
        (unless (or (visited? pos*) (obstacle? pos*))
          (put visited? pos* true)
          (array/push queue [(+ steps 1) pos*])))))
  answer)

(defn main [&]
  (let [input (parse-input (file/read stdin :all))
        [rows cols] [71 71]]
    (def input (array/slice input 0 1024))
    (print (shortest-path-length input rows cols))))
