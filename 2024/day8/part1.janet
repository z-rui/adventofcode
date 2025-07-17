(defn uniq [arr]
  (def n (length arr))
  (when (= n 0) (break arr))
  (var i 1)
  (for j 1 n
    (def x (in arr j))
    (unless (= x (in arr (- j 1)))
      (put arr i x)
      (++ i)))
  (array/remove arr i (- n i)))

(defn compute-antinodes [rows cols antenna-groups]
  (def antinodes @[])
  (defn valid-pos? [[i j]]
    (and (<= 0 i) (< i rows)
         (<= 0 j) (< j cols)))
  (each antenna-group antenna-groups
    (loop [from :in antenna-group
           to :in antenna-group]
      (when (not= from to)
        (def delta (map - to from))
        (def new-antinodes (filter valid-pos?
                                   (map tuple/slice [(map + to delta)
                                                     (map - from delta)])))
        (array/concat antinodes new-antinodes))))
  (sort antinodes)
  (uniq antinodes))

(defn parse-input [file]
  (var [rows cols] [0 nil])
  (def antenna-groups @{})
  (loop [line :in (file/lines file) :after (++ rows)]
    (eachp [i ch] line
      (case ch
        (chr ".") nil
        (chr "\n") (if (= cols nil)
                     (set cols i)
                     (assert (= cols i)))
        (let [locations (or (get antenna-groups ch)
                            (set (antenna-groups ch) @[]))]
          (array/push locations [rows i])))))
  [rows cols (values antenna-groups)])

(let [input (parse-input stdin)
      result (compute-antinodes ;input)]
  (print (length result)))
