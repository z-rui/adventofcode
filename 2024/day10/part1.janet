(defn generate-digit-locations [topo-map]
  (def digit-locations (seq [:repeat 10] @[]))
  (loop [[i row] :pairs topo-map
         [j digit] :pairs row
         :when digit]
    (array/push (digit-locations digit) [i j]))
  digit-locations)

(defn solve [topo-map]
  (def rows (length topo-map))
  (def cols (length (first topo-map)))
  (defn valid-pos? [i j]
    (and (<= 0 i) (< i rows)
         (<= 0 j) (< j cols)))
  (def digit-locations (generate-digit-locations topo-map))
  (def reachables
    (seq [:repeat rows] (seq [:repeat cols] @{})))
  (each [i j] (in digit-locations 9)
    (set ((-> reachables (in i) (in j)) [i j]) true))
  (loop [digit :down-to [9 1]
         [i j] :in (digit-locations digit)
         :let [curr-set (-> reachables (in i) (in j))]
         [di dj] :in [[-1 0] [0 -1] [0 1] [1 0]]
         :let [i* (+ i di)
               j* (+ j dj)]
         :when (and (valid-pos? i* j*)
                    (= (-> topo-map (in i*) (in j*))
                       (- digit 1)))]
    (merge-into (-> reachables (in i*) (in j*)) curr-set))
  (sum (seq [[i j] :in (in digit-locations 0)]
         (length (-> reachables (in i) (in j))))))

(defn parse-input [input]
  (peg/match
    ~{:digit (/ ':d ,scan-number)
      :line (* (/ (some :digit) ,tuple) "\n")
      :main (some :line)}
    input))

(def *test-input*
  `
  89010123
  78121874
  87430965
  96549874
  45678903
  32019012
  01329801
  10456732

  `)

(assert (= (solve (parse-input *test-input*)) 36))

(print (solve (parse-input (file/read stdin :all))))
