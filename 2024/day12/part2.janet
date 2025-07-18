(def [*up* *left* *right* *down*] [[-1 0] [0 -1] [0 1] [1 0]])

(defn v+ [[a b] [c d]] [(+ a c) (+ b d)])

(defn connected-components [grid-map]
  (def rows (length grid-map))
  (def cols (length (first grid-map)))
  (defn valid-pos? [[i j]]
    (and (<= 0 i) (< i rows) (<= 0 j) (< j cols)))
  (def visited? (seq [:repeat rows]
                  (array/new-filled cols false)))
  (defn connected-component [at]
    (def sym (get-in grid-map at))
    (def queue @[at])
    (put-in visited? at true)
    (var n 0)
    (def edges @[])
    (while (< n (length queue))
      (def pos (in queue n))
      (++ n)
      (each dir [*up* *left* *right* *down*]
        (def next-pos (v+ pos dir))
        (if (and (valid-pos? next-pos)
                 (= sym (get-in grid-map next-pos)))
          # at dir is a neighbor
          (unless (get-in visited? next-pos)
            (put-in visited? next-pos true)
            (array/push queue next-pos))
          # at dir is a boundary
          (array/push edges [dir pos]))))
    {:sym (string/from-bytes sym)
     :locations queue
     :area (length queue)
     :edges edges})
  (def accum @[])
  (for i 0 rows
    (for j 0 cols
      (unless (-> visited? (in i) (in j))
        (array/push accum (connected-component [i j])))))
  accum)

(defn combine-edges [edges]
  (def dir-edges @{})
  (each [dir pos] edges
    (def edges-at-dir (or (get dir-edges dir)
                          (set (dir-edges dir) @[])))
    (array/push edges-at-dir pos))
  (eachp [dir edges] dir-edges
    (if (or (= dir *left*) (= dir *right*))
      # vertical edge - sort by j then i
      (sort-by (fn [[i j]] [j i]) edges)
      # horizontal edge - sort by i then j
      (sort edges)))
  (eachp [dir edges] dir-edges
    (def [di dj] (get {*up* *right*
                       *down* *right*
                       *left* *down*
                       *right* *down*}
                      dir))
    (var edge-count (length edges))
    (var out-count 0)
    (var scan-count 0)
    (while (< scan-count edge-count)
      (def start-pos (in edges scan-count))
      (var [i j] start-pos)
      (var k (+ scan-count 1))
      (while (< k edge-count)
        (def [i* j*] (in edges k))
        (+= i di)
        (+= j dj)
        (if (and (= i i*) (= j j*))
          (++ k)
          (break)))
      (set (edges out-count) [start-pos (- k scan-count)])
      (++ out-count)
      (set scan-count k))
    (array/remove edges out-count (- edge-count out-count)))
  dir-edges)

(defn count-edges [dir-edges]
  (var accum 0)
  (loop [edges :in dir-edges]
    (+= accum (length edges)))
  accum)

(defn solve [grid-map]
  (var price 0)
  (each component (connected-components grid-map)
    (def dir-edges (combine-edges (component :edges)))
    (let [edge-count (count-edges dir-edges)
          area (component :area)]
      (+= price (* edge-count area))))
  price)

(let [input (seq [line :in (file/lines stdin)]
              (buffer/popn line 1))]
  (print (solve input)))
