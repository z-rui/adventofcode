(defn connected-components [grid-map]
  (def rows (length grid-map))
  (def cols (length (first grid-map)))
  (defn valid-pos? [i j]
    (and (<= 0 i) (< i rows) (<= 0 j) (< j cols)))
  (def visited? (seq [:repeat rows]
                  (array/new-filled cols false)))
  (defn connected-component [at]
    (def sym (get-in grid-map at))
    (def queue @[at])
    (put-in visited? at true)
    (var n 0)
    (var perimeter 0)
    (while (< n (length queue))
      (def [i j] (in queue n))
      (++ n)
      (+= perimeter 4)
      (each [di dj] [[-1 0] [0 -1] [0 1] [1 0]]
        (def [i* j*] [(+ i di) (+ j dj)])
        (when (and (valid-pos? i* j*)
                   (= sym (-> grid-map (in i*) (in j*))))
          (-- perimeter)
          (unless (-> visited? (in i*) (in j*))
            (set ((visited? i*) j*) true)
            (array/push queue [i* j*])))))
    {:sym (string/from-bytes sym)
     :locations queue
     :area (length queue)
     :perimeter perimeter})
  (def accum @[])
  (for i 0 rows
    (for j 0 cols
      (unless (-> visited? (in i) (in j))
        (array/push accum (connected-component [i j])))))
  accum)

(defn solve [grid-map]
  (var price 0)
  (each component (connected-components grid-map)
    (+= price (* (component :perimeter) (component :area))))
  price)

(let [input (seq [line :in (file/lines stdin)]
              (buffer/popn line 1))]
  (print (solve input)))
