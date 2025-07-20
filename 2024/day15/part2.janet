(defn parse-input [input]
  (defn expand-row [row]
    (def buf (buffer/new (* (length row) 2)))
    (each ch row
      (buffer/push buf
                   (case ch
                     (chr "#") "##"
                     (chr "O") "[]"
                     (chr ".") ".."
                     (chr "@") "@.")))
    buf)
  (var robot-pos nil)
  (def [grid-map move-seq]
    (peg/match
      ~{:main (* :grid-map "\n" :move-seq -1)
        :grid-map (/ (some :grid-row) ,tuple)
        :grid-row (* (/ '(some (set "#.O@")) ,expand-row) "\n")
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

(defn simulate [grid-map move-seq pos]
  (def rows (length grid-map))
  (def cols (length (first grid-map)))
  (def ch-to-dir {(chr "^") (- cols)
                  (chr "<") -1
                  (chr ">") 1
                  (chr "v") cols})
  (def total-grids (* rows cols))
  (def visited-bitmap (buffer/new-filled total-grids))
  (def queue (array/new total-grids))
  (defn pos->index [i j] (+ (* i cols) j))
  (defn index->pos [idx] [(div idx cols) (mod idx cols)])
  (defn maybe-enqueue [idx]
    (unless (buffer/bit visited-bitmap idx)
      (buffer/bit-set visited-bitmap idx)
      (array/push queue idx)))
  (var curr-pos-idx (pos->index ;pos))
  (each move-ch move-seq
    (def dir (ch-to-dir move-ch))
    # get the locations of the set of boxes that are pushed (BFS).
    (buffer/fill visited-bitmap 0)
    (array/clear queue)
    (array/push queue curr-pos-idx)
    (var ok true)
    (var n 0)
    (while (< n (length queue))
      (def pos-idx (queue n))
      (++ n)
      (def next-pos-idx (+ pos-idx dir))
      (def [i j] (index->pos next-pos-idx))
      (case ((grid-map i) j)
        (chr "#") (do
                    # pushing at this direction will hit a wall.
                    # will do nothing.
                    (set ok false)
                    (break))
        (chr ".") nil
        (chr "[") (do
                    (maybe-enqueue next-pos-idx)
                    (maybe-enqueue (+ next-pos-idx 1)))
        (chr "]") (do
                    (maybe-enqueue next-pos-idx)
                    (maybe-enqueue (- next-pos-idx 1)))
        (errorf "bad symbol %c at (%d %d)"
                ((grid-map i) j) i j)))
    (when ok
      # if not hitting a wall, modify the map in place.
      # this needs to be done in reverse order to avoid
      # clobbering.
      (loop [k :down-to [(- (length queue) 1) 0]
             :let [pos-idx (queue k)
                   [i j] (index->pos pos-idx)
                   [i* j*] (index->pos (+ pos-idx dir))]]
        (set ((grid-map i*) j*) ((grid-map i) j))
        (set ((grid-map i) j) (chr ".")))
      (+= curr-pos-idx dir))
    (comment
      (printf "Move %c:" move-ch)
      (show-map grid-map)
      (print)))
  (index->pos curr-pos-idx))

(defn score [grid-map]
  (var accum 0)
  (loop [[i row] :pairs grid-map
         [j ch] :pairs row
         :when (= ch (chr "["))]
    (+= accum (+ (* i 100) j)))
  accum)

(let [[grid-map move-seq pos] (parse-input (file/read stdin :all))]
  (show-map grid-map)
  (pp pos)
  (def pos (simulate grid-map move-seq pos))
  (show-map grid-map)
  (pp pos)
  (print (score grid-map)))
