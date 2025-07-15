(def *empty* (chr "."))
(def *obstacle* (chr "#"))
(def *dir-map*
  {(chr "^") [-1 0]
   (chr "<") [0 -1]
   (chr ">") [0 1]
   (chr "v") [1 0]})
(def *dir-bit-flag*
  {[-1 0] 1
   [0 -1] 2
   [0 1] 4
   [1 0] 8})

(defn read-map [file]
  (var initial-pos nil)
  (var initial-dir nil)
  (var i 0)
  (def grid-map
    (seq [line :in (file/lines file) :after (++ i)]
      (buffer/popn line 1) # remove "\n"
      (loop [[j ch] :pairs line]
        (if-let [dir (get *dir-map* ch)]
          (do (set initial-pos [i j])
            (set initial-dir dir)
            (set [line j] (chr ".")))
          (assertf (index-of ch ".#")
                   "invalid character in input - '%c'" ch)))
      line))
  (assert initial-pos "initial location not found")
  {:initial-pos initial-pos
   :initial-dir initial-dir
   :grid-map grid-map})

(defn simulate [{:initial-pos initial-pos
                 :initial-dir initial-dir
                 :grid-map grid-map}]
  (def visited @{})
  (def n (length grid-map))
  (def m (length (first grid-map)))
  (defn advance [[i j] [di dj]] [(+ i di) (+ j dj)])
  (defn rotate [[di dj]] [dj (- di)])
  (var pos initial-pos)
  (var dir initial-dir)
  (var result :exit)
  (forever
    # prevent infinite loops...
    (def visited-flags (get visited pos 0))
    (def dir-flag (get *dir-bit-flag* dir))
    (if (= (band visited-flags dir-flag) 0)
      (put visited pos (+ visited-flags dir-flag))
      (do
        (eprintf "already visited %q with direction %q, inifite loop!"
                pos dir)
        (set result :loop)
        (break)))
    # all right, what to do now?
    (def next-pos (advance pos dir))
    (case (get-in grid-map next-pos nil)
      *obstacle* (set dir (rotate dir))
      *empty* (set pos next-pos)
      (break)))
  {result visited})

(defn main [&]
  (let [parsed-input (read-map stdin)
        result (simulate parsed-input)]
    (pp (length (result :exit)))))
