(defn read-graph [lines]
  (def name-map @{})
  (def names @[])
  (def adj @[])
  (def deg @[])
  (def deg* @[])
  (defn lookup [name]
    (or
      (name-map name)
      (do
        (array/push names name)
        (array/push adj @[])
        (array/push deg 0)
        (array/push deg* 0)
        (set (name-map name) (length name-map)))))
  (each line lines
    (def [from-name to-name]
      (peg/match '(* '(repeat 2 (range "az"))
                     "-"
                     '(repeat 2 (range "az")))
                 line))
    (def from-id (lookup from-name))
    (def to-id (lookup to-name))
    (array/push (adj from-id) to-id)
    (array/push (adj to-id) from-id)
    (++ (deg from-id))
    (++ (deg to-id))
    (let [min-id (min from-id to-id)]
      (++ (deg* min-id))))
  (def size (length name-map))
  (def adj-set-size (math/ceil (/ size 8)))
  (def adj-sets (array/new size))
  (eachp [i adj-list] adj
    (def adj-set (buffer/new-filled adj-set-size))
    (each j adj-list
      (buffer/bit-set adj-set j))
    (array/push adj-sets adj-set))
  {:size size
   :name-map name-map
   :names names
   :adj adj
   :adj-sets adj-sets
   :deg deg
   :deg* deg*})

(defn count-3-groups [{:adj adj :adj-sets adj-sets} root-set]
  (var accum 0)
  (loop [x :keys root-set
         :let [x-adj (adj x)
               skip? |(and (in root-set $) (< $ x))]
         [i y] :pairs x-adj
         :unless (skip? y)
         j :range [(+ i 1) (length x-adj)]
         :let [z (x-adj j)]
         :when (and (buffer/bit (adj-sets y) z) (not (skip? z)))]
    (++ accum))
  accum)

(defn main [&]
  (let [G (read-graph (file/lines stdin))
        root-set (tabseq [[i name] :pairs (G :names)
                          :when (= (first name) (chr "t"))]
                   i true)]
    (print (count-3-groups G root-set))))
