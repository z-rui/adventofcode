(use ./part1)

(defn find-max-clique [{:adj adj :adj-sets adj-sets :deg* deg* :names names}]
  (def N (length adj))
  (var max-clique-size 0)
  (var max-clique nil)
  (def workset @[])
  (defn recur [id]
    (def workset-size (length workset))
    (when (> workset-size max-clique-size)
      (set max-clique-size workset-size)
      (set max-clique (string/join (sort (map names workset)) ",")))
    (for next-id id N
      (when (and (>= (+ workset-size (deg* id)) max-clique-size)
                 (all (partial buffer/bit (adj-sets next-id)) workset))
        (array/push workset next-id)
        (recur (+ next-id 1))
        (array/pop workset))))
  (recur 0)
  max-clique)

(defn main [&]
  (let [G (read-graph (file/lines stdin))]
    (print (find-max-clique G))))
