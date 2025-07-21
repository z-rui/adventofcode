(use ./part1)

(defn reconstruct
  ``Find locations of tiles which are on one of the best paths through the maze.``
  [cols cost-table source-pos dest-pos]

  (def rotate {(- cols) 1
               -1 (- cols)
               1 cols
               cols -1})
  (def queue @[])
  (def visited @{})
  (defn maybe-enqueue [state]
    (unless (visited state)
      (put visited state true)
      (array/push queue state)))
  # put whatever state that has minimum cost for dest-pos in queue
  (let [dest-states (sort (seq [[state cost] :pairs cost-table
                                :let [[pos dir] state]
                                :when (= pos dest-pos)]
                            [cost state]))
        min-cost (first (first dest-states))]
    (loop [[cost state] :in dest-states
           :while (= cost min-cost)]
      (maybe-enqueue state)))
  # going backwards, adding all states whose cost matches optimal cost
  (var n 0)
  (while (< n (length queue))
    (def state (queue n))
    (def cost (get cost-table state math/inf))
    (def [pos dir] state)
    (++ n)
    # option 1: keep going
    (let [pos* (- pos dir)
          cost* (- cost 1)
          state* [pos* dir]]
      (when (= cost* (cost-table state*))
        (maybe-enqueue state*)))
    # option 2: make a turn
    (let [dir* (rotate dir)
          cost* (- cost 1000)]
      (each state* [[pos dir*] [pos (- dir*)]]
        (when (= cost* (cost-table state*))
          (maybe-enqueue state*)))))
  # now discard the dir in the states and only keep the location
  (distinct (seq [[pos dir] :in queue] pos)))

(defn main [&]
  (let [[cols start-pos end-pos grid-map] (parse-input (file/read stdin :all))]
    (def cost-table (dijkstra grid-map cols start-pos))
    (def pathset (reconstruct cols cost-table start-pos end-pos))
    (print (length pathset))))
