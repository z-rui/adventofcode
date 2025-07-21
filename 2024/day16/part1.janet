(import ./heap)

(defn parse-input [input]
  (def buf @"")
  (var start-pos nil)
  (var end-pos nil)
  (var cols nil)
  (var idx 0)
  (each ch input
    (if (= ch (chr "\n"))
      (if cols
        (assert (= (mod idx cols) 0))
        (set cols idx))
      (do
        (case ch
          (chr "S") (do
                      (set start-pos idx)
                      (buffer/push buf (chr ".")))
          (chr "E") (do 
                      (set end-pos idx)
                      (buffer/push buf (chr ".")))
          (buffer/push buf ch))
        (++ idx))))
  [cols start-pos end-pos buf])

(defn dijkstra [grid-map cols source-pos]
  (def rotate {(- cols) 1
               -1 (- cols)
               1 cols
               cols -1})
  (def cost-table @{})
  (def queue @[])
  (defn maybe-enqueue [cost pos dir]
    (def state [pos dir])
    (when (and
            (= (grid-map pos) (chr "."))
            (< cost (get cost-table state math/inf)))
      (put cost-table state cost)
      (heap/push queue [cost state])))
  # push initial state
  (maybe-enqueue 0 source-pos 1)
  (while (not (empty? queue))
    (def [cost state] (heap/pop queue))
    (def [pos dir] state)
    # option 1: keep going
    (let [pos* (+ pos dir)
          cost* (+ cost 1)]
      (maybe-enqueue cost* pos* dir))
    # option 2: make a turn
    (let [dir* (rotate dir)
          cost* (+ cost 1000)]
      (maybe-enqueue cost* pos dir*)
      (maybe-enqueue cost* pos (- dir*))))
  cost-table)

(defn final-score [cost-table dest-pos]
  (min-of (seq [[state cost] :pairs cost-table
                :let [[pos dir] state]
                :when (= pos dest-pos)]
            cost)))

(defn main [&]
  (let [[cols start-pos end-pos grid-map] (parse-input (file/read stdin :all))]
    (def cost-table (dijkstra grid-map cols start-pos))
    (print (final-score cost-table end-pos))))
