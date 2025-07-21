(defmacro- swap! [x y]
  ~(do
     (def tmp ,x)
     (set ,x ,y)
     (set ,y tmp)))

(defn push [heap value]
  (array/push heap value)
  (var i (- (length heap) 1))
  (while (> i 0)
    (def p (div (- i 1) 2))
    (if (< (heap i) (heap p))
      (do
        (swap! (heap i) (heap p))
        (set i p))
      (break))))

(defn pop [heap]
  (def first-val (first heap))
  (def last-val (array/pop heap))
  (if (empty? heap) (break first-val))
  (put heap 0 last-val)

  (var i 0)
  (var n (length heap))
  (forever
    (def l (+ (* 2 i) 1))
    (def r (+ l 1))
    (var smallest i)
    (when (and (< l n) (< (heap l) (heap smallest)))
      (set smallest l))
    (when (and (< r n) (< (heap r) (heap smallest)))
      (set smallest r))
    (if (= smallest i)
      (break))
    (swap! (heap i) (heap smallest))
    (set i smallest))
  first-val)

(let [heap @[]
      out @[]]
  (each val [3 7 2 6 5 8]
    (push heap val))
  (while (not (empty? heap))
    (array/push out (pop heap)))
  (assert (deep= out @[2 3 5 6 7 8])))
