(defn parse-input [input]
  (peg/match
    ~{:main (* (any :line) -1)
      :line (/ (* "p=" :int "," :int " v=" :int "," :int "\n") ,array)
      :int (/ '(* (? "-") :d+) ,scan-number)}
    input))

(defn simulate [xsize ysize state]
  (def outbuf (buffer/new-filled (* ysize (+ xsize 1))))
  (defn index-at [x y]
    (+ (* y (+ xsize 1)) x))
  (var i 0)
  (var init-outbuf nil)
  (forever
    # update the graphical representation
    (buffer/fill outbuf (chr " "))
    (for y 0 ysize
      (put outbuf (index-at xsize y) (chr "\n")))
    (each [x y dx dy] state
      (put outbuf (index-at x y) (chr "*")))
    # this is a finite state machine,
    # so it must go back to first state at some point
    (if init-outbuf
      (when (= (memcmp outbuf init-outbuf) 0)
        (printf "i = %d: back to initial state" i)
        (break))
      (set init-outbuf (buffer outbuf)))
    # what the hell does "a picture of a Christmas tree" mean?
    # need some heuristics
    (when (string/find "*********" outbuf)
      (prin outbuf)
      (print "i = " i))
    # simulate one step
    (each robot state
      (def [x y dx dy] robot)
      (-> robot
          (put 0 (mod (+ x dx) xsize))
          (put 1 (mod (+ y dy) ysize))))
    (++ i)))

(let [xsize 101
      ysize 103
      state (parse-input (file/read stdin :all))]
  (simulate xsize ysize state))
