(put (dyn *peg-grammar*) :int ~(/ ':d+ ,scan-number))

(defmacro get-or-put [ds k dflt]
  ~(or (get ,ds ,k) (set (,ds ,k) ,dflt)))

(defn valid-input? [rules input]
  (def seen @{})
  (label ok
         (loop [number :in input
                :after (put seen number true)
                after-number :in (get rules number [])]
           (when (seen after-number)
             (return ok false)))
         true))

(defn toposort [rules numbers]
  (def in-degree (tabseq [x :in numbers] x 0))
  (loop [[before afters] :pairs rules
         :when (in in-degree before)
         after :in afters]
    (if-let [degree (in-degree after)]
      (put in-degree after (+ degree 1))))
  (def queue
    (seq [[k v] :pairs in-degree :when (= v 0)]
      k))
  (var n 0)
  (while (< n (length queue))
    (each after (get rules (queue n) [])
      (when-let [degree (in-degree after)]
        (put in-degree after (- degree 1))
        (when (= degree 1)
          (array/push queue after))))
    (++ n))
  (unless (= n (length numbers))
    (errorf "toposort not completed - expect %d, got %d - input has cycle?"
            (length numbers) n))
  queue)

(defn middle [xs]
  (get xs (div (length xs) 2)))

(defn solve [file]
  (def lines-iter (file/lines file))
  (def rules @{})
  (loop [line :in lines-iter :until (= (memcmp line "\n") 0)]
    (let [[before after] (peg/match '(* :int "|" :int) line)]
      (array/push (get-or-put rules before @[]) after)))

  (var accum 0)
  (loop [line :in lines-iter
         :let [numbers (peg/match '(* :int (any (* "," :int))) line)]]
    (if-not (valid-input? rules numbers)
      (let [sorted-numbers (toposort rules numbers)]
        (+= accum (middle sorted-numbers)))))
  accum)

(pp (solve stdin))
