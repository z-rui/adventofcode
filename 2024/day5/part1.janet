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
         :let [numbers (peg/match '(* :int (any (* "," :int))) line)]
         :when (valid-input? rules numbers)]
    (+= accum (middle numbers)))
  accum)

(pp (solve stdin))
