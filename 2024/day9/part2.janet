(defn expand-diskmap [diskmap]
  (def disk @[])
  (var is-gap false)
  (var curr-offset 0)
  (var file-id 0)

  (each ch diskmap
    (def digit (- ch (chr "0")))
    (assert (<= 0 digit 9))
    # trying to be clever by doing a run-length-encoding
    (array/push disk [(if is-gap nil file-id) digit])
    (if-not is-gap (++ file-id))
    (set is-gap (not is-gap)))
  disk)

# This becomes O(n^2) now, but still within 2sec on my machine.
(defn compact [disk]
  (def out @[])
  (defn fill-gap [gap-size idx]
    (var gap-size gap-size)
    (loop [i :down [(- (length disk) 1) idx]
           :until (= gap-size 0)
           :let [file (in disk i)
                 [file-id file-size] file]
           :when (not= file-id nil)]
      (when (<= file-size gap-size)
        (array/push out file)
        # mark old file location as gap 
        (put disk i [nil file-size])
        (-= gap-size file-size)))
    (if (not= gap-size 0)
      (array/push out [nil gap-size])))
  (eachp [idx entry] disk
    (def [file-id size] entry)
    (if file-id
      (array/push out entry)
      (fill-gap size idx)))
  out)

(defn linear-sum [start n]
  return (/ (* (+ start start n -1) n) 2))

(assert (= (linear-sum 1 100) 5050))

(defn checksum [disk]
  (var accum 0)
  (var curr-loc 0)
  (loop [entry :in disk
         :let [[file-id size] entry]
         :after (+= curr-loc size)
         :when (not= file-id nil)]
    (+= accum (* file-id
                 (linear-sum curr-loc size))))
  accum)

(defn solve [diskmap]
  (def disk (expand-diskmap diskmap))
  (def compacted-disk (compact disk))
  (checksum compacted-disk))

(let [diskmap "2333133121414131402"]
  (assert (= (solve diskmap) 2858) ))

(defn main [&]
  (def diskmap (file/read stdin :all))
  (if (= (last diskmap) (chr "\n"))
    (buffer/popn diskmap 1))
  (print (solve diskmap)))
