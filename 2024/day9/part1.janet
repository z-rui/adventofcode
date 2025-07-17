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

(defn compact [disk]
  (def out @[])
  (defn fill-gap [gap-size]
    (if (= gap-size 0) (break))  # no gap!
    (def last-file (array/pop disk))
    (def [file-id file-size] last-file)
    (assert file-id)
    (if (<= file-size gap-size)
      (do  # file fits in gap
        (array/push out last-file)
        (let [last-gap (array/pop disk)]
          (assert (= (in last-gap 0) nil)))
        (fill-gap (- gap-size file-size)))
      (do  # move part of the file into the gap
        (array/push out [file-id gap-size])
        (array/push disk [file-id (- file-size gap-size)]))))
  (each entry disk
    (def [file-id size] entry)
    (if file-id
      (array/push out entry)
      (fill-gap size)))
  out)

(defn linear-sum [start n]
  return (/ (* (+ start start n -1) n) 2))

(assert (= (linear-sum 1 100) 5050))

(defn checksum [disk]
  (var accum 0)
  (var curr-loc 0)
  (each entry disk
    (def [file-id size] entry)
    (assert file-id)
    (+= accum (* file-id
                 (linear-sum curr-loc size)))
    (+= curr-loc size))
  accum)

(defn solve [diskmap]
  (def disk (expand-diskmap diskmap))
  (def compacted-disk (compact disk))
  (checksum compacted-disk))

(let [diskmap "2333133121414131402"]
  (assert (= (solve diskmap) 1928)))

(defn main [&]
  (def diskmap (file/read stdin :all))
  (if (= (last diskmap) (chr "\n"))
    (buffer/popn diskmap 1))
  (print (solve diskmap)))
