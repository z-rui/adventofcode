(use ./part1)

(defn preprocess [[a b c d x y]]
  [a b c d (+ x 10000000000000) (+ y 10000000000000)])

(defn main [&]
  (->> (file/read stdin :all)
       (parse-input)
       (map preprocess)
       (solve)
       (print)))
