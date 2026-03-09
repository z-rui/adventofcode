open Common

let count_reachable rows cols state neighbors =
  let cnt = ref 0 in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      if is_reachable state neighbors i j then incr cnt
    done
  done;
  !cnt

let () =
  let rows, cols, state = parse_input stdin in
  let neighbors = calculate_neighbors rows cols state in
  count_reachable rows cols state neighbors |> print_int;
  print_newline ()
