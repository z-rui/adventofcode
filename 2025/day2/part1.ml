open Common

let solve ranges =
  List.fold_left
    (fun acc range -> acc + sum_range (is_invalid 2) range)
    0 ranges

let () =
  parse_input stdin |> solve |> print_int;
  print_newline ()
