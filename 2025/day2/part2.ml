open Common

let is_invalid_any s =
  try
    for n = 2 to String.length s do
      if is_invalid n s then raise Exit
    done;
    false
  with Exit -> true

let solve ranges =
  List.fold_left
    (fun acc range -> acc + sum_range is_invalid_any range)
    0 ranges

let () =
  parse_input stdin |> solve |> print_int;
  print_newline ()
