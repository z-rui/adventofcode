open Common

let merge_ranges ranges =
  List.fold_left
    (fun acc ((a, b) as x) ->
      match acc with
      | (c, d) :: rest when a <= d -> if b <= d then acc else (c, b) :: rest
      | _ -> x :: acc)
    []
    (List.sort (fun (a, _) (b, _) -> Int.compare a b) ranges)

let count_total ranges =
  List.fold_left (fun acc (a, b) -> acc + (b - a + 1)) 0 ranges

let () =
  input_ranges Scanf.Scanning.stdin |> merge_ranges |> count_total |> print_int;
  print_newline ()
