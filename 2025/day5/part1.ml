open Common

let run_queries chan ~ranges =
  let rec loop sum =
    try
      Scanf.bscanf chan "%d\n" @@ fun point ->
      if List.exists (point_in_range point) ranges then loop (sum + 1)
      else loop sum
    with End_of_file -> sum
  in
  loop 0

let () =
  let in_chan = Scanf.Scanning.stdin in
  let ranges = input_ranges in_chan in
  run_queries in_chan ~ranges |> print_int;
  print_newline ()
