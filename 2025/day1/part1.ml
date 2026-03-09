open Common

let () =
  In_channel.(input_lines stdin)
  |> List.to_seq |> Seq.map parse_rotation
  |> Seq.scan (fun x y -> (x + y + 100) mod 100) 50
  |> Seq.filter (Int.equal 0)
  |> Seq.length |> print_int;
  print_newline ()
