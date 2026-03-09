open Common

let rec rotate (crosses, angle) amount =
  let angle' = angle + amount in
  let q, r = Int.(div angle' 100, rem angle' 100) in
  if amount >= 0 || angle' > 0 then (crosses + q, r)
  else
    let q' = -q + if angle = 0 then 0 else 1
    and r' = if r = 0 then 0 else r + 100 in
    (crosses + q', r')

let () =
  In_channel.(input_lines stdin)
  |> List.to_seq |> Seq.map parse_rotation
  |> Seq.fold_left rotate (0, 50)
  |> fst |> print_int;
  print_newline ()
