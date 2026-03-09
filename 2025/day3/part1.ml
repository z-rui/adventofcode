let parse_input chan =
  In_channel.input_lines chan
  |> List.map @@ fun line ->
     let len = String.length line in
     Array.init len @@ fun i -> Char.code line.[i] - Char.code '0'

let max_subarray arr first last =
  let rec loop m midx i =
    if i > last then (m, midx)
    else
      let x = arr.(i) in
      if m < x then loop x i (i + 1) else loop m midx (i + 1)
  in
  loop arr.(first) first first

let max_joltage config =
  let len = Array.length config in
  let a, i = max_subarray config 0 (len - 2) in
  let b, _ = max_subarray config (i + 1) (len - 1) in
  (10 * a) + b

let () =
  parse_input stdin
  |> List.fold_left (fun acc config -> acc + max_joltage config) 0
  |> print_int;
  print_newline ()
