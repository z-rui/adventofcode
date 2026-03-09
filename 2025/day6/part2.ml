open Common

let split_chars s = String.to_seq s |> List.of_seq
let combine_chars cs = List.to_seq cs |> String.of_seq
let all_spaces s = String.for_all (Char.equal ' ') s

let rotate_operands operands =
  List.map
    (fun nums ->
      List.map split_chars nums |> zip
      |> List.filter_map (fun cs ->
          let s = combine_chars cs in
          if all_spaces s then None else Some s))
    operands

let () =
  let operators, operands = parse_input stdin in
  calculate operators (rotate_operands operands) |> print_int;
  print_newline ()
