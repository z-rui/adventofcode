open Common

let () =
  let operators, operands = parse_input stdin in
  calculate operators operands |> print_int;
  print_newline ()
