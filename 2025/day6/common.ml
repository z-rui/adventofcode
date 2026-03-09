type operator = Add | Mul

let split_operators s =
  let chan = Scanf.Scanning.from_string s in
  Seq.of_dispenser (fun () ->
      Scanf.bscanf_opt chan "%c %n" @@ fun c i ->
      match c with
      | '+' -> (i, Add)
      | '*' -> (i, Mul)
      | _ -> failwith "invalid operator in input")
  |> List.of_seq

let split_line operators line =
  let n, operands =
    List.fold_left
      (fun (i, acc) (j, _) -> (j, String.sub line i (j - i) :: acc))
      (0, []) operators
  in
  assert (n = String.length line);
  List.rev operands

let[@tail_mod_cons] rec zip lists =
  if List.(for_all is_empty lists) then []
  else List.(map hd lists :: zip (map tl lists))

let parse_input chan =
  match List.rev (In_channel.input_lines chan) with
  | operators :: lines ->
      let operators' = split_operators operators in
      let lines' = List.rev_map (split_line operators') lines in
      (operators', zip lines')
  | [] -> failwith "empty input"

let calculate operators operands =
  List.fold_left2
    (fun acc (_, op) nums ->
      let nums' = List.map (Fun.compose int_of_string String.trim) nums in
      let x =
        match op with
        | Add -> List.fold_left ( + ) 0 nums'
        | Mul -> List.fold_left ( * ) 1 nums'
      in
      acc + x)
    0 operators operands
