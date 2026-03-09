let parse_rotation s =
  let x = int_of_string @@ String.sub s 1 (String.length s - 1) in
  match s.[0] with 'L' -> -x | 'R' -> +x | _ -> failwith "parse_rotation"
