(* Start with brute force, until it turns out to be too slow. *)

let repeat s n =
  let len = String.length s in
  let b = Bytes.create (len * n) in
  for i = 0 to n - 1 do
    Bytes.blit_string s 0 b (i * len) len
  done;
  Bytes.unsafe_to_string b

let is_invalid n s =
  let len = String.length s in
  if len mod n <> 0 then false
  else
    let seg_len = len / n in
    let seg = String.sub s 0 seg_len in
    s = repeat seg n

let parse_range s =
  match String.split_on_char '-' s with
  | [ first; last ] -> (first, last)
  | _ -> failwith "parse_range"

let sum_range pred (first, last) =
  let sum = ref 0 in
  for i = int_of_string first to int_of_string last do
    if pred (Int.to_string i) then sum := !sum + i
  done;
  !sum

let parse_line s =
  List.filter_map
    (fun s -> if s = "" then None else Some (parse_range s))
    (String.split_on_char ',' s)

let parse_input chan = In_channel.input_lines chan |> List.concat_map parse_line
