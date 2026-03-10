let collect_indices c s =
  let rec loop acc i =
    if i = 0 then acc
    else
      let i' = i - 1 in
      match s.[i'] with
      | '.' -> loop acc i'
      | c' when c' = c -> loop (i' :: acc) i'
      | _ -> failwith "invalid character"
  in
  loop [] (String.length s)

let parse_first_line line =
  match collect_indices 'S' line with
  | [ start_pos ] -> (String.length line, start_pos)
  | _ -> failwith "invalid starting position"

let indicies_to_bools len indicies =
  let arr = Array.init len (Fun.const false) in
  List.iter (fun i -> arr.(i) <- true) indicies;
  arr

let parse_other_lines lines =
  List.filter_map
    (fun line ->
      match collect_indices '^' line with
      | [] -> None
      | indicies -> Some (indicies_to_bools (String.length line) indicies))
    lines

let parse_input chan =
  match In_channel.input_lines chan with
  | first :: rest ->
      let width, start_pos = parse_first_line first in
      let levels = parse_other_lines rest in
      (width, start_pos, levels)
  | [] -> failwith "empty input"
