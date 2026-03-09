type cell = Empty | Roll

let get_columns state =
  let cols = Array.length state.(0) in
  Array.iter (fun row -> assert (Array.length row = cols)) state;
  cols

let parse_input chan =
  let lines = In_channel.input_lines chan in
  let state =
    Array.map
      (fun line ->
        Array.init (String.length line) @@ fun i ->
        match line.[i] with
        | '.' -> Empty
        | '@' -> Roll
        | _ -> failwith "invalid character in input")
      (Array.of_list lines)
  in
  (Array.length state, get_columns state, state)

let calculate_neighbors rows cols state =
  let chk i j =
    Bool.to_int
      (i >= 0 && i < rows && j >= 0 && j < cols && state.(i).(j) = Roll)
  in
  Array.init_matrix rows cols @@ fun i j ->
  chk (i - 1) (j - 1)
  + chk (i - 1) j
  + chk (i - 1) (j + 1)
  + chk i (j - 1)
  + chk i (j + 1)
  + chk (i + 1) (j - 1)
  + chk (i + 1) j
  + chk (i + 1) (j + 1)

let is_reachable state neighbors i j =
  state.(i).(j) = Roll && neighbors.(i).(j) < 4
