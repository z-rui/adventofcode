open Common

let count_reachable_once rows cols state neighbors =
  let cnt = ref 0 in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      if is_reachable state neighbors i j then begin
        incr cnt;
        state.(i).(j) <- Empty;
        let upd i j =
          if i >= 0 && i < rows && j >= 0 && j < cols then
            neighbors.(i).(j) <- neighbors.(i).(j) - 1
        in
        upd (i - 1) (j - 1);
        upd (i - 1) j;
        upd (i - 1) (j + 1);
        upd i (j - 1);
        upd i (j + 1);
        upd (i + 1) (j - 1);
        upd (i + 1) j;
        upd (i + 1) (j + 1)
      end
    done
  done;
  !cnt

let count_reachable_all rows cols state neighbors =
  Seq.of_dispenser (fun () ->
      match count_reachable_once rows cols state neighbors with
      | 0 -> None
      | n -> Some n)
  |> Seq.fold_left ( + ) 0

let () =
  let rows, cols, state = parse_input stdin in
  let neighbors = calculate_neighbors rows cols state in
  count_reachable_all rows cols state neighbors |> print_int;
  print_newline ()
