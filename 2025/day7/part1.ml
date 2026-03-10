open Common

let solve width start_pos levels =
  let split_count = ref 0 in
  let bufs = Array.make_matrix 2 width false in
  bufs.(0).(start_pos) <- true;
  List.iteri
    (fun i split_at ->
      let active = bufs.(i mod 2) and active' = bufs.((i + 1) mod 2) in
      Array.fill active' 0 width false;
      for j = 0 to width - 1 do
        if active.(j) then
          if split_at.(j) then begin
            incr split_count;
            if j > 0 then active'.(j - 1) <- true;
            if j + 1 < width then active'.(j + 1) <- true
          end
          else active'.(j) <- true
      done)
    levels;
  !split_count

let () =
  let width, start_pos, levels = parse_input stdin in
  solve width start_pos levels |> print_int;
  print_newline ()
