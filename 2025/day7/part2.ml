open Common

let solve width start_pos levels =
  let bufs = Array.make_matrix 2 width 0 in
  bufs.(0).(start_pos) <- 1;
  List.iteri
    (fun i split_at ->
      let count = bufs.(i mod 2) and count' = bufs.((i + 1) mod 2) in
      Array.fill count' 0 width 0;
      for j = 0 to width - 1 do
        let this_count = count.(j) in
        if this_count > 0 then
          if split_at.(j) then begin
            if j > 0 then count'.(j - 1) <- count'.(j - 1) + this_count;
            if j + 1 < width then count'.(j + 1) <- count'.(j + 1) + this_count
          end
          else count'.(j) <- count'.(j) + this_count
      done)
    levels;
  let count = bufs.(List.length levels mod 2) in
  Array.fold_left ( + ) 0 count

let () =
  let width, start_pos, levels = parse_input stdin in
  solve width start_pos levels |> print_int;
  print_newline ()
