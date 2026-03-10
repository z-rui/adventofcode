open Common

let solve { indicators; buttons } =
  let n = Array.length buttons in
  let min_pop_cnt = ref n in
  (* n <= 13, brute force is fine *)
  for combination = 0 to Bitset.all n do
    let result = ref Bitset.empty in
    let popcnt = ref 0 in
    for i = 0 to n - 1 do
      if Bitset.test combination i then begin
        result := !result lxor buttons.(i);
        incr popcnt
      end
    done;
    if !result = indicators && !popcnt < !min_pop_cnt then begin
      min_pop_cnt := !popcnt
    end
  done;
  !min_pop_cnt

let () =
  In_channel.(input_lines stdin)
  |> List.fold_left (fun acc line -> acc + solve (parse_line line)) 0
  |> print_int;
  print_newline ()
