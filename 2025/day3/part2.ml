let max_joltage n line =
  let len = String.length line in
  assert (len >= n);
  (* After i-th iteration:
    dp.(j) is maximum joltage of length i in line.[j..len-1] *)
  let dp = Array.init len (Fun.const "") in
  for i = 1 to n do
    let prev = ref dp.(len - i) in
    dp.(len - i) <- String.sub line (len - i) i;
    for j = len - i - 1 downto 0 do
      let seq1 = dp.(j + 1) in
      let seq2 = String.make 1 line.[j] ^ !prev in
      prev := dp.(j);
      dp.(j) <- max seq1 seq2
    done
  done;
  int_of_string dp.(0)

let () =
  In_channel.(input_lines stdin)
  |> List.fold_left (fun acc line -> acc + max_joltage 12 line) 0
  |> print_int;
  print_newline ()
