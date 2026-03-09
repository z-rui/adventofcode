let max_joltage n line =
  let len = String.length line in
  assert (len >= n);
  (* dp[i][j] := maximum joltage of length i in config[j..len-1] *)
  let dp = Array.init_matrix (n + 1) len (fun _ _ -> "") in
  for i = 1 to n do
    dp.(i).(len - i) <- String.sub line (len - i) i;
    for j = len - i - 1 downto 0 do
      let seq1 = dp.(i).(j + 1) in
      let seq2 =
        let b = Bytes.create i in
        Bytes.set b 0 line.[j];
        Bytes.blit_string dp.(i - 1).(j + 1) 0 b 1 (i - 1);
        Bytes.unsafe_to_string b
      in
      dp.(i).(j) <- max seq1 seq2
    done
  done;
  int_of_string dp.(n).(0)

let () =
  In_channel.(input_lines stdin)
  |> List.fold_left (fun acc line -> acc + max_joltage 12 line) 0
  |> print_int;
  print_newline ()
