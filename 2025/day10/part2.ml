open Common

(* This part is surprisingly difficult until one finds a handy linear
   programming solver *)
let solve { length; buttons; joltages } =
  let n = Array.length buttons in
  let vars =
    Array.init n (fun i -> Lp.var ~integer:true ("x" ^ Int.to_string i))
  in
  let constraints =
    List.init length @@ fun i ->
    let lhs = ref Lp.zero in
    for j = 0 to n - 1 do
      if Bitset.test buttons.(j) i then lhs := Lp.(!lhs ++ vars.(j))
    done;
    let rhs = Lp.c (Float.of_int joltages.(i)) in
    Lp.(!lhs =~ rhs)
  in
  let sum = Array.fold_left Lp.( ++ ) Lp.zero vars in
  let prob = Lp.(make (minimize sum) constraints) in
  match Lp_glpk.solve ~term_output:false prob with
  | Ok (min_sum, _) -> min_sum
  | Error error -> failwith error

let () =
  In_channel.(input_lines stdin)
  |> List.fold_left (fun acc line -> acc +. solve (parse_line line)) 0.
  |> print_float;
  print_newline ()
