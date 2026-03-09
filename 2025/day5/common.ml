let[@tail_mod_cons] rec input_ranges chan =
  match Scanf.bscanf_opt chan "%d-%d\n" Pair.make with
  | Some x -> x :: input_ranges chan
  | None ->
      Scanf.bscanf chan "\n" ();
      []

let point_in_range point (a, b) = a <= point && point <= b
