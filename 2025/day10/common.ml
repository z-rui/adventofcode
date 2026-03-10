module Bitset = struct
  type t = int

  let empty = 0
  let bit i = 1 lsl i
  let all nbit = (1 lsl nbit) - 1
  let set t i = t lor bit i
  let test t i = t land bit i <> 0
  let of_list seq = List.fold_left set empty seq
end

type problem_spec = {
  length : int;
  indicators : Bitset.t;
  buttons : Bitset.t array;
  joltages : int array;
}

let parse_int_list chan =
  Scanf.bscanf chan "%d" @@ fun x ->
  let xs = Seq.of_dispenser (fun () -> Scanf.bscanf_opt chan ",%d" Fun.id) in
  x :: List.of_seq xs

let parse_line s =
  let chan = Scanf.Scanning.from_string s in
  Scanf.bscanf chan "[%[.#]]" @@ fun indicator_str ->
  let length = String.length indicator_str in
  let indicators =
    let bs = ref Bitset.empty in
    String.iteri
      (fun i -> function
        | '#' -> bs := Bitset.set !bs i
        | '.' -> ()
        | _ -> failwith "invalid indicator char")
      indicator_str;
    !bs
  in
  let buttons =
    Seq.of_dispenser (fun () ->
        Scanf.bscanf_opt chan " (%r)" parse_int_list Bitset.of_list)
    |> Array.of_seq
  in
  Scanf.bscanf chan " {%r}" parse_int_list @@ fun joltages' ->
  let joltages = Array.of_list joltages' in
  { length; indicators; buttons; joltages }
