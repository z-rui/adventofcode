open Common

let solve g =
  let n = Graph.length g in
  let topo = topological_sort g in
  let inv_topo = Array.make n 0 in
  Array.iteri (fun idx id -> inv_topo.(id) <- idx) topo;
  let node_ids = Array.map (Graph.node_id g) [| "svr"; "fft"; "dac"; "out" |] in
  Array.sort (fun i j -> Int.compare inv_topo.(i) inv_topo.(j)) node_ids;
  let total = ref 1 in
  for i = 0 to Array.length node_ids - 2 do
    let src = node_ids.(i) and dst = node_ids.(i + 1) in
    let path_count = path_count_single_src g topo inv_topo.(src) in
    total := !total * path_count.(dst)
  done;
  !total

let () =
  parse_input stdin |> solve |> print_int;
  print_newline ()
