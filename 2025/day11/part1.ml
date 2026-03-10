open Common

let solve g =
  let topo = topological_sort g in
  let you = Graph.node_id g "you" and out = Graph.node_id g "out" in
  match Array.find_index (Int.equal you) topo with
  | Some idx -> (path_count_single_src g topo idx).(out)
  | None -> failwith "cannot find you in topological order"

let () =
  parse_input stdin |> solve |> print_int;
  print_newline ()
