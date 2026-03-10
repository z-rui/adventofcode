open Common

let solve nodes edges =
  let node_count = List.length nodes in
  let node_sets = Disjoint_set.create node_count in
  List.iter
    (fun (a, b, _) ->
      let i = Disjoint_set.find node_sets a
      and j = Disjoint_set.find node_sets b in
      if i <> j then Disjoint_set.union node_sets i j)
    edges;
  let count = Array.make node_count 0 in
  for i = 0 to node_count - 1 do
    let i' = Disjoint_set.find node_sets i in
    count.(i') <- count.(i') + 1
  done;
  Array.sort (fun x y -> Int.compare y x) count;
  Array.to_seq count |> Seq.take 3 |> Seq.fold_left ( * ) 1

let () =
  let n = int_of_string Sys.argv.(1) in
  let nodes = parse_input stdin in
  let edges = build_edges nodes in
  solve nodes (List.take n edges) |> print_int;
  print_newline ()
