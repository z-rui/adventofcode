open Common

let solve nodes edges =
  let node_count = List.length nodes in
  let last_edge = ref None in
  let node_sets = Disjoint_set.create node_count in
  List.iter
    (fun ((a, b, _) as e) ->
      let i = Disjoint_set.find node_sets a
      and j = Disjoint_set.find node_sets b in
      if i <> j then begin
        last_edge := Some e;
        Disjoint_set.union node_sets i j
      end)
    edges;
  let a, b, _ = Option.get !last_edge in
  let x1, _, _ = List.nth nodes a and x2, _, _ = List.nth nodes b in
  x1 * x2

let () =
  let nodes = parse_input stdin in
  let edges = build_edges nodes in
  solve nodes edges |> print_int;
  print_newline ()
