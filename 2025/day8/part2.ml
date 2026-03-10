open Common

let solve nodes edges =
  let node_count = Array.length nodes in
  let edge_cnt = ref 0 in
  let last_edge = ref None in
  let node_sets = Disjoint_set.create node_count in
  while !edge_cnt < node_count - 1 do
    match EdgeMinHeap.pop_min edges with
    | None -> failwith "not enough connections"
    | Some ({ src; dst } as e) ->
        let i = Disjoint_set.find node_sets src
        and j = Disjoint_set.find node_sets dst in
        if i <> j then begin
          incr edge_cnt;
          last_edge := Some e;
          Disjoint_set.union node_sets i j
        end
  done;
  let { src; dst } = Option.get !last_edge in
  nodes.(src).x * nodes.(dst).x

let () =
  let nodes = parse_input Scanf.Scanning.stdin in
  let edges = build_edge_heap nodes in
  solve nodes edges |> print_int;
  print_newline ()
