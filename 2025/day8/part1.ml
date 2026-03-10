open Common
module IntMaxHeap = Pqueue.MakeMax (Int)

let solve nodes edges n =
  let node_count = Array.length nodes in
  let node_sets = Disjoint_set.create node_count in
  for i = 0 to n - 1 do
    match EdgeMinHeap.pop_min edges with
    | None -> failwith "not enough connections"
    | Some { src; dst } ->
        let i = Disjoint_set.find node_sets src
        and j = Disjoint_set.find node_sets dst in
        if i <> j then Disjoint_set.union node_sets i j
  done;
  let count = Array.make node_count 0 in
  for i = 0 to node_count - 1 do
    let i' = Disjoint_set.find node_sets i in
    count.(i') <- count.(i') + 1
  done;
  let heap = IntMaxHeap.of_array count in
  Seq.of_dispenser (fun () -> IntMaxHeap.pop_max heap)
  |> Seq.take 3 |> Seq.fold_left ( * ) 1

let () =
  let n = int_of_string Sys.argv.(1) in
  let nodes = parse_input Scanf.Scanning.stdin in
  let edges = build_edge_heap nodes in
  solve nodes edges n |> print_int;
  print_newline ()
