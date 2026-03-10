module Disjoint_set = struct
  type t = private int array

  let create n = Array.init n Fun.id

  let rec find t i =
    let j = t.(i) in
    if j = i then i
    else
      let j' = find t j in
      t.(i) <- j';
      j'

  let union t i j =
    let i' = find t i and j' = find t j in
    t.(i') <- j'
end

type node = { x : int; y : int; z : int }
type edge = { src : int; dst : int; dist_sq : int }

module EdgeMinHeap = Pqueue.MakeMin (struct
  type t = edge

  let compare { dist_sq = d1 } { dist_sq = d2 } = Int.compare d1 d2
end)

let parse_input chan =
  let nodes = Dynarray.create () in
  try
    while true do
      Scanf.bscanf chan "%d,%d,%d\n" @@ fun x y z ->
      Dynarray.add_last nodes { x; y; z }
    done
  with End_of_file -> Dynarray.to_array nodes

let build_edge_heap nodes =
  let dist_sq u v =
    let dx, dy, dz = (u.x - v.x, u.y - v.y, u.z - v.z) in
    (dx * dx) + (dy * dy) + (dz * dz)
  in
  let n = Array.length nodes in
  let edges = Dynarray.create () in
  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      Dynarray.add_last edges
        { src = i; dst = j; dist_sq = dist_sq nodes.(i) nodes.(j) }
    done
  done;
  EdgeMinHeap.of_array (Dynarray.to_array edges)
