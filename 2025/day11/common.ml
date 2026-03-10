module StrHashtbl = Hashtbl.Make (String)

module Graph = struct
  type node = { name : string; adj : int Dynarray.t }
  type t = { nodes : node Dynarray.t; name_lookup : int StrHashtbl.t }

  let create n =
    let g = { nodes = Dynarray.create (); name_lookup = StrHashtbl.create n } in
    Dynarray.ensure_capacity g.nodes n;
    g

  let length t = Dynarray.length t.nodes

  let node_id t name =
    match StrHashtbl.find_opt t.name_lookup name with
    | Some id -> id
    | None ->
        let id = length t in
        Dynarray.add_last t.nodes { name; adj = Dynarray.create () };
        StrHashtbl.add t.name_lookup name id;
        id

  let node_name t id = (Dynarray.get t.nodes id).name
  let iter_adj t id f = Dynarray.iter f (Dynarray.get t.nodes id).adj

  let add_edges t src dsts =
    let src_adj = (Dynarray.get t.nodes (node_id t src)).adj in
    List.iter (fun dst -> Dynarray.add_last src_adj (node_id t dst)) dsts
end

let parse_input chan =
  let lines = In_channel.input_lines chan in
  let g = Graph.create (List.length lines) in
  List.iter
    (fun line ->
      let src_len = String.index line ':' in
      let src = String.sub line 0 src_len in
      let dsts =
        String.sub line (src_len + 2) (String.length line - src_len - 2)
        |> String.split_on_char ' '
      in
      Graph.add_edges g src dsts)
    lines;
  g

let topological_sort g =
  let n = Graph.length g in
  let in_degree = Array.make n 0 in
  for i = 0 to n - 1 do
    Graph.iter_adj g i @@ fun j -> in_degree.(j) <- in_degree.(j) + 1
  done;
  let q = Array.make n 0 in
  let front = ref 0 and rear = ref 0 in
  for i = 0 to n - 1 do
    if in_degree.(i) = 0 then begin
      q.(!rear) <- i;
      incr rear
    end
  done;
  while !front < !rear do
    let i = q.(!front) in
    incr front;
    Graph.iter_adj g i begin fun j ->
        in_degree.(j) <- in_degree.(j) - 1;
        if in_degree.(j) = 0 then begin
          q.(!rear) <- j;
          incr rear
        end
      end
  done;
  if !front <> n then failwith "cycle in graph";
  q

let path_count_single_src g topo idx =
  let path_count = Array.make (Graph.length g) 0 in
  path_count.(topo.(idx)) <- 1;
  for i = idx to Array.length topo - 1 do
    let id = topo.(i) in
    let count = path_count.(id) in
    Graph.iter_adj g id @@ fun id' ->
    path_count.(id') <- path_count.(id') + count
  done;
  path_count
