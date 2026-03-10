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

let parse_input chan =
  In_channel.input_lines chan
  |> List.map (fun s -> Scanf.sscanf s "%d,%d,%d" (fun x y z -> (x, y, z)))

let build_edges nodes =
  let edge i (x1, y1, z1) j (x2, y2, z2) =
    let dx, dy, dz = (x1 - x2, y1 - y2, z1 - z2) in
    let dist_sq = (dx * dx) + (dy * dy) + (dz * dz) in
    (i, j, dist_sq)
  in
  let[@tail_mod_cons] rec loop i = function
    | [] -> []
    | x :: xs ->
        List.mapi (fun j y -> edge i x (i + 1 + j) y) xs :: loop (i + 1) xs
  in
  loop 0 nodes |> List.concat
  |> List.sort (fun (_, _, d1) (_, _, d2) -> Int.compare d1 d2)
