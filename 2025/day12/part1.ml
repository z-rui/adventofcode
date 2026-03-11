(* We can skip searching for the following cases:
   1. If (rows / shape_height) * (cols / shape_width) >= total number of shapes,
      then the grid is large enough to hold all tiles.
      (Thankfully, all shapes share the same height and width in this problem.)
   2. If rows * cols < total area of required tiles, then the grid is too
      small to hold all tiles.
   These conditions apply regardless how the shapes are placed.
   
   Only if (1) and (2) are not met, we need to search for solutions.
   However, for the real input (unlike the test input given in the problem),
   all input cases meet one of the two cases, so there is no need for
   searching.
   This is a trick problem, as the search space would be huge.
*)

(* these are constants in the problem *)
let shape_height = 3
let shape_width = 3

type problem_spec = { cols : int; rows : int; required_count : int array }

let count_char c s =
  String.fold_left (fun acc c' -> if c = c' then acc + 1 else acc) 0 s

let parse_input chan =
  let areas = Dynarray.create () and specs = Dynarray.create () in
  let rec loop () =
    match Scanf.bscanf chan "%[x0-9]:" Fun.id with
    | header ->
        (* There are always 6 shapes in the input.  This is hardcoded. *)
        if String.contains header 'x' then begin
          Scanf.sscanf header "%dx%d" @@ fun w h ->
          Scanf.bscanf chan " %d %d %d %d %d %d\n" @@ fun a b c d e f ->
          Dynarray.add_last specs
            { rows = h; cols = w; required_count = [| a; b; c; d; e; f |] }
        end
        else begin
          Scanf.sscanf header "%d" @@ fun id ->
          Scanf.bscanf chan "\n%s\n%s\n%s\n\n" @@ fun a b c ->
          Dynarray.add_last areas
          @@ Array.fold_left
               (fun acc s -> acc + count_char '#' s)
               0 [| a; b; c |]
        end;
        loop ()
    | exception End_of_file -> ()
  in
  loop ();
  Dynarray.(to_array areas, to_array specs)

let feasible areas { rows; cols; required_count } =
  let total_required = Array.fold_left ( + ) 0 required_count in
  let total_area =
    Array.map2 ( * ) areas required_count |> Array.fold_left ( + ) 0
  in
  if rows / shape_height * (cols / shape_width) >= total_required then true
  else if rows * cols < total_area then false
  else failwith "happy searching, suckers"

let () =
  let areas, specs = parse_input Scanf.Scanning.stdin in
  Array.to_seq specs
  |> Seq.filteri (fun i spec -> feasible areas spec)
  |> Seq.length |> print_int;
  print_newline ()
