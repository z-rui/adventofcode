let parse_input chan =
  let points = Dynarray.create () in
  try
    while true do
      Scanf.bscanf chan "%d,%d\n" @@ fun x y -> Dynarray.add_last points (x, y)
    done
  with End_of_file -> Dynarray.to_array points
