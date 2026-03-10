open Common

(* n < 500, brute force is fine *)
let max_rectangle_area points =
  let n = Array.length points in
  let max_area = ref 0 in
  for i = 0 to n - 1 do
    let x1, y1 = points.(i) in
    for j = i + 1 to n - 1 do
      let x2, y2 = points.(j) in
      let area = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1) in
      if area > !max_area then max_area := area
    done
  done;
  !max_area

let () =
  parse_input Scanf.Scanning.stdin |> max_rectangle_area |> print_int;
  print_newline ()
