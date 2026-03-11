open Common
module IntHashtbl = Hashtbl.Make (Int)

module type COMPRESS_COORD = sig
  type coord
  type t

  val of_seq : coord Seq.t -> t
  val compress : t -> coord -> coord
  val decompress : t -> coord -> coord
  val dim : t -> coord
end

module Compress1D : COMPRESS_COORD with type coord = int = struct
  type coord = int
  type t = { compress : int IntHashtbl.t; decompress : int array }

  let of_seq seq =
    let coords = Array.of_seq seq in
    let n = Array.length coords in
    Array.fast_sort Int.compare coords;
    let compress = IntHashtbl.create n in
    let k = ref 0 in
    for i = 0 to n - 1 do
      let x = coords.(i) in
      if i = 0 || x <> coords.(i - 1) then begin
        IntHashtbl.add compress x !k;
        incr k
      end
    done;
    let decompress = Array.make (IntHashtbl.length compress) (-1) in
    IntHashtbl.iter (fun x i -> decompress.(i) <- x) compress;
    { compress; decompress }

  let compress t x = IntHashtbl.find t.compress x
  let decompress t x = t.decompress.(x)
  let dim t = Array.length t.decompress
end

module Compress2D : COMPRESS_COORD with type coord = int * int = struct
  type coord = int * int
  type t = Compress1D.t * Compress1D.t

  let of_seq seq =
    let xseq, yseq = Seq.unzip seq in
    Compress1D.(of_seq xseq, of_seq yseq)

  let compress (tx, ty) (x, y) = Compress1D.(compress tx x, compress ty y)
  let decompress (tx, ty) (x, y) = Compress1D.(decompress tx x, decompress ty y)
  let dim (tx, ty) = Compress1D.(dim tx, dim ty)
end

let sign x =
  let s = Int.compare x 0 in
  if s > 0 then 1 else if s < 0 then -1 else 0

module Bitmap : sig
  type pixel_kind = Outside | Inside | Boundary
  type t

  val create : (int * int) array -> int -> int -> t
  val query : t -> int -> int -> pixel_kind
end = struct
  type pixel_kind = Outside | Inside | Boundary
  type t = Bigarray.((int, int8_unsigned_elt, c_layout) Array2.t)

  let create points xsize ysize =
    let bitmap =
      Bigarray.(Array2.create int8_unsigned c_layout) (xsize + 2) (ysize + 2)
    in
    Bigarray.Array2.fill bitmap 1;
    (* Conventions:
     * 0 - outside of boundary
     * 1 - inside boundary (or initial state)
     * 2 - boundary
     *)
    let drawline (x1, y1) (x2, y2) =
      let rec aux x y dx dy =
        bitmap.{x + 1, y + 1} <- 2;
        if x <> x2 || y <> y2 then aux (x + dx) (y + dy) dx dy
      in
      if x1 = x2 then aux x1 y1 0 (sign (y2 - y1))
      else if y1 = y2 then aux x1 y1 (sign (x2 - x1)) 0
      else failwith "side must be horizontal or vertical"
    in
    let rec floodfill x y =
      if 0 <= x && x < xsize + 2 && 0 <= y && y < ysize + 2 && bitmap.{x, y} = 1
      then begin
        bitmap.{x, y} <- 0;
        floodfill (x - 1) y;
        floodfill x (y - 1);
        floodfill x (y + 1);
        floodfill (x + 1) y
      end
    in
    let n = Array.length points in
    for i = 1 to n - 1 do
      drawline points.(i - 1) points.(i)
    done;
    drawline points.(n - 1) points.(0);
    floodfill 0 0;
    bitmap

  let query t x y =
    match t.{x + 1, y + 1} with
    | 0 -> Outside
    | 1 -> Inside
    | 2 -> Boundary
    | _ -> failwith "bitmap corrupted"
end

(* brute force seems sufficient with coordinate compression *)
let max_rectangle_area points =
  let comp = Compress2D.of_seq (Array.to_seq points) in
  let xsize, ysize = Compress2D.dim comp in
  let points' = Array.map (Compress2D.compress comp) points in
  let bitmap = Bitmap.create points' xsize ysize in
  let check_rect (x1, y1) (x2, y2) =
    try
      let xmin = Int.min x1 x2
      and xmax = Int.max x1 x2
      and ymin = Int.min y1 y2
      and ymax = Int.max y1 y2 in
      for x = xmin to xmax do
        for y = ymin to ymax do
          match Bitmap.query bitmap x y with
          | Outside -> raise Exit
          | Inside | Boundary -> ()
        done
      done;
      true
    with Exit -> false
  in

  let n = Array.length points' in
  let max_area = ref 0 in
  for i = 0 to n - 1 do
    let p1 = points'.(i) in
    for j = i + 1 to n - 1 do
      let p2 = points'.(j) in
      if check_rect p1 p2 then
        let x1, y1 = Compress2D.decompress comp p1
        and x2, y2 = Compress2D.decompress comp p2 in
        let area = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1) in
        if area > !max_area then max_area := area
    done
  done;
  !max_area

let () =
  parse_input Scanf.Scanning.stdin |> max_rectangle_area |> print_int;
  print_newline ()
