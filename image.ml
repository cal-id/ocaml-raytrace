open Printf

type color = float * float * float
type xy = int * int
type image = Image of xy * (color array) array

(* Create a colour instance from its hex code starting with 0x *)
let clr h :color= 
  let n = (int_of_string h) in
  let b = float (n mod 256) /. 256.0 in
  let g = float ((n / 256) mod 256) /. 256.0 in
  let r = float ((n / 256 / 256) mod 256) /. 256.0 in
  (r, g, b)

(* Define some standard colours *)
let white = clr("0xFFFFFF")
let black = clr("0x000000")
let blue = clr("0x0000FF")
let green = clr("0x00FF00")

(* Helper function for debugging *)
let printClr (r,g,b) = printf "%3.2f %3.2f %3.2f\n" r g b

(* create image according to a constructor function *)
let imageFromFunc (((w, h) as dim):xy) constructor = 
    let initRow y = Array.init w (fun x -> constructor (x, y)) in 
  Image(dim, Array.init h initRow)

(* create image of a set colour and dimension*)
let image dim clr = imageFromFunc dim (fun _ -> clr)

(*Returns the size of a given image*)
let size (Image(xy, _)) = xy

(*Sets a pixel in an image at a position to a colour*)
let drawPixel (Image(_, a)) clr ((x, y): xy) = (a.(y)).(x) <- clr

exception ClrOutOfRange of float

(*Writes a PPM image to a file*)
let toPPM (Image((w, h), a)) filename =
  let oc = open_out filename in
  let rec conv i = 
    if i < -.0.001 then raise (ClrOutOfRange i)
    else if i < 0.0 then 0
    else if i >= 1.001 then raise (ClrOutOfRange i)
    else if i >= 1.0 then 255
    else int_of_float (i *. 256.0) in
  let printC (r, g, b) = fprintf oc "%4d%4d%4d" (conv r) (conv g) (conv b) in
  let printRow row = (Array.iter printC row; fprintf oc "\n") in
  fprintf oc "P3\n%d %d\n255\n" w h;  (* write the header *)   
  Array.iter printRow a;              (* write each row *)
  close_out oc                        (* flush and close the channel *)

(*Exception raised with negative length calls to any of the line functions*)
exception NegativeLengthLine

(*Base line drawing funcition, taking a function to move to the next point
 *from the previous one. This is partially evaluated to give the required
 *functions.*)
let rec drawStraightLine f i c p n = 
  if n = 0 
  then ()
  else if n < 0 
  then raise NegativeLengthLine
  else 
    (drawPixel i c p; 
     drawStraightLine f i c (f p) (n - 1))

let drawHoriz = drawStraightLine (fun (x, y) -> (x + 1, y    ))
let drawVert  = drawStraightLine (fun (x, y) -> (x,     y + 1))
let drawDiag  = drawStraightLine (fun (x, y) -> (x + 1, y + 1))

(*Draws a line between two points with Bresenham's algorithm*)
let drawLine i c (((x0, y0) as first):xy) (((x1, y1) as last):xy) =
  let dx = abs (x1 - x0) in
  let dy = abs (y1 - y0) in
  let sx = if x0 < x1 then 1 else -1 in
  let sy = if y0 < y1 then 1 else -1 in
  let rec loop ((x, y) as p) err =
    drawPixel i c p;
    if p = last 
    then ()
    else 
      let e2 = 2 * err in
      let advanceX = e2 > -dy in
      let advanceY = e2 < dx in
      let x' = if advanceX then x + sx else x in
      let y' = if advanceY then y + sy else y in 
      let erry = if advanceX then dy else 0 in
      let errx = if advanceY then dx else 0 in 
      loop (x', y') (err - erry + errx) in 
  loop first (dx - dy)

exception ConversionFailed

let toPNG im filename = 
  let () = toPPM im "tmp.ppm" in
  let cmd = sprintf "convert tmp.ppm %s" filename in
  let status = Sys.command cmd in
  if status <> 0 then raise ConversionFailed
  else let () = Sys.remove "tmp.ppm" in ()
