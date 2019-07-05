open Ctypes
open Foreign
open Printf

let isZero a = (a < 1e-14) && (-1e-14 < a) 
let isZeroRelaxed a = (a < 1e-3) && (-1e-3 < a) 
let cbrt = foreign "cbrt" (double @-> returning double)
(* TODO: There must be a better way? *)
let pi = 3.14159265358979323846
(*
 Returns the solutions to
    c0 + c1*x + c2*x^2 + c3*x^3 + c4*x^4 = 0
 as a float list. 
 Adapted from https://github.com/erich666/GraphicsGems/blob/master/gems/Roots3And4.c*)

let solveQuadratic c0 c1 c2 = 
  let p_half = c1 /. (2.0 *. c2) in
  let q = c0 /. c2 in
  let d = p_half *. p_half -. q in
  if (isZero d) then [-.p_half]
  else if (d < 0.0) then []
  else 
    let sqrt_d = sqrt d in 
    [sqrt_d -. p_half; -. sqrt_d -. p_half]

let solveCubic c0 c1 c2 c3 =
  let a = c2 /. c3 in
  let b = c1 /. c3 in
  let c = c0 /. c3 in
  (* substitute x = y - A/3 to eliminate quadric term:
     	        x^3 +px + q = 0 *)
  let sq_a = a *. a in
  let p = 1.0 /. 3.0 *. (-. 1.0 /. 3.0 *. sq_a +. b) in
  let q = 1.0 /. 2.0 *. (2.0 /. 27.0 *. a *. sq_a -. 1.0 /. 3.0 *. a *. b +. c) in
  let cb_p = p *. p *. p in
  let d = q *. q +. cb_p in 

  let workingRoots = 
    if isZero d then
      if isZero q then [0.0]
      else 
        let u = cbrt (-.q) in
        [2.0 *. u; -. u]
    else if d < 0.0 then
      let phi = 1.0 /. 3.0 *. acos(-.q /. sqrt(-.cb_p)) in
      let t = 2.0 *. sqrt(-. p) in
      [
        t *. cos(phi);
        -. t *. cos(phi +. pi /. 3.0);
        -. t *. cos(phi -. pi /. 3.0);
      ]
    else 
      let sqrt_d = sqrt d in
      [cbrt(sqrt_d -. q) -. cbrt(sqrt_d +. q)]
  in
  (* resubstitute *)
  List.map (fun f -> f -. 1.0 /. 3.0 *. a) workingRoots

exception SolveCheckFailed of float * float * float * float * float 

let solveQuartic c0 c1 c2 c3 c4 =
  (* Normal form x^4 + Ax^3 + Bx^2 + Cx + D = 0 *)
  let a = c3 /. c4 in
  let b = c2 /. c4 in
  let c = c1 /. c4 in
  let d = c0 /. c4 in

  let sq_a = a *. a in 
  let p = - 3.0 /. 8.0 *. sq_a +. b in 
  let q = 1.0 /. 8.0 *. sq_a *. a -. 1.0 /. 2.0 *. a *. b +. c in 
  let r = - 3.0 /. 256.0 *. sq_a *. sq_a +. 1.0 /. 16.0 *. sq_a *. b -. 1.0 /. 4.0 *. a *. c +. d in 
  let workingRoots =
    if isZero r then 
      0.0::(solveCubic q p 0.0 1.0)
    else 
      (* solve the resolvent cubic ... *)
      let coef1 = 1.0 /. 2.0 *. r *. p -. 1.0 /. 8.0 *. q *. q in
      let coef3 = -. 1.0 /. 2.0 *. p in
      (* ... and take the one real solution ... *)
      let z = List.hd (solveCubic coef1 (-. r) coef3 1.0) in
      (* ... to build two quadric equations *)
      let u' = z *. z -. r in 
      let v' = 2.0 *. z -. p in 
      if (not (isZero u') && u' < 0.0) || (not (isZero v') && v' < 0.0) then [] 
      else
        let u = 
          if isZero u' then 0.0
          else sqrt u' in
        let v = 
          if isZero v' then 0.0
          else sqrt v' in
        let coef2 = if q < 0.0 then -. v else v in 
        let sols1 = solveQuadratic (z -. u) coef2 1.0 in 
        let sols2 = solveQuadratic (z +. u) (-.coef2) 1.0 in
        List.append sols1 sols2 in
  List.map (fun f -> f -. 1.0 /. 4.0 *. a) workingRoots
