type vec3 = float * float * float

let elementwise o ((x1, y1, z1):vec3) ((x2, y2, z2):vec3) :vec3 =
  (o x1 x2, o y1 y2, o z1 z2)

let map o ((x, y, z):vec3): vec3 = (o x, o y, o z)

let sub = elementwise (-.)
let add = elementwise (+.)
let divide = elementwise (/.)
let mult = elementwise ( *.)

let powAll s = map (fun x -> x ** s)
let addAll s = map (fun x -> x +. s)

let inv = map (fun x -> 1.0 /. x)

let scale s = map (fun x -> x *. s)

let foldLeft f a ((x, y, z):vec3) = f(f (f a x) y) z

let dotProd v1 v2 = foldLeft (+.) 0.0 (mult v1 v2)

let lengthSquared v = dotProd v v

let length v = sqrt (lengthSquared v)

let normalise v = 
  let l = length v in 
  divide v (l, l, l) 

let reflect v1 v2 = sub (scale (2.0 *. (dotProd v1 v2)) v2) v1
