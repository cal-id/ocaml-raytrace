open Vector


let applyRotationX rx ((x, y, z): vec3) =
    let c = cos rx in
    let s = sin rx in
    (* 1  0  0     x
       0  c -s  .  y 
       0  s  c     z  *)
    (x, c *. y -. s *. z, s *. y +. c *. z)

let applyRotationY ry ((x, y, z): vec3) =
    let c = cos ry in
    let s = sin ry in
    (* c  0  s     x
       0  1  0  .  y 
      -s  0  c     z  *)
    (c *. x +. s *. z, y, (-. s) *. x +. c *. z)

let applyRotationZ rz ((x, y, z): vec3) =
    let c = cos rz in
    let s = sin rz in
    (* c -s  0     x
       s  c  0  .  y 
       0  0  1     z  *)
    (c *. x -. s *. y, s *. x +. c *. y, z)

let applyRotation (rx, ry, rz) v =
    applyRotationZ rz (applyRotationY ry (applyRotationX rx v))

let invertRotation (rx, ry, rz) v =
    applyRotationX (-. rx) (applyRotationY (-. ry) (applyRotationZ (-. rz) v))