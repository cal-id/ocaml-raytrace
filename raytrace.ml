open Vector
open Printf
open Image
open Quartic
open Transformation

type point = vec3
type direction = vec3

(* A ray is a point paired with a direction vector *)
type ray = point * direction

(* An object is defined by some attributes specifying its size *)
type geometry = Torus of float * float (* major radius, minor radius*)
              | Sphere of float (* radius *)
              | Plane of direction (* normal *)

(* A light is one of these enums*)
type light = Ambient of color 
           | Point of point * color * float (*position, color and intensity *)

(* A material is described by kd, ks, alpha and reflectivity *)
type material = float * float * float * float

(* An object's surface is described by: a colour, and a material *)
type surface = color * material

(* Simple representation of rotations as radians in x, y and z axis *)
type rotation = vec3

(* A transformation is a translation and a rotation *)
type transformation = direction * rotation

(* An object is a colour, a transformation into world coordinates
   and a description of its geometry *)
type obj = surface * transformation * geometry

(* A scene represents all the lights and objects *)
type scene = obj list * light list

(* The camera initially faces down the z axis with the y axis as 
   the up invariant. 

   We can describe cameras by a pair of the transformation and a direction vector
   (width in world, height in world, distance from eye in world) *)
type camera = transformation * direction

(* A display is a camera and a size *)
type display = camera * (int * int)

(* This is the colour that is used when there is no ray intersect *)
let background = (0.001, 0.001, 0.001)

let epsilon = 0.0001 (*Bias factor for reflected and shadow rays *)

(* Given a point in object coordinates which is on the surface of a geometry,
   return the normal *)
let getNormalOnGeometryFromPoint p = function
  | Torus(major, minor) -> 
    (* c is the point on the major axis of the Torus below p*)
    let c = scale major (normalise (mult p (1.0, 0.0, 1.0))) in
    normalise (sub p c)
  | Sphere(radius) -> normalise p
  | Plane(n) -> n

(* Helper function to make more readable *)
let square = function a -> a *. a

(* Helper function for reducing possibly empty list to minimum positive value option*)
let minMostPositiveFloat l = 
  let foldHelp accOpt y =
    if y >= 0.0 then 
      match accOpt with 
      | None -> Some(y)
      | Some(acc) -> Some(min acc y) 
    else accOpt in
  List.fold_left foldHelp None l

(* Assume that the object is centred on the origin in the standard orientation
   and return an optional ray parameter for the closest point of intersection

   A simple transform (without rotation or scale might be)
   let ((ox, oy, oz) as ovec) = sub rayFrom centre in 
   let ((dx, dy, dz) as dvec) = rayDirection in

   See https://marcin-chwedczuk.github.io/ray-tracing-torus
*)
let intersectRayWithGeometry (((ox, oy, oz) as ovec:vec3), ((dx, dy, dz) as dvec:vec3)) geometry =
  let solutionsForT = (match geometry with 
      | Torus(major, minor) ->
        (* Tori are centred at point (0, 0, 0) and lying on the XZ plane *)
        let c4 = square (lengthSquared dvec) in
        let c3 = 4.0 *. (lengthSquared dvec) *. dotProd ovec dvec in
        let k = lengthSquared ovec -. ((square minor) +. (square major)) in
        let c2 = 2.0 *. (lengthSquared dvec) *. k +. 4.0 *. square (dotProd ovec dvec) 
                 +. 4.0 *. (square major) *. square dy in 
        let c1 = 4.0 *. k  *. (dotProd ovec dvec) +. 8.0 *. (square major) *. oy *. dy in
        let c0 = square k -. 4.0 *. (square major) *. ((square minor) -. (square oy)) in
        solveQuartic c0 c1 c2 c3 c4
      | Sphere(radius) -> 
        (* See http://viclw17.github.io/2018/07/16/raytracing-ray-sphere-intersection/
           For explaination *)
        let c0 = lengthSquared ovec -. square radius in
        let c1 = 2.0 *. dotProd dvec ovec in 
        let c2 = lengthSquared dvec in
        solveQuadratic c0 c1 c2 
      | Plane(n) ->
        let s = -. (dotProd ovec n) /. (dotProd dvec n) in
        if s < 0.0 then [] 
        else [s])
  (* Get closest point of intersection, if there is one *)
  in minMostPositiveFloat solutionsForT

(* This transforms from linear RGB space to display RGB space.
   This program works almost entirely in linear RGB space, using this transform
   at the last minute before creating the image array. *)
let tonemap (linearRGB: color): color =
  let invGamma = 1.0 /. 2.2 in
  let a = 2.0 in (*controls brightness*)
  let b = 1.3 in (*controls contrast*)
  let powRGB = powAll b linearRGB in
  let inc = (0.5 /. a) ** b in 
  let displayRGB = mult (inv (addAll inc powRGB)) powRGB in
  powAll invGamma displayRGB  

(* A useful intermediate value stores the intersect between a ray and an object.
   It is made up of: the point of intersection, the normal at the intersection, 
   the color of the object at that point  *)
type ray_intersect = (point * direction * surface) option

(* Given a ray defined by a pixel and an eye, return a ray_intersect instance
   for the closest intersect with objects in the scene. *)
let intersectRayWithScene ((from, dir):ray) (objects: obj list): ray_intersect =
  List.fold_left (
    fun accOpt (objSurface, (objTrans, objRot), objGeom) ->
      (* Transform from world coordinates to object coordinates. Since we are't considering
         scaling transforms at the moment, we never need to transform back to world coordinates
         since the length of the ray parameter for any intersection isn't changed under these
         transforms. *)
      let fromWorldToObjectCoords p = invertRotation objRot (sub p objTrans) in
      let rayInObjectCoords = (
        fromWorldToObjectCoords from, 
        invertRotation objRot dir
      ) in 

      let tOpt = intersectRayWithGeometry rayInObjectCoords objGeom in

      (* Given a ray parameter t, this returns a ray_intersect instance*)
      let getIntersect t = 
        let p = add from (scale t dir) in
        (* This is the normal in object coordinates *)
        let n' = getNormalOnGeometryFromPoint (fromWorldToObjectCoords p) objGeom in
        (* Transform to world coordinates *)
        let n = applyRotation objRot n' in 
        (p, n, objSurface) in

      (* Perform the folding to get the closest intersect*)
      match accOpt, tOpt with
      | _                   , None    -> accOpt
      | None                , Some(t) -> Some(getIntersect t)
      | Some((p, n, objSurface)), Some(t) -> 
        if t < length (sub from p) && t > 0.0 then 
          Some(getIntersect t)
        else accOpt
  ) None objects

(* Given a point on an object, it's normal, the viewing position and a set of lights
   return the colour that the point is observed *)
let illuminatePoint p n eye ((c_diff, (k_d, k_s, alpha, _)):surface)  ((objects, lights):scene): color = 
  (* We'll apply this function to each of the lights in turn
     and then add up each light's contribution. *)
  let mapLightToColour = (function 
      | Ambient(clr) -> mult clr c_diff
      | Point(pos, c_spec, intensity) ->
        let distanceToLight = (length (sub pos p)) in 

        (* Work out how strong the light is at this distance *)
        let scaledIntensity = intensity /. (pi *. 4.0 *. (distanceToLight ** 2.0)) in
        let i = scale scaledIntensity c_spec in

        (* These are the standard vectors used in the phong illumination 
           model. They are all normalised:
               l: direction vector of light from p
               r: l reflected in the normal
               v: direction vector of the viewer *)
        let l = normalise (sub pos p) in
        let r = reflect l n in
        let v = normalise (scale (-1.0) (sub p eye)) in

        (* Calculate diffuse and specular components*)
        let diffuseMax = max 0.0 (dotProd (normalise n) l) in
        let diffuse = scale (k_d *. diffuseMax) (mult c_diff i) in
        let specularPow = (max 0.0 (dotProd r v)) ** alpha in
        let specular = scale (k_s *. specularPow) (mult c_spec i) in
        let thisLightsContribution = add diffuse specular in

        (* Check that we aren't in the shadow of this light *)
        let shadowRay = (add p (scale epsilon l), l) in
        match intersectRayWithScene shadowRay objects with
        | None -> thisLightsContribution
        | Some((interPos, _, _)) -> 
          if length (sub interPos p) < distanceToLight then 
            black
          else thisLightsContribution
    ) in
  let clrsFromLights = List.map mapLightToColour lights in 
  List.fold_left add black clrsFromLights

(* This recursively follows the path of a ray accumulating its illumination 
   through reflections. *)
let rec traceRay ((from, dir) as ray: ray) ((objects, lights) as scene) bouncesLeft = 
  match intersectRayWithScene ray objects with
  | None -> background
  | Some((p, n, (c, (_, _, _, rfact) as m))) -> (
      (* This is the illumination without reflection *)
      let directIllumination = illuminatePoint p n from m scene in
      if bouncesLeft = 0 || rfact = 0.0 then 
        directIllumination
      else
        let r = normalise (reflect (scale (-1.0) dir) (normalise n)) in 
        let reflectedRay = (add p (scale epsilon r), r) in
        let refectedIllum = traceRay reflectedRay scene (bouncesLeft - 1) in
        add (scale rfact refectedIllum) (scale (1.0 -. rfact) directIllumination)
    )

(* Given a eye and pixel positions in world spaces and a scene,
   return the colour of that pixel ready to display. *)
let renderPixel eye pixel scene = 
  tonemap (
    let ray = (eye, sub pixel eye) in
    let maxBounces = 2 in
    traceRay ray scene maxBounces
  )

(* Helper constructor for creating a display from fov*)
let createDisplay (w, h) fov eye rotation =
  let aspectRatio = (float w) /. (float h) in
  let w_w = 2.0 *. tan(fov /. 2.0) in
  let h_w = w_w /. aspectRatio in 
  (((eye, rotation), (w_w, h_w, 1.0)), (w, h))

(* Casts a pair of coordinates on the screen to 3d coordinates in the world *)
let imageToWorld ((((camTransl, camRot), (w_w, h_w, z_w)), (w, h)):display) (x, y) =
  let w_step = w_w /. float w in
  let h_step = h_w /. float h in
  (* Vector pointing from the eye to the position in 3D space of the screen
     in *camera* coordinates *)
  let relativeToEye = (w_step *. x -. w_w /. 2.0, h_step *. (float h -. y) -. h_w /. 2.0, z_w) in 
  add camTransl (applyRotation camRot relativeToEye)

(* Render the scene on the given image *)
let renderScene ((((eye, _), _), dim) as display) scene = 
  let perPixel (xi, yi) = 
    renderPixel eye (imageToWorld display (float xi, float yi)) scene in
  imageFromFunc dim perPixel


(* Example Usage: *)
(* Create a ring of balls centered on the origin *)
let ringOfBalls sur displacement total =
  let radius = 0.5 *. pi *. displacement /. (float total) in 
  let s = Sphere(radius) in
  let initialPos = (0.0, radius, displacement) in
  let noRot = (0.0, 0.0, 0.0) in
  let rec create = function 
    | 0 -> []
    | n -> 
      let rotY = 2.0 *. pi *. float n /. float total in
      let pos = applyRotationY rotY initialPos in
      (sur, (pos, noRot), s) :: (create (n-1)) in
  create total


(* Parameterised scene definiton where t ranges [0, 1] for the
   different frames and count is an index (used only for the names) *)
let createScene count t =
  (* Define some standard colours / materials *)
  let blue      = clr("0x0071BC") in
  let red       = clr("0xFF1D25") in
  let green     = clr("0x3AA010") in
  let ambi      = clr("0x010101") in
  let planeClr  = clr("0x111111") in
  let white     = clr("0xFFFFFF") in
  let purple    = clr("0xAAAAFF") in
  let planeMaterial: material = (0.6, 0.0, 0.0, 0.1) in
  let sphereMaterial: material = (0.8, 1.2, 10.0, 0.3) in
  (* Define objects and lights *)
 
  let plane = 
     let noRotation = (0.0, 0.0, 0.0) in
     let noTranslation = (0.0, 0.0, 0.0) in
     let yUnitVector = 0.0, 1.0, 0.0 in
    ((planeClr , planeMaterial) , (noTranslation, noRotation), Plane(yUnitVector)) in
  let objs = plane::
             (ringOfBalls (blue, sphereMaterial) 1.7 7) 
             @ (ringOfBalls (green, sphereMaterial) 0.7 3) in
  let lights = [
    Ambient(ambi);
    Point((2.0, 2.0, 2.0), white, 80.0);
    Point((-2.0, 2.5, -1.5), purple, 50.0)
  ] in
  let rotY = t *. pi *. 2.0 in
  let rotX = 0.35 in (* Make camera point down from above *)
  let camRot = (rotX, rotY, 0.0) in
  let camTranl = applyRotationY (rotY) (0.0, 1.84, -2.5) in
  let display = createDisplay (640, 480) 1.0 camTranl camRot in
  let torus = 
    let torusCentre = (0.0, 0.74, 0.0) in
    let torusOrientation =  (t *. pi *. 2.0 *. 3.0, 0.0, t *. pi *. 2.0) in
    ((red, sphereMaterial), (torusCentre, torusOrientation), Torus(0.35, 0.1)) in
  let scene = (torus::objs, lights) in
  toPNG (renderScene display scene) (sprintf "test/rot-large-%d.png" count)

let rec animate totalFrames currentFrame =
  if currentFrame < totalFrames then 
    let t = (float currentFrame) /. (float totalFrames) in
    (createScene currentFrame t; 
     animate totalFrames (currentFrame + 1))
  else ()

(* Output 150 frames *)
let () = (animate 150 0; 
          printf "Finished raytrace \n")

