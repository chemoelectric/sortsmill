(*
  Copyright (c) 2011 Barry Schwartz

  Permission is hereby granted, free of charge, to any person
  obtaining a copy of this software and associated documentation
  files (the "Software"), to deal in the Software without
  restriction, including without limitation the rights to use,
  copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following
  conditions:

  The above copyright notice and this permission notice shall be
  included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
  OTHER DEALINGS IN THE SOFTWARE.
*)

open Batteries
module Crossing = Caml2geom.Crossing

(*-----------------------------------------------------------------------*)

let posmod n k =
  let m = k mod n in
  if m < 0 then
    m + n
  else
    m

let deg theta = (180. /. Float.pi) *. theta
let rad theta = (Float.pi /. 180.) *. theta

let dsin = sin -| rad
let dcos = cos -| rad
let dtan = tan -| rad

let adsin = deg -| asin
let adcos = deg -| acos
let adtan = deg -| atan
let adtan2 x y = deg (atan2 x y)

(*-----------------------------------------------------------------------*)

module Extended_complex =
(* Extensions for the Complex module. *)
struct
  include Complex

  let x' x = { re = x; im = 0. }
  let y' y = { re = 0.; im = y }

  let re c = c.re
  let im c = c.im

  let dpolar norm arg = polar norm (rad arg)
  let rot theta = polar 1.0 (rad theta)
  let dir c = c / abs(c)

  let inner a b = (a.re *. b.re) +. (a.im *. b.im)

  let proj a b =
    let b' = dir b in
    of_float (inner a b') * b'

  let x_shear c angle = c + x'(c.im *. (dtan angle))
  let y_shear c angle = c + y'(c.re *. (dtan angle))

  let min_bound a b = { re = min a.re b.re; im = min a.im b.im }
  let max_bound a b = { re = max a.re b.re; im = max a.im b.im }

  let round c =
    let rnd v = floor (v +. 0.5) in
    { re = rnd c.re; im = rnd c.im }

  let t_printer paren outp c =
    if paren then IO.write outp '(';
    print outp c;
    if paren then IO.write outp ')'
end

(*-----------------------------------------------------------------------*)

module type Point_type =
sig
  type bool'
  type int'
  type float'
  type string'
  type t
  val zero : t
  val one : t
  val i : t
  val re : t -> float'
  val im : t -> float'
  val neg : t -> t
  val conj : t -> t
  val sqrt : t -> t
  val norm2 : t -> float'
  val norm : t -> float'
  val arg : t -> float'
  val exp : t -> t
  val log : t -> t
  val inv : t -> t
  val succ : t -> t
  val pred : t -> t
  val abs : t -> t
  val dir : t -> t
  val round : t -> t
  val rot : float' -> t
  val x' : float' -> t
  val y' : float' -> t
  val of_int : int' -> t
  val to_int : t -> int'
  val of_float : float' -> t
  val to_float : t -> float'
  val of_string : string' -> t
  val to_string : t -> string'
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val modulo : t -> t -> t
  val pow : t -> t -> t
  val proj : t -> t -> t
  val x_shear : t -> float' -> t
  val y_shear : t -> float' -> t
  val min_bound : t -> t -> t
  val max_bound : t -> t -> t
  val inner : t -> t -> float'
  val compare : t -> t -> int'
  val dpolar : float' -> float' -> t
  val polar : float' -> float' -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( ** ) : t -> t -> t
  val ( <> ) : t -> t -> bool'
  val ( >= ) : t -> t -> bool'
  val ( <= ) : t -> t -> bool'
  val ( > ) : t -> t -> bool'
  val ( < ) : t -> t -> bool'
  val ( = ) : t -> t -> bool'
  val print : unit IO.output -> t -> unit
  val t_printer : t Value_printer.t
end

module type Parameter_type =
sig
  type t
end

(*-----------------------------------------------------------------------*)

module Complex_point =
struct
  type bool' = Bool.t
  type int' = Int.t
  type float' = Float.t
  type string' = String.t

  include Extended_complex

  let to_bezier_point pt = Caml2geom.Point.make pt.re pt.im
  let of_bezier_point bp = { re = Caml2geom.Point.coord bp 0;
                             im = Caml2geom.Point.coord bp 1 }

  let print outp cp =
    let x = cp.re and y = cp.im in
    let format =
      Float.(
        if x < 0. && y < 0. then
          p"x'(%F) + y'(%F)"
        else if x < 0. then
          p"x'(%F) + y' %F"
        else if y < 0. then
          p"x' %F + y'(%F)"
        else
          p"x' %F + y' %F"
      )
    in
    Print.fprintf outp format x y

  let t_printer paren outp c =
    if paren then IO.write outp '(';
    print outp c;
    if paren then IO.write outp ')'
end

(*-----------------------------------------------------------------------*)

module Cubic_base
  (L : module type of List)
  (P : Point_type) =
struct
  type point = P.t
  type t = (P.t * P.t * P.t) L.t

  (* Nodewise and pointwise mapping. *)
  let nodewise = L.map
  let pointwise f = L.map (fun (ih,oc,oh) -> (f ih, f oc, f oh))

  let make_node rel_inhandle on_curve_point rel_outhandle =
    [P.(on_curve_point + rel_inhandle,
        on_curve_point,
        on_curve_point + rel_outhandle)]

  let make_vert_node inhandle_height on_curve_point outhandle_height =
    [P.(x'(re on_curve_point) + y' inhandle_height,
        on_curve_point,
        x'(re on_curve_point) + y' outhandle_height)]

  let make_horiz_node inhandle_pos on_curve_point outhandle_pos =
    [P.(x' inhandle_pos + y'(im on_curve_point),
        on_curve_point,
        x' outhandle_pos + y'(im on_curve_point))]

  let make_up_node on_curve_point =
    P.(make_node (neg i) on_curve_point i)

  let make_down_node on_curve_point =
    P.(make_node i on_curve_point (neg i))

  let make_right_node on_curve_point =
    P.(make_node (neg one) on_curve_point one)

  let make_left_node on_curve_point =
    P.(make_node one on_curve_point (neg one))

  let make_dir_node direction on_curve_point =
    P.(make_node (neg direction) on_curve_point direction)

  let is_empty contour = contour = []
  let is_singleton contour = contour <> [] && L.tl contour = []

  let rev contour = L.rev_map (fun (ih,oc,oh) -> (oh,oc,ih)) contour

  let print_point = P.print
  let point_printer paren outp pt =
    if paren then IO.write outp '(';
    print_point outp pt;
    if paren then IO.write outp ')'

  let print ?first ?last ?sep outp contour =
    let print_node outp (ih,oc,oh) =
      output_string outp "make_node (";
      P.print outp P.(ih - oc);
      output_string outp ") (";
      P.print outp oc;
      output_string outp ") (";
      P.print outp P.(oh - oc);
      output_string outp ")"
    in
    L.print ?first ?last ?sep print_node outp contour

  let t_printer _paren outp contour = print outp contour

  let ( <.> ) contour f = pointwise f contour
  let ( <*> ) contour pt = pointwise (flip P.mul pt) contour
  let ( </> ) contour pt = pointwise (flip P.div pt) contour
  let ( <+> ) contour pt = pointwise (flip P.add pt) contour
  let ( <-> ) contour pt = pointwise (flip P.sub pt) contour
end

(*-----------------------------------------------------------------------*)

(* John Hobby's direction-guessing formulas that were used in Metafont. *)
module Direction_guessing =
struct
  open Lacaml.Impl.D

  let extend_tensions_array ?(default = (1.,1.)) tensions count =
    (* If necessary, extend the tensions array with default values. *)
    let n_tensions = Array.length tensions in
    if n_tensions < count then
      Array.append tensions (Array.make (count - n_tensions) default)
    else
      tensions


  let guess_directions
      ?start_dir
      ?end_dir
      ?(start_curl = 1.)
      ?(end_curl = 1.)
      ?(tensions = [||])
      points =

    let n = Array.length points in
    let tensions = extend_tensions_array tensions (n - 1) in

    (* Reciprocals of the tensions. Allow for negative tensions, which
       can be used to represent an "inflectionless" requirement. *)
    let alpha = Array.map (fun (a,_) -> 1. /. abs_float a) tensions in
    let beta = Array.map (fun (_,b) -> 1. /. abs_float b) tensions in

    (* Point-to-point vectors. *)
    let vector' =
      Array.of_enum
        (map
           (fun k -> Complex.sub points.(k + 1) points.(k))
           (0 --^ (n - 1))
        )
    in

    (* The lengths of those vectors. *)
    let len = Array.map Complex.norm vector' in

    (* The turning angles, in radians. The last turning angle is
       arbitrary and for convenience is set to zero. The vector array
       is extended to express the same arbitrary choice. *)
    let vector = Array.append vector' [| vector'.(n - 2) |] in
    let turn = Array.make (n - 1) 0. in
    iter
      (fun k -> turn.(k - 1) <-
        let angle = Complex.arg (Complex.div vector'.(k) vector'.(k - 1)) in
        if abs_float (angle +. Float.pi) < Float.epsilon then
          Float.pi                      (* Convert -pi to pi *)
        else
          angle
      )
      (1 --^ (n - 1));

    (* Because the spline is non-cyclic, the problem is a tridiagonal
       system of n linear equations. The following arrays will store
       ourm atrices. *)
    let diag = Array.make n 0. in
    let subdiag = Array.make (n - 1) 0. in
    let superdiag = Array.make (n - 1) 0. in
    let right_side = Array.make n 0. in

    begin
      match start_dir with
        | None ->        (* Use curl instead of a direction vector. *)
          (* Use the equations from Metafont, section 277, but with both
             sides multiplied by the denominators, to prevent 0/0 in cases
             where tension = IEEE floating point "infinity". *)
          let chi_numer = alpha.(0) *. alpha.(0) *. start_curl in
          let chi_denom = beta.(0) *. beta.(0) in
          let c = alpha.(0) *. chi_numer +. (3. -. beta.(0)) *. chi_denom in
          let d = (3. -. alpha.(0)) *. chi_numer +. beta.(0) *. chi_denom in
          diag.(0) <- c;
          superdiag.(0) <- d;
          right_side.(0) <- -.d *. turn.(0)

        | Some dir_vector ->     (* Use the given direction vector. *)
          diag.(0) <- 1.;
          right_side.(0) <- Complex.(arg (dir_vector / vector.(0)))
    end;

    begin
      match end_dir with
        | None ->        (* Use curl instead of a direction vector. *)
          (* Use the equations from Metafont, section 277, but with both
             sides multiplied by the denominators, to prevent 0/0 in cases
             where tension = IEEE floating point "infinity". *)
          let chi_numer = beta.(n - 2) *. beta.(n - 2) *. end_curl in
          let chi_denom = alpha.(n - 2) *. alpha.(n - 2) in
          let a = (3. -. beta.(n - 2)) *. chi_numer +. alpha.(n - 2) *. chi_denom in
          let b = beta.(n - 2) *. chi_numer +. (3. -. alpha.(n - 2)) *. chi_denom in
          subdiag.(n - 2) <- a;
          diag.(n - 1) <- b

        | Some dir_vector ->     (* Use the given direction vector. *)
          diag.(n - 1) <- 1.;
          right_side.(n - 1) <- Complex.(arg (dir_vector / vector.(Int.(n - 2))))
    end;

    for k = 1 to n - 2 do
      (* Use the equations from Metafont, section 276, but with both
         sides multiplied by the denominators, to prevent 0/0 in cases
         where tension = IEEE floating point "infinity". *)
      let denom_a_b = beta.(k - 1) *. beta.(k - 1) *. len.(k - 1) in
      let denom_c_d = alpha.(k) *. alpha.(k) *. len.(k) in
      let a = denom_c_d *. alpha.(k - 1) in
      let b = denom_c_d *. (3. -. alpha.(k - 1)) in
      let c = denom_a_b *. (3. -. beta.(k)) in
      let d = denom_a_b *. beta.(k) in
      subdiag.(k - 1) <- a;
      diag.(k) <- b +. c;
      superdiag.(k) <- d;
      right_side.(k) <- -.(b *. turn.(k - 1) +. d *. turn.(k));
    done;

    (* Use LAPACK to solve the system. *)
    let subdiag' = Vec.of_array subdiag in
    let diag' = Vec.of_array diag in
    let superdiag' = Vec.of_array superdiag in
    let right_side' = Mat.mvec_of_array right_side in
    gtsv subdiag' diag' superdiag' right_side';
    let solution = Mat.mvec_to_array right_side' in

    let angle_to_dir k =
      if k = 0 && Option.is_some start_dir then
        Option.get start_dir (* Avoid some arithmetic and roundoff by returning the original. *)
      else if k = n - 1 && Option.is_some end_dir then
        Option.get end_dir (* Avoid some arithmetic and roundoff by returning the original. *)
      else
        Complex_point.(dir vector.(k) * polar 1. solution.(k))
    in
    Array.of_enum (map angle_to_dir (0 --^ n))


  let guess_cycle_directions
      ?(tensions = [||])
      points =

    let n = Array.length points in
    let modn = posmod n in

    let tensions = extend_tensions_array tensions n in

    (* Reciprocals of the tensions. Allow for negative tensions, which
       can be used to represent an "inflectionless" requirement. *)
    let alpha = Array.map (fun (a,_) -> 1. /. abs_float a) tensions in
    let beta = Array.map (fun (_,b) -> 1. /. abs_float b) tensions in

    (* Point-to-point vectors. *)
    let vector =
      Array.of_enum
        (map
           (fun k -> Complex.sub points.(modn (k + 1)) points.(k))
           (0 --^ n)
        )
    in

    (* The lengths of those vectors. *)
    let len = Array.map Complex.norm vector in

    (* The turning angles, in radians. *)
    let turn = Array.make n 0. in
    iter
      (fun k -> turn.(modn (k - 1)) <-
        let angle = Complex.arg (Complex.div vector.(k) vector.(modn (k - 1))) in
        if abs_float (angle +. Float.pi) < Float.epsilon then
          Float.pi                      (* Convert -pi to pi *)
        else
          angle
      )
      (0 --^ n);

    (* We need to solve a system of linear equations that is _almost_
       but not quite tridiagonal in general. For now, at least, let's
       just solve it as a general system. The following arrays will
       store the matrices. (If the points are many, the left side will
       be sparse.) *)
    let left_side = Array.make_matrix n n 0. in
    let right_side = Array.make n 0. in

    (* Use the equations from Metafont, section 276, but with both
       sides multiplied by the denominators, to prevent 0/0 in cases
       where tension = IEEE floating point "infinity". *)
    for k = 0 to n - 1 do
      let denom_a_b = beta.(modn (k - 1)) *. beta.(modn (k - 1)) *. len.(modn (k - 1)) in
      let denom_c_d = alpha.(k) *. alpha.(k) *. len.(k) in
      let a = denom_c_d *. alpha.(modn (k - 1)) in
      let b = denom_c_d *. (3. -. alpha.(modn (k - 1))) in
      let c = denom_a_b *. (3. -. beta.(k)) in
      let d = denom_a_b *. beta.(k) in
      left_side.(k).(modn (k - 1)) <- a;
      left_side.(k).(k) <- b +. c;
      left_side.(k).(modn (k + 1)) <- left_side.(k).(modn (k + 1)) +. d;
      right_side.(k) <- -.(b *. turn.(modn (k - 1)) +. d *. turn.(k));
    done;

    (* Use LAPACK to solve the system. *)
    let left_side' = Mat.of_array left_side in
    let right_side' = Mat.mvec_of_array right_side in
    gesv left_side' right_side';
    let solution = Mat.mvec_to_array right_side' in

    let angle_to_dir k =
        Complex_point.(dir vector.(k) * polar 1. solution.(k))
    in
    Array.of_enum (map angle_to_dir (0 --^ n))
end

(*-----------------------------------------------------------------------*)

(*
  See http://paulbourke.net/geometry/lineline2d/ --
  "Intersection point of two lines (2 dimensions)"
*)
let find_intersection_of_lines
    ?(first_is_segment = false)
    ?(second_is_segment = false)
    (p1,p2) (p3,p4) =
  let (x1,y1) = Complex_point.(re p1, im p1) in
  let (x2,y2) = Complex_point.(re p2, im p2) in
  let (x3,y3) = Complex_point.(re p3, im p3) in
  let (x4,y4) = Complex_point.(re p4, im p4) in
  let denom = (y4 -. y3) *. (x2 -. x1) -. (x4 -. x3) *. (y2 -. y1) in
  let numer_a = (x4 -. x3) *. (y1 -. y3) -. (y4 -. y3) *. (x1 -. x3) in
  let numer_b = (x2 -. x1) *. (y1 -. y3) -. (y2 -. y1) *. (x1 -. x3) in
  let ua = numer_a /. denom in
  let ub = numer_b /. denom in
  let x = x1 +. ua *. (x2 -. x1) in
  let y = y1 +. ua *. (y2 -. y1) in
  let pa =
    if first_is_segment then
      if 0. <= ua && ua <= 1. then
        Complex.({ re = x; im = y })
      else
        Complex.({ re = nan; im = nan })
    else
      Complex.({ re = x; im = y })
  in
  let pb =
    if second_is_segment then
      if 0. <= ub && ub <= 1. then
        Complex.({ re = x; im = y })
      else
        Complex.({ re = nan; im = nan })
    else
      Complex.({ re = x; im = y })
  in
  (pa, pb)

let remove_inflection point1 point2 control1 control2 =
  let (pa, pb) =
    find_intersection_of_lines
      ~first_is_segment:true
      ~second_is_segment:true
      (point1, control1) (point2, control2)
  in
  let new_control1 = if Complex.norm pa < infinity then pa else control1 in
  let new_control2 = if Complex.norm pb < infinity then pb else control2 in
  (new_control1, new_control2)

(* A function (or factor) due to John Hobby. See the METAFONTbook *)
let f_hobby theta phi =
  let (sintheta, costheta) = (sin theta, cos theta) in
  let (sinphi, cosphi) = (sin phi, cos phi) in
  let root2 = sqrt 2. in
  let root5 = sqrt 5. in
  let top = 2. +. root2 *. (sintheta -. sinphi /. 16.) *. (sinphi -. sintheta /. 16.) *. (costheta -. cosphi) in
  let bottom = 3. *. (1. +. 0.5 *. (root5 -. 1.) *. costheta +. 0.5 *. (3. -. root5) *. cosphi) in
  top /. bottom

let tensions_to_controls ?(no_inflection = false) point1 point2 dir1 dir2 tension1 tension2 =
  Complex_point.(
    let chord = point2 - point1 in
    let theta = arg (dir1 / chord) in
    let phi = arg (chord / dir2) in
    let control1 = point1 + (polar 1. theta) * chord * x'(f_hobby theta phi /. tension1) in
    let control2 = point2 - (polar 1. (-.phi)) * chord * x'(f_hobby phi theta /. tension2) in
    if no_inflection then
      remove_inflection point1 point2 control1 control2
    else
      (control1, control2)
  )

(*-----------------------------------------------------------------------*)

let bezier_curve_to_four_complexes bez =
  let points = Caml2geom.Bezier_curve.to_complex_array bez in
  let len = Array.length points in
  assert (len = 2 || len = 4); (* Only linears and cubics are allowed. *)
  if len = 2 then
    (points.(0), points.(0), points.(1), points.(1))
  else
    (points.(0), points.(1), points.(2), points.(3))

module Cubic =
struct
  include Cubic_base(List)(Complex_point)
  module L = List
  module P = Complex_point

  let time_error = Invalid_argument "time out of range"

  let linear_tolerance = ref 0.001
  let basis_conversion_tolerance = ref 0.001

  let points_coincide ?(tol = !linear_tolerance) p1 p2 =
    P.(norm (p1 - p2)) < tol

  let _nodes_coincide ?tol (_, p1, _) (_, p2, _) =
    points_coincide ?tol p1 p2

  let is_closed ?tol contour =
    L.tl contour <> [] &&
      _nodes_coincide ?tol (L.first contour) (L.last contour)

  let close ?tol contour =
    if is_closed ?tol contour then
      contour
    else
      contour @ [L.first contour]

  let unclose ?tol contour =
    (* FIXME: There may be coincident points. Have this loop as long
       as the contour remains closed. *)
    if is_closed ?tol contour then
      L.drop (L.length contour - 1) contour
    else
      contour

  let bezier_curve ?(pos = 0) contour =
    let c = L.drop pos contour in
    let (_, on_curve1, outhandle1) = L.hd c in
    let (inhandle2, on_curve2, _) = L.hd (L.tl c) in
    let p0 = P.to_bezier_point on_curve1 in
    let p1 = P.to_bezier_point outhandle1 in
    let p2 = P.to_bezier_point inhandle2 in
    let p3 = P.to_bezier_point on_curve2 in
    Caml2geom.Cubic_bezier.of_four_points p0 p1 p2 p3

  let of_bezier_curves bez1 bez2 =
    let (_, _, ih, oc) = bezier_curve_to_four_complexes bez1 in
    let (_, oh, _, _) = bezier_curve_to_four_complexes bez2 in
    [(ih, oc, oh)]

  let curve_bounds ?(fast = false) ?pos contour =
    let bounds_func =
      if fast then
        Caml2geom.Cubic_bezier.bounds_fast
      else
        Caml2geom.Cubic_bezier.bounds_exact
    in
    let bez = bezier_curve ?pos contour in
    let rect = bounds_func bez in
    P.(of_bezier_point (Caml2geom.Rect.min rect),
       of_bezier_point (Caml2geom.Rect.max rect))

  let curve_point_at ?pos contour time =
    let bez = bezier_curve ?pos contour in
    let pt = Caml2geom.Cubic_bezier.point_at bez time in
    P.of_bezier_point pt

  let curve_times_at_x ?pos contour x_coord =
    let bez = bezier_curve ?pos contour in
    Caml2geom.Cubic_bezier.roots bez x_coord Caml2geom.Coord.X

  let curve_times_at_y ?pos contour y_coord =
    let bez = bezier_curve ?pos contour in
    Caml2geom.Cubic_bezier.roots bez y_coord Caml2geom.Coord.Y

  let curve_crossings ?pos1 contour1 ?pos2 contour2 =
    let bez1 = bezier_curve ?pos:pos1 contour1 in
    let bez2 = bezier_curve ?pos:pos2 contour2 in
    Caml2geom.Cubic_bezier.crossings bez1 bez2

  let subdivide_curve ?(pos = 0) contour time =
    let c = L.drop pos contour in
    let (ih1, _, _) = L.hd c in
    let (_, _, oh2) = L.hd (L.tl c) in
    let bez = bezier_curve c in
    let (bez1, bez2) = Caml2geom.Cubic_bezier.subdivide bez time in
    let (a0, a1, a2, a3) = bezier_curve_to_four_complexes bez1 in
    let (_b0, b1, b2, b3) = bezier_curve_to_four_complexes bez2 in
    let node1 = (ih1, a0, a1) in
    let node2 = (a2, a3, b1) in
    let node3 = (b2, b3, oh2) in
    ([node1; node2], [node2; node3])

  let curve_portion ?(pos = 0) contour time1 time2 =
    let c = L.drop pos contour in
    let reversed = time2 < time1 in
    let (t1,t2) = if reversed then (time2, time1) else (time1, time2) in
    let portion =
      if t1 = 0. then
        if t2 = 1. then
          L.take 2 c
        else
          fst (subdivide_curve c t2)
      else if t2 = 1. then
        snd (subdivide_curve c t1)
      else
        let (c1,_) = subdivide_curve c t2 in
        snd (subdivide_curve c1 (t1 /. t2))
    in
    if reversed then
      rev portion
    else
      portion

  let _curve_derivative ?pos contour =
    let bez = bezier_curve ?pos contour in
    Caml2geom.Cubic_bezier.derivative bez

  let curve_extrema_and_inflections ?pos contour =
    let deriv = _curve_derivative ?pos contour in
    let x_times = Caml2geom.Bezier_curve.roots deriv 0. Caml2geom.Coord.X in
    let y_times = Caml2geom.Bezier_curve.roots deriv 0. Caml2geom.Coord.Y in
    (x_times, y_times)

  let rec to_cubic_beziers contour =
    match contour with
      | [] | [_] -> []
      | _ :: remaining as c ->
        bezier_curve c :: to_cubic_beziers remaining

  let to_path contour =
    let curve_list = to_cubic_beziers contour in
    let (_, first_point, _) = L.hd contour in
    let path = Caml2geom.Path.make (P.to_bezier_point first_point) in
    List.iter
      (Caml2geom.Path.append_curve ~stitch:Caml2geom.Path.NO_STITCHING path -|
          Caml2geom.Cubic_bezier.to_curve)
      curve_list;
    path

  let of_path
      ?(tol = !basis_conversion_tolerance)
      ?(rel_inhandle = P.zero)
      ?(rel_outhandle = P.zero)
      path =
    let bez_curves = Caml2geom.Path.to_cubic_beziers_open path tol in
    let curves = List.map bezier_curve_to_four_complexes bez_curves in
    let (a0, _, _, _) = List.first curves in
    let (_, _, _, b3) = List.last curves in
    let pre_first = P.(zero, zero, a0 + rel_inhandle, a0) in
    let post_last = P.(b3, b3 + rel_outhandle, zero, zero) in
    let rec make_nodes =
      function
        | [] | [_] -> []
        | curve1 :: curve2 :: remaining ->
          let (_, _, ih, oc) = curve1 in
          let (_, oh, _, _) = curve2 in
          (ih, oc, oh) :: make_nodes (curve2 :: remaining)
    in
    make_nodes (pre_first :: (curves @ [post_last]))

  let bounds ?fast contour =
    let most_positive = Complex.({ re = infinity; im = infinity }) in
    let most_negative = Complex.({ re = neg_infinity; im = neg_infinity }) in
    let rec get_bounds contour =
      match contour with
        | [] | [_] -> []
        | _ :: remaining as c ->
          curve_bounds ?fast c :: get_bounds remaining
    in
    let bounds_list = get_bounds contour in
    List.fold_left
      (fun (min1, max1) (min2, max2) -> P.(min_bound min1 min2,
                                           max_bound max1 max2))
      (most_positive, most_negative)
      bounds_list

  let overall_bounds ?fast enum_of_contours =
    if Enum.is_empty enum_of_contours then
      invalid_arg "empty enum of contours"
    else
      let most_positive = Complex.({ re = infinity; im = infinity }) in
      let most_negative = Complex.({ re = neg_infinity; im = neg_infinity }) in
      fold
        (fun (current_min, current_max) contour ->
          let (this_min, this_max) = bounds ?fast contour in
          P.(min_bound current_min this_min,
             max_bound current_max this_max))
        (most_positive, most_negative)
        enum_of_contours

  let rec subdivide contour time =
    if time < 0. then
      raise time_error;
    match contour with
      | [] -> raise time_error
      | [node] when time = 0. -> ([node], [node])
      | node :: remaining when 1. <= time ->
        let (contour1, contour2) = subdivide remaining (time -. 1.) in
        (node :: contour1, contour2)
      | c ->
        let (c1, c2) = subdivide_curve c time in
        (c1, c2 @ L.drop 2 c)

  let portion ?(tol = !basis_conversion_tolerance) contour time1 time2 =
    (* FIXME: Either give this function the ability to handle cycles,
       or write something cycle-handling in terms of it. *)
    let node_count = L.length contour in
    let curve_count = node_count - 1 in
    let max_time = float_of_int curve_count in
    let reversed = time2 < time1 in
    let (t1,t2) = if reversed then (time2, time1) else (time1, time2) in
    let t1 = max t1 0. in
    let t2 = min t2 max_time in
    let t1_floor = Float.floor t1 in
    let t2_floor = Float.floor t2 in
    let t2 = if t2 -. t2_floor < tol then t2_floor else t2 in
    let t1_int = int_of_float t1_floor in
    let t2_int = int_of_float t2_floor in
    let c = L.drop t1_int contour in
    let part =
      if t1_int = t2_int || (t1_int + 1 = t2_int && t2 = t2_floor) then
        curve_portion c (t1 -. t1_floor) (t2 -. t1_floor)
      else
        let (_,c1) = subdivide_curve c (t1 -. t1_floor) in
        if t2 = t2_floor then
          let tail = L.take (t2_int - t1_int) (L.tl c) in
          L.append c1 tail
        else
          let (c2,_) = subdivide_curve ~pos:(t2_int - t1_int) c (t2 -. t2_floor) in
          let c_middle = L.take (t2_int - t1_int) (L.tl c) in
          L.append c1 (L.append (L.tl c_middle) (L.tl c2))
    in
    if reversed then
      rev part
    else
      part

  let join ?tol contour1 contour2 =
    if contour1 == contour2 then
      close ?tol contour1
    else
      let rev1 = L.rev contour1 in
      let last1 = L.first rev1 in
      let first2 = L.first contour2 in
      if _nodes_coincide ?tol last1 first2 then
        let (ih1, oc1, _) = last1 in
        let (_, _, oh2) = first2 in
        let joined_node = (ih1, oc1, oh2) in
        L.rev_append (joined_node :: L.tl rev1) (L.tl contour2)
      else
        L.append contour1 contour2

  let point_at contour time =
    let pt = Caml2geom.Path.point_at (to_path contour) time in
    P.of_bezier_point pt

  let times_at_x contour x_coord =
    Caml2geom.Path.roots (to_path contour) x_coord Caml2geom.Coord.X

  let times_at_y contour y_coord =
    Caml2geom.Path.roots (to_path contour) y_coord Caml2geom.Coord.Y

  let crossings contour1 contour2 =
    Caml2geom.Path.crossings (to_path contour1) (to_path contour2)

  let modify_inhandle path vector =
    Complex_point.(
      let (ih, oc, oh) = L.hd path in
      let new_ih = oc + dir (ih - oc) * vector in
      (new_ih, oc, oh) :: L.tl path
    )

  let modify_outhandle path vector =
    Complex_point.(
      let rev_path = L.rev path in
      let (ih, oc, oh) = L.hd rev_path in
      let new_oh = oc + dir (oh - oc) * vector in
      L.rev ((ih, oc, new_oh) :: L.tl rev_path)
    )

  let remove_inflection_from_curve ?(pos = 0) contour =
    let (h, c) = L.split_at pos contour in
    let (ih1, oc1, oh1) = L.hd c in
    let (ih2, oc2, oh2) = L.hd (L.tl c) in
    let (pa, pb) =
      find_intersection_of_lines
        ~first_is_segment:true
        ~second_is_segment:true
        (oc1, oh1) (oc2, ih2)
    in
    let new_oh1 = if Complex.norm pa < infinity then pa else oh1 in
    let new_ih2 = if Complex.norm pb < infinity then pb else ih2 in
    h @ [(ih1, oc1, new_oh1); (new_ih2, oc2, oh2)] @ L.tl (L.tl c)

  (* You can use Metafont-style "tension" to set handle lengths. *)
  let apply_tensions ?(pos = 0) ?(no_inflection = false) contour tension1 tension2 =
    Complex_point.(
      let (h, c) = L.split_at pos contour in
      let (ih1, oc1, oh1) = L.hd c in
      let (ih2, oc2, oh2) = L.hd (L.tl c) in
      let chord = oc2 - oc1 in
      let dir1 = oh1 - oc1 in
      let dir2 = oc2 - ih2 in
      let theta = arg (dir1 / chord) in
      let phi = arg (chord / dir2) in
      let new_oh1 = oc1 + (polar 1. theta) * chord * x'(f_hobby theta phi /. tension1) in
      let new_ih2 = oc2 - (polar 1. (~-.phi)) * chord * x'(f_hobby phi theta /. tension2) in
      let c' = [(ih1, oc1, new_oh1); (new_ih2, oc2, oh2)] @ L.tl (L.tl c) in
      if no_inflection then
        h @ remove_inflection_from_curve c'
      else
        h @ c'
    )

  let apply_tension ?pos ?no_inflection contour tension =
    apply_tensions ?pos ?no_inflection contour tension tension

  (* FIXME: Bring these sorts of functions into harmony with their
     Metacubic equivalents. Then remove these old versions. *)
  let join_with_tensions ?no_inflection tension1 tension2 contour1 contour2 =
    let rev1 = L.rev contour1 in
    L.rev_append (L.tl rev1)
      (apply_tensions ?no_inflection (L.hd rev1 :: contour2) tension1 tension2)

  let join_with_tension ?no_inflection tension contour1 contour2 =
    join_with_tensions ?no_inflection tension tension contour1 contour2

  let close_with_tensions ?tol ?no_inflection tension1 tension2 contour =
    if is_closed ?tol contour then
      invalid_arg "close_with_tensions: the contour is closed already";
    let rev_contour = L.rev contour in
    let tensioned_nodes =
      apply_tensions ?no_inflection [L.hd rev_contour; L.hd contour] tension1 tension2
    in
    let contour' = L.rev (L.tl rev_contour) @ tensioned_nodes in
    L.hd (L.tl tensioned_nodes) :: L.tl contour'

  let close_with_tension ?tol ?no_inflection tension contour =
    if is_closed ?tol contour then
      invalid_arg "close_with_tension: the contour is closed already";
    close_with_tensions ?no_inflection tension tension contour

  let to_point_bool_list contour =
    let node_to_list (ih,oc,oh) = [(ih, false); (oc, true); (oh, false)] in
    L.flatten (L.map node_to_list contour)

  let print_python_contour_code ?variable outp contour =
    let c =
      if is_closed contour then
        L.rev (L.tl (L.rev contour))
      else
        contour
    in
    let point_list = to_point_bool_list c in
    match variable with
      | None ->
        (* This branch doesn't set the closedness. *)
        output_string outp "(fontforge.contour()";
        List.iter
          Complex.(fun (pt, on_curve) ->
            if on_curve then
              Print.fprintf outp p" + fontforge.point(%f,%f)" pt.re pt.im
            else
              Print.fprintf outp p" + fontforge.point(%f,%f, False)" pt.re pt.im)
          point_list;
        output_string outp ")"

      | Some var_name ->
        Print.fprintf outp p"%s = fontforge.contour()\n" var_name;
        List.iter
          Complex.(fun (pt, on_curve) ->
            if on_curve then
              Print.fprintf outp p"%s += fontforge.point(%f,%f)\n" var_name pt.re pt.im
            else
              Print.fprintf outp p"%s += fontforge.point(%f,%f, False)\n" var_name pt.re pt.im)
          point_list;
        Print.fprintf outp p"%s.closed = %s\n" var_name
          (if is_closed contour then "True" else "False")

  let ( <@> ) contour1 = join contour1

  let ( <@@ ) contour t_or_f =
    (if t_or_f then close else unclose) contour

  let ( <@~~.> ) contour1 (tension1, tension2) contour2 =
    join_with_tensions tension1 tension2 contour1 contour2

  let ( <@--.> ) contour1 (tension1, tension2) contour2 =
    join_with_tensions ~no_inflection:true tension1 tension2 contour1 contour2

  let ( <@~.> ) contour1 tension contour2 =
    join_with_tension tension contour1 contour2

  let ( <@-.> ) contour1 tension contour2 =
    join_with_tension ~no_inflection:true tension contour1 contour2

  let ( <.~~@> ) f a = f a
  let ( <.~@> ) f a = f a
  let ( <.--@> ) f a = f a
  let ( <.-@> ) f a = f a

  let ( <@~> ) contour1 = join_with_tension 1. contour1
  let ( <@-> ) contour1 = join_with_tension ~no_inflection:true 1. contour1

  let ( <~~@@ ) contour (tension1, tension2) =
    close_with_tensions tension1 tension2 contour

  let ( <--@@ ) contour (tension1, tension2) =
    close_with_tensions ~no_inflection:true tension1 tension2 contour

  let ( <~@@ ) contour tension =
    close_with_tension tension contour

  let ( <-@@ ) contour tension =
    close_with_tension ~no_inflection:true tension contour
end

(*-----------------------------------------------------------------------*)

module Metacubic =
struct
  (* FIXME: Subroutinize the code more, and otherwise increase its
     terseness. *)

  type knot_side =
    [
    | `Ctrl of Complex.t               (* control point *)
    | `Dir of Complex.t * float        (* (direction, tension) *)
    | `Curl of float * float           (* (curl parameter, tension) *)
    | `Open of float                   (* tension *)
    ]

  type knot = knot_side * Complex.t * knot_side
  type t = knot Vect.t

  let print_knot_side outp ks =
    match ks with
      | `Ctrl p ->
        output_string outp "`Ctrl (";
        Complex_point.print outp p;
        IO.write outp ')'
      | `Dir (p,t) ->
        output_string outp "`Dir (";
        Complex_point.print outp p;
        output_string outp ", ";
        Float.print outp t;
        IO.write outp ')'
      | `Curl (u,t) ->
        output_string outp "`Curl (";
        Float.print outp u;
        output_string outp ", ";
        Float.print outp t;
        IO.write outp ')'
      | `Open t ->
        output_string outp "`Open ";
        Float.print outp t

  let knot_side_printer paren outp ks =
    if paren then IO.write outp '(';
    print_knot_side outp ks;
    if paren then IO.write outp ')'

  let print_knot outp (incoming, point, outgoing) =
    IO.write outp '(';
    print_knot_side outp incoming;
    output_string outp ", ";
    Complex_point.print outp point;
    output_string outp ", ";
    print_knot_side outp outgoing;
    IO.write outp ')'

  let knot_printer _paren outp ks = print_knot outp ks

  let print = Vect.print print_knot
  let t_printer _paren outp contour = print outp contour

  let knot_side_tension ks =
    match ks with
      | `Ctrl _ -> failwith "knot_side_tension"
      | `Dir (_, t) -> t
      | `Curl (_, t) -> t
      | `Open t -> t

  let set_knot_side_tension ks tension =
    match ks with
      | `Ctrl _ -> ks
      | `Dir (d, _) -> `Dir (d, tension)
      | `Curl (u, _) -> `Curl (u, tension)
      | `Open _ -> `Open tension

  let knot_incoming_dir ?tol (incoming, point, _) =
    match incoming with
      | `Ctrl ctrl when not (Cubic.points_coincide ?tol point ctrl) ->
        Complex_point.(dir (point - ctrl))
      | `Dir (d, _) -> Complex_point.dir d
      | `Ctrl _ | `Curl _ | `Open _ -> failwith "knot_incoming_dir"

  let knot_outgoing_dir ?tol (_, point, outgoing) =
    match outgoing with
      | `Ctrl ctrl when not (Cubic.points_coincide ?tol ctrl point) ->
        Complex_point.(dir (ctrl - point))
      | `Dir (d, _) -> Complex_point.dir d
      | `Ctrl _ | `Curl _ | `Open _ -> failwith "knot_outgoing_dir"

  let set_knot_incoming_dir ?tol ?dir knot =
    let (incoming, point, outgoing) = knot in
    let tension = try knot_side_tension incoming with _ -> 1. in
    let incoming_dir =
      match dir with
        | Some d -> Complex_point.dir d
        | None ->
          try
            knot_outgoing_dir ?tol knot
          with Failure "knot_outgoing_dir" ->
            failwith "set_knot_incoming_dir"
    in
    (`Dir (incoming_dir, tension), point, outgoing)

  let set_knot_outgoing_dir ?tol ?dir knot =
    let (incoming, point, outgoing) = knot in
    let tension = try knot_side_tension outgoing with _ -> 1. in
    let outgoing_dir =
      match dir with
        | Some d -> Complex_point.dir d
        | None ->
          try
            knot_incoming_dir ?tol knot
          with Failure "knot_incoming_dir" ->
            failwith "set_knot_outgoing_dir"
    in
    (incoming, point, `Dir (outgoing_dir, tension))

  let _knots_coincide ?tol (_,point1,_) (_,point2,_) =
    Cubic.points_coincide ?tol point1 point2

  let is_closed ?tol contour =
    Vect.length contour <> 1 &&
      _knots_coincide ?tol (Vect.at contour 0) (Vect.at contour (Vect.length contour - 1))

  let unclose ?tol contour =
    (* FIXME: There may be coincident points. Have this loop as long
       as the contour remains closed. *)
    if is_closed ?tol contour then
      Vect.sub 0 (Vect.length contour - 1) contour
    else
      contour

  let set_incoming_tension ?tol contour tension =
    if is_closed ?tol contour then
      failwith "set_incoming_tension";
    Vect.modify contour 0
      (fun (incoming, point, outgoing) ->
        set_knot_side_tension incoming tension, point, outgoing)

  let set_outgoing_tension ?tol contour tension =
    if is_closed ?tol contour then
      failwith "set_outgoing_tension";
    Vect.modify contour (Vect.length contour - 1)
      (fun (incoming, point, outgoing) ->
        incoming, point, set_knot_side_tension outgoing tension)

  let join ?tol ?in_tension ?out_tension ?tension contour1 contour2 =
    if (Option.is_some tension && Option.is_some in_tension) ||
      (Option.is_some tension && Option.is_some out_tension) then
      invalid_arg "join";
    let t1 =
      if Option.is_some tension then
        Option.get tension
      else if Option.is_some out_tension then
        Option.get out_tension
      else
        1.
    in
    let t2 =
      if Option.is_some tension then
        Option.get tension
      else if Option.is_some in_tension then
        Option.get in_tension
      else
        1.
    in
    if contour1 == contour2 then
      if not (is_closed ?tol contour1) then
        let contour = set_incoming_tension (set_outgoing_tension contour1 t1) t2 in
        Vect.append (Vect.at contour 0) contour
      else
        contour1
    else
      let last1 = Vect.at contour1 (Vect.length contour1 - 1) in
      let first2 = Vect.at contour2 0 in
      if _knots_coincide ?tol last1 first2 then
        let (incoming, point, _) = last1 in
        let (_, _, outgoing) = first2 in
        let joined_knot = (incoming, point, outgoing) in
        Vect.concat (Vect.set contour1 (Vect.length contour1 - 1) joined_knot)
          (Vect.sub 1 (Vect.length contour2 - 1) contour2)
      else
        Vect.concat
          (set_outgoing_tension contour1 t1)
          (set_incoming_tension contour2 t2)

  let close ?tol ?in_tension ?out_tension ?tension contour =
    join ?tol ?in_tension ?out_tension ?tension contour contour

(* FIXME: May need to restrict these to certain kinds of transformations.

  (* Knotwise mapping. *)
  let knotwise = Vect.map

  (* Pointwise mapping. *)
  let pointwise f =
    Vect.map
      (fun (incoming, point, outgoing) ->
        let new_point = f point in
        let new_incoming =
          match incoming with
            | `Ctrl ctrl -> `Ctrl (f ctrl)
            | `Dir (d,t) -> FIXME
            | `Curl _ | `Open _ -> incoming
        in
        let new_outgoing =
          match outgoing with
            | `Ctrl ctrl -> `Ctrl (f ctrl)
            | `Dir (d,t) -> FIXME
            | `Curl _ | `Open _ -> outgoing
        in
        (new_incoming, new_point, new_outgoing)
      )
*)

  let rev contour =
    let n = Vect.length contour in
    Vect.init
      n
      (fun k ->
        let (incoming, point, outgoing) = Vect.at contour (n - k - 1) in
        let new_incoming =
          match outgoing with
            | `Ctrl _ | `Curl _ | `Open _ -> outgoing
            | `Dir (d,t) -> `Dir (Complex.neg d, t)
        in
        let new_outgoing =
          match incoming with
            | `Ctrl _ | `Curl _ | `Open _ -> incoming
            | `Dir (d,t) -> `Dir (Complex.neg d, t)
        in
        (new_incoming, point, new_outgoing)
      )

  let join_coincident_knots ?tol contour =
    let n = Vect.length contour in
    fold
      (fun c k ->
        let (incoming1, point1, _) = Vect.at c k in
        let (_, point2, outgoing2) = Vect.at c (k + 1) in
        if Cubic.points_coincide ?tol point1 point2 then
          let c' = Vect.set c k (incoming1, point1, `Ctrl point1) in
          let c' = Vect.set c' (k + 1) (`Ctrl point2, point2, outgoing2) in
          c'
        else
          c
      )
      contour
      (0 --^ (n - 1))

  let find_first_breakpoint contour =
    let n = Vect.length contour in
    let rec find_it k =
      if k = n then
        n                               (* No breakpoints. *)
      else
        match Vect.at contour k with
          | (`Open t1, _, `Open t2) when t1 = infinity && t2 = infinity -> k
          | (`Open _, _, `Open _) -> find_it (k + 1)
          | _ -> k
    in
    find_it 0

  let find_next_breakpoint contour k last_bp =
    let n = Vect.length contour in
    let rec find_it j =
      if j = last_bp then
        j
      else
        match Vect.at contour j with
          | (`Open t1, _, `Open t2) when t1 = infinity && t2 = infinity -> k
          | (`Open _, _, `Open _) -> find_it ((j + 1) mod n)
          | _ -> j
    in
    find_it ((k + 1) mod n)

  let fill_in_cycle_control_points ?tol contour =
    let contour = unclose ?tol contour in
    let n = Vect.length contour in
    let points = Array.init n (fun k -> let (_,pt,_) = Vect.at contour k in pt) in
    let tensions =
      Array.init n
        (fun k ->
          let t1 =
            match Vect.at contour k with
              | (_, _, `Open t1) -> t1
              | _ -> failwith "fill_in_cycle_control_points"
          in
          let t2 =
            match Vect.at contour ((k + 1) mod n) with
              | (`Open t2, _, _) -> t2
              | _ -> failwith "fill_in_cycle_control_points"
          in
          (t1,t2))
    in
    let dirs = Direction_guessing.guess_cycle_directions ~tensions points in
    let contour =
      fold
        (fun c k ->
          let (incoming1, point1, _) = Vect.at c k in
          let (_, point2, outgoing2) = Vect.at c ((k + 1) mod n) in
          let dir1 = dirs.(k) in
          let dir2 = dirs.((k + 1) mod n) in
          let (tension1, tension2) = tensions.(k) in
          let no_inflection = tension1 < 0. || tension2 < 0. in
          let (control1, control2) =
            tensions_to_controls ~no_inflection point1 point2 dir1 dir2
              (abs_float tension1) (abs_float tension2)
          in
          let c' = Vect.set c k (incoming1, point1, `Ctrl control1) in
          let c' = Vect.set c' ((k + 1) mod n) (`Ctrl control2, point2, outgoing2) in
          c')
        contour
        (0 --^ n)
    in
    close ?tol contour

  let rec fix_breakpoints ?tol contour start_bp end_bp =
    let n = Vect.length contour in
    let modn = posmod n in
    let (incoming1, point1, outgoing1) = Vect.at contour start_bp in
    let (incoming2, point2, outgoing2) = Vect.at contour end_bp in
    match (outgoing1, incoming2) with
      | (`Dir _, `Dir _) | (`Dir _, `Curl _) |
          (`Curl _, `Dir _) | (`Curl _, `Curl _) | (`Ctrl _, `Ctrl _) -> contour

      | (`Ctrl ctrl, _) ->
        let (ks, _, _) = Vect.at contour (modn (start_bp + 1)) in
        let tension = knot_side_tension ks in
        if Cubic.points_coincide ?tol point1 ctrl then
          fix_breakpoints ?tol
            (Vect.set contour start_bp (incoming1, point1, `Curl (1., tension)))
            start_bp end_bp
        else
          let d = Complex_point.(dir (ctrl - point1)) in
          fix_breakpoints ?tol
            (Vect.set contour start_bp (incoming1, point1, `Dir (d, tension)))
            start_bp end_bp

      | (_, `Ctrl ctrl) ->
        let (ks, _, _) = Vect.at contour (modn (end_bp - 1)) in
        let tension = knot_side_tension ks in
        if Cubic.points_coincide ?tol ctrl point2 then
          fix_breakpoints ?tol
            (Vect.set contour end_bp (`Curl (1., tension), point2, outgoing2))
            start_bp end_bp
        else
          let d = Complex_point.(dir (point2 - ctrl)) in
          fix_breakpoints ?tol
            (Vect.set contour end_bp (`Dir (d, tension), point2, outgoing2))
            start_bp end_bp

      | (`Open tension, _) ->
        begin
          match incoming1 with
            | `Dir (d,_) ->
              fix_breakpoints ?tol
                (Vect.set contour start_bp (incoming1, point1, `Dir (d, tension)))
                start_bp end_bp
            | `Ctrl ctrl when not (Cubic.points_coincide ?tol point1 ctrl) ->
              let d = Complex_point.dir (Complex.sub point1 ctrl) in
              fix_breakpoints ?tol
                (Vect.set contour start_bp (incoming1, point1, `Dir (d, tension)))
                start_bp end_bp
            | _ ->
              fix_breakpoints ?tol
                (Vect.set contour start_bp (incoming1, point1, `Curl (1., tension)))
                start_bp end_bp
        end

      | (_, `Open tension) ->
        begin
          match outgoing2 with
            | `Dir (d,_) ->
              fix_breakpoints ?tol
                (Vect.set contour end_bp (`Dir (d, tension), point2, outgoing2))
                start_bp end_bp
            | `Ctrl ctrl when not (Cubic.points_coincide ?tol ctrl point2) ->
              let d = Complex_point.dir (Complex.sub ctrl point2) in
              fix_breakpoints ?tol
                (Vect.set contour end_bp (`Dir (d, tension), point2, outgoing2))
                start_bp end_bp
            | _ ->
              fix_breakpoints ?tol
                (Vect.set contour end_bp (`Curl (1., tension), point2, outgoing2))
                start_bp end_bp
        end

  let fill_in_length1_segment_control_points ?tol contour start_bp end_bp =
    let contour = fix_breakpoints ?tol contour start_bp end_bp in
    let (incoming1, point1, outgoing1) = Vect.at contour start_bp in
    let (incoming2, point2, outgoing2) = Vect.at contour end_bp in
    match (outgoing1, incoming2) with
      | (`Ctrl _, `Ctrl _) -> contour

      | (`Curl _, `Curl _) ->
          (* A straight line segment. *)
        let c = Vect.set contour start_bp (incoming1, point1, `Ctrl point1) in
        Vect.set c end_bp (`Ctrl point2, point2, outgoing2)

      | (`Dir (d1,t1), `Dir (d2,t2)) ->
        let (ctrl1, ctrl2) =
          tensions_to_controls ~no_inflection:(t1 < 0. || t2 < 0.) point1 point2 d1 d2
            (abs_float t1) (abs_float t2)
        in
        let c = Vect.set contour start_bp (incoming1, point1, `Ctrl ctrl1) in
        Vect.set c end_bp (`Ctrl ctrl2, point2, outgoing2)

      | (`Dir (d1, t1), `Curl (u2, t2)) ->
        let dirs =
          Direction_guessing.guess_directions ~start_dir:d1 ~end_curl:u2
            ~tensions:[|(t1,t2)|] [|point1; point2|]
        in
        let (ctrl1, ctrl2) =
          tensions_to_controls ~no_inflection:(t1 < 0. || t2 < 0.) point1 point2
            dirs.(0) dirs.(1) (abs_float t1) (abs_float t2)
        in
        let c = Vect.set contour start_bp (incoming1, point1, `Ctrl ctrl1) in
        Vect.set c end_bp (`Ctrl ctrl2, point2, outgoing2)

      | (`Curl (u1, t1), `Dir (d2, t2)) ->
        let dirs =
          Direction_guessing.guess_directions ~start_curl:u1 ~end_dir:d2
            ~tensions:[|(t1,t2)|] [|point1; point2|]
        in
        let (ctrl1, ctrl2) =
          tensions_to_controls ~no_inflection:(t1 < 0. || t2 < 0.) point1 point2
            dirs.(0) dirs.(1) (abs_float t1) (abs_float t2)
        in
        let c = Vect.set contour start_bp (incoming1, point1, `Ctrl ctrl1) in
        Vect.set c end_bp (`Ctrl ctrl2, point2, outgoing2)

      | (`Ctrl _, _) | (_, `Ctrl _) | (`Open _, _) | (_, `Open _) ->
        failwith "fill_in_length1_segment_control_points"

  let fill_in_longer_segment_control_points ?tol contour start_bp end_bp =
    let contour = fix_breakpoints ?tol contour start_bp end_bp in
    let n = Vect.length contour in
    let modn = posmod n in
    let point_count =
      if start_bp < end_bp then
        end_bp - start_bp + 1
      else
        n - start_bp + end_bp + 1
    in
    let (incoming1, point1, outgoing1) = Vect.at contour start_bp in
    let (incoming2, point2, outgoing2) = Vect.at contour end_bp in
    let (dir1, curl1) =
      match outgoing1 with
        | `Curl (u,_) -> (None, Some u)
        | `Dir (d,_) -> (Some d, None)
        | _ -> failwith "fill_in_longer_segment_control_points"
    in
    let (dir2, curl2) =
      match incoming2 with
        | `Curl (u,_) -> (None, Some u)
        | `Dir (d,_) -> (Some d, None)
        | _ -> failwith "fill_in_longer_segment_control_points"
    in
    let points =
      Array.init
        point_count
        (fun k -> let (_,pt,_) = Vect.at contour (modn (start_bp + k)) in pt)
    in
    let tensions =
      Array.init
        (point_count - 1)
        (fun k ->
          let t1 =
            match Vect.at contour (modn (start_bp + k)) with
              | (_, _, `Open t1) | (_, _, `Dir (_,t1)) | (_, _, `Curl(_,t1)) -> t1
              | (_, _, `Ctrl _) -> failwith "fill_in_longer_segment_control_points"
          in
          let t2 =
            match Vect.at contour (modn (start_bp + k + 1)) with
              | (`Open t2, _, _) | (`Dir (_,t2), _, _) | (`Curl (_,t2), _, _) -> t2
              | (`Ctrl _, _, _) -> failwith "fill_in_longer_segment_control_points"
          in
          (t1,t2))
    in
    let dirs =
      Direction_guessing.guess_directions ?start_dir:dir1 ?end_dir:dir2
        ?start_curl:curl1 ?end_curl:curl2 ~tensions points
    in
    let contour =
      fold
        (fun c k ->
          let j1 = modn (start_bp + k) in
          let j2 = modn (start_bp + k + 1) in
          let (incoming1, point1, _) = Vect.at c j1 in
          let (_, point2, outgoing2) = Vect.at c j2 in
          let dir1 = dirs.(k) in
          let dir2 = dirs.(k + 1) in
          let (tension1, tension2) = tensions.(k) in
          let no_inflection = tension1 < 0. || tension2 < 0. in
          let (control1, control2) =
            tensions_to_controls ~no_inflection point1 point2 dir1 dir2
              (abs_float tension1) (abs_float tension2)
          in
          let c' = Vect.set c j1 (incoming1, point1, `Ctrl control1) in
          let c' = Vect.set c' j2 (`Ctrl control2, point2, outgoing2) in
          c')
        contour
        (0 --^ (Array.length dirs - 1))
    in
    contour

  let fill_in_segment_control_points ?tol contour start_bp end_bp =
    let n = Vect.length contour in
    if (start_bp + 1) mod n = end_bp then
      fill_in_length1_segment_control_points ?tol contour start_bp end_bp
    else
      fill_in_longer_segment_control_points ?tol contour start_bp end_bp

  let fill_in_all_segment_control_points ?tol contour first_bp last_bp =
    let rec fill c bp =
      let next_bp = find_next_breakpoint c bp last_bp in
      let c = fill_in_segment_control_points ?tol c bp next_bp in
      if next_bp = last_bp then
        c
      else
        fill c next_bp
    in
    fill contour first_bp

  let guess_dirs ?tol contour =
    let contour = join_coincident_knots ?tol contour in
    let n = Vect.length contour in
    if is_closed ?tol contour then
      let first_bp = find_first_breakpoint contour in
      if first_bp = n then
        (* There are no breakpoints. *)
        if n = 2 then
          contour
        else
          fill_in_cycle_control_points ?tol contour
      else
        (close ?tol
           (fill_in_all_segment_control_points ?tol (unclose ?tol contour) first_bp first_bp))
    else
      if n = 1 then
        contour
      else if n = 2 then
        fill_in_segment_control_points ?tol contour 0 (n - 1)
      else
        fill_in_all_segment_control_points ?tol contour 0 (n - 1)

  let incoming_dir ?tol contour =
    try
      knot_incoming_dir ?tol (Vect.at contour 0)
    with Failure "knot_incoming_dir" -> failwith "incoming_dir"

  let outgoing_dir ?tol contour =
    try
      knot_outgoing_dir ?tol (Vect.at contour (Vect.length contour - 1))
    with Failure "knot_outgoing_dir" -> failwith "outgoing_dir"

  let set_incoming_dir ?tol ?(guess = true) ?dir contour =
    let contour = if guess then guess_dirs ?tol contour else contour in
    Vect.modify contour 0 (fun knot -> set_knot_incoming_dir ?tol ?dir knot)

  let set_outgoing_dir ?tol ?(guess = true) ?dir contour =
    let contour = if guess then guess_dirs ?tol contour else contour in
    let k = Vect.length contour - 1 in
    Vect.modify contour k (fun knot -> set_knot_outgoing_dir ?tol ?dir knot)

  let set_dirs ?tol ?(guess = true) ?in_dir ?out_dir contour =
    let contour = if guess then guess_dirs ?tol contour else contour in
    set_outgoing_dir ?tol ?dir:out_dir (set_incoming_dir ?tol ?dir:in_dir contour)

  let rec fix_endpoints ?tol contour =
    let n = Vect.length contour in
    let (incoming1, point1, outgoing1) = Vect.at contour 0 in
    let (incoming2, point2, outgoing2) = Vect.at contour (n - 1) in
    match (incoming1, outgoing2)  with
      | (`Ctrl _, `Ctrl _) -> contour
      | _ ->
        let incoming =
          match incoming1 with
            | `Ctrl _ -> incoming1
            | `Dir (d,_) -> `Ctrl Complex_point.(point1 - d)
            | `Curl _ -> `Ctrl point1
            | `Open _ ->
              match outgoing1 with
                | `Ctrl ctrl when Cubic.points_coincide ?tol ctrl point1 -> `Ctrl point1
                | `Ctrl ctrl -> `Ctrl Complex_point.(point1 + dir (point1 - ctrl))
                | `Dir (d,_) -> `Ctrl Complex_point.(point1 - d)
                | `Curl _ | `Open _ -> `Ctrl point1
        in
        let outgoing =
          match outgoing2 with
            | `Ctrl _ -> outgoing2
            | `Dir (d,_) -> `Ctrl Complex.(point2 + d)
            | `Curl _ -> `Ctrl point2
            | `Open _ ->
              match incoming2 with
                | `Ctrl ctrl when Cubic.points_coincide ?tol ctrl point2 -> `Ctrl point2
                | `Ctrl ctrl -> `Ctrl Complex_point.(point2 + dir (point2 - ctrl))
                | `Dir _ | `Curl _ | `Open _ -> failwith "fix_endpoints"
        in
        let contour = Vect.set contour 0 (incoming, point1, outgoing1) in
        let contour = Vect.set contour (n - 1) (incoming2, point2, outgoing) in
        contour

  let to_cubic ?tol contour =
    let make_cycle = if is_closed ?tol contour then Cubic.close else Cubic.unclose in
    let contour = unclose ?tol (fix_endpoints ?tol (guess_dirs ?tol contour)) in
    let cubic =
      Vect.fold
        (fun cubic_contour knot ->
          match knot with
            | (`Ctrl ctrl1, point1, `Ctrl ctrl2) ->
              Cubic.join ?tol cubic_contour [(ctrl1, point1, ctrl2)]
            | _ -> failwith "to_cubic")
        (match Vect.at contour 0 with
          | (`Ctrl ctrl1, point1, `Ctrl ctrl2) -> [(ctrl1, point1, ctrl2)]
          | _ -> failwith "to_cubic")
        (Vect.sub 1 (Vect.length contour - 1) contour)
    in
    make_cycle cubic

  let of_cubic cubic =
    Vect.of_list (List.map (fun (ih, oc, oh) -> (`Ctrl ih, oc, `Ctrl oh)) cubic)

  let knot
      ?in_tension ?out_tension
      ?in_curl ?out_curl
      ?in_dir ?out_dir ?dir
      ?in_control ?out_control
      point =
    if (Option.is_some in_dir || Option.is_some out_dir) && Option.is_some dir ||
      1 < ((if Option.is_some in_curl then 1 else 0)
            + (if Option.is_some in_dir then 1 else 0)
            + (if Option.is_some in_control then 1 else 0)) ||
      1 < ((if Option.is_some out_curl then 1 else 0)
           + (if Option.is_some out_dir then 1 else 0)
           + (if Option.is_some out_control then 1 else 0)) ||
      (Option.is_some in_control && Option.is_some in_tension) ||
      (Option.is_some out_control && Option.is_some out_tension) then
      invalid_arg "knot";
    let t1 = if Option.is_some in_tension then Option.get in_tension else 1. in
    let t2 = if Option.is_some out_tension then Option.get out_tension else 1. in
    let in_dir = if Option.is_some dir then dir else in_dir in
    let out_dir = if Option.is_some dir then dir else out_dir in
    let incoming =
      if Option.is_some in_curl then
        `Curl (Option.get in_curl, t1)
      else if Option.is_some in_dir then
        `Dir (Complex_point.dir (Option.get in_dir), t1)
      else if Option.is_some in_control then
        `Ctrl (Option.get in_control)
      else
        `Open t1
    in
    let outgoing =
      if Option.is_some out_curl then
        `Curl (Option.get out_curl, t2)
      else if Option.is_some out_dir then
        `Dir (Complex_point.dir (Option.get out_dir), t2)
      else if Option.is_some out_control then
        `Ctrl (Option.get out_control)
      else
        `Open t2
    in
    (incoming, point, outgoing)

  let dir_knot dir ?in_tension ?out_tension point =
    knot ~dir ?in_tension ?out_tension point

  let left_knot = dir_knot Complex.(neg one)
  let right_knot = dir_knot Complex.one
  let up_knot = dir_knot Complex.i
  let down_knot = dir_knot Complex.(neg i)

  let point
      ?in_tension ?out_tension
      ?in_curl ?out_curl
      ?in_dir ?out_dir ?dir
      ?in_control ?out_control
      pt =
    Vect.make 1
      (knot
         ?in_tension ?out_tension
         ?in_curl ?out_curl
         ?in_dir ?out_dir ?dir
         ?in_control ?out_control
         pt)
  let along dir ?in_tension ?out_tension point =
    Vect.make 1 (knot ~dir ?in_tension ?out_tension point)
  let left ?in_tension ?out_tension point =
    Vect.make 1 (left_knot  ?in_tension ?out_tension point)
  let right ?in_tension ?out_tension point =
    Vect.make 1 (right_knot  ?in_tension ?out_tension point)
  let up ?in_tension ?out_tension point =
    Vect.make 1 (up_knot  ?in_tension ?out_tension point)
  let down ?in_tension ?out_tension point =
    Vect.make 1 (down_knot  ?in_tension ?out_tension point)

  let ( <@> ) contour1 = join contour1
  let ( <@~> ) contour1 = join ~tension:1. contour1
  let ( <@-> ) contour1 = join ~tension:(-1.) contour1

  let ( <@~~.> ) contour1 (in_tension, out_tension) contour2 =
    join ~in_tension ~out_tension contour1 contour2

  let ( <@--.> ) contour1 (in_tension, out_tension) contour2 =
    join ~in_tension:(-.in_tension) ~out_tension:(-.out_tension)
      contour1 contour2

  let ( <@~.> ) contour1 tension contour2 =
    join ~tension contour1 contour2

  let ( <@-.> ) contour1 tension contour2 =
    join ~tension:(-.tension) contour1 contour2

  let ( <.~~@> ) f a = f a
  let ( <.~@> ) f a = f a
  let ( <.--@> ) f a = f a
  let ( <.-@> ) f a = f a
(*
  let ( <@@ ) contour t_or_f =
    (if t_or_f then close else unclose) contour

  let ( <~~@@ ) contour (tension1, tension2) =
    close_with_tensions tension1 tension2 contour

  let ( <--@@ ) contour (tension1, tension2) =
    close_with_tensions ~no_inflection:true tension1 tension2 contour

  let ( <~@@ ) contour tension =
    close_with_tension tension contour

  let ( <-@@ ) contour tension =
    close_with_tension ~no_inflection:true tension contour
*)

(* FIXME: These depend on how "pointwise" gets repaired.
  let ( <.> ) contour f = pointwise f contour
  let ( <*> ) contour pt = pointwise (flip Complex.mul pt) contour
  let ( </> ) contour pt = pointwise (flip Complex.div pt) contour
  let ( <+> ) contour pt = pointwise (flip Complex.add pt) contour
  let ( <-> ) contour pt = pointwise (flip Complex.sub pt) contour
*)
end

(*-----------------------------------------------------------------------*)

let transform_option f opt =
  match opt with
    | None -> None
    | Some v -> Some (f v)

let transform_pair f1 f2 (a,b) = (f1 a, f2 b)

module Glyph =
struct
  type (+'float, +'contour) t = {
    name : string;
    unicode : int option;
    contours : 'contour list;
    lsb : 'float option;
    rsb : 'float option;
    hints : ('float * 'float) list; (* FIXME: really implement hints. *)
  }

  let empty = {
    name = "";
    unicode = None;
    contours = [];
    lsb = None;
    rsb = None;
    hints = [];
  }

  let transform float_to_float contour_to_contour glyph = {
    glyph with
      contours = List.map contour_to_contour glyph.contours;
      lsb = transform_option float_to_float glyph.lsb;
      rsb = transform_option float_to_float glyph.rsb;
      hints = List.map (transform_pair float_to_float float_to_float) glyph.hints;
  }
end

module Cubic_glyph =
struct
  type t = (float, Cubic.t) Glyph.t

  open Glyph

  let _print_python_glyph_code
      ~glyph_variable
      ~contour_variable
      outp glyph =
    Print.fprintf outp p"%s.foreground = fontforge.layer()\n" glyph_variable;
    List.iter
      (fun contour ->
        Cubic.print_python_contour_code ?variable:(Some contour_variable) outp contour;
        Print.fprintf outp p"%s.foreground += %s\n" glyph_variable contour_variable)
      glyph.contours;
    if Option.is_some glyph.lsb || Option.is_some glyph.rsb then
      begin
        if glyph.contours = [] then
          (* A space character. Treat rsb as advance width. *)
          if Option.is_some glyph.rsb then
            Print.fprintf outp p"%s.width = %F\n" glyph_variable (Option.get glyph.rsb)
          else
            ()
        else
          let (min_pt, max_pt) = Cubic.overall_bounds ~fast:false (List.enum glyph.contours) in

          let x_shift =
            if Option.is_some glyph.lsb then
              Option.get glyph.lsb -. min_pt.Complex_point.re
            else
              0.
          in
          if x_shift <> 0. then
            Print.fprintf outp p"%s.transform(psMat.translate(%F,0))\n" glyph_variable x_shift;

          if Option.is_some glyph.rsb then
            Print.fprintf outp p"%s.width = %F\n"
              glyph_variable
              (floor (x_shift +. Option.get glyph.rsb +. max_pt.Complex_point.re +. 0.5));
      end;
    Print.fprintf outp p"%s.canonicalContours()\n" glyph_variable;
    Print.fprintf outp p"%s.canonicalStart()\n" glyph_variable

  let print_python_glyph_code
      ?(font_variable = "my_font")
      ?(glyph_variable = "my_glyph")
      ?(contour_variable = "my_contour")
      outp glyph =
    begin
      match glyph.unicode with
        | None ->
          Print.fprintf outp p"%s = %s.createChar(preferred_unicode('%s'), '%s')\n"
            glyph_variable font_variable glyph.name glyph.name;
          Print.fprintf outp p"%s.unicode = preferred_unicode('%s')\n"
            glyph_variable glyph.name
        | Some unicode ->
          Print.fprintf outp p"%s = %s.createChar(%d, '%s')\n"
            glyph_variable font_variable unicode glyph.name;
          Print.fprintf outp p"%s.unicode = %d\n" glyph_variable unicode
    end;
    _print_python_glyph_code
      ~glyph_variable:glyph_variable
      ~contour_variable:contour_variable
      outp glyph

  let print_python_glyph_update_module outp glyph =
    output_string outp "import fontforge\n";
    output_string outp "import psMat\n";
    output_string outp "my_glyph = fontforge.activeGlyph()\n";
    _print_python_glyph_code
      ~glyph_variable:"my_glyph"
      ~contour_variable:"my_contour"
      outp glyph
end

(*-----------------------------------------------------------------------*)
