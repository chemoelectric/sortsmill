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
    let modn k =
      let m = k mod n in
      if m < 0 then
        m + n
      else
        m
    in

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

(*
    Print.fprintf stderr p"-----------------------------------------------------------------------\n";
    Print.fprintf stderr p"%{float array array}\n" left_side;
    Print.fprintf stderr p"%{float array}\n" right_side;
    Print.fprintf stderr p"solution: %{float array}\n" (Array.map deg solution);
    Array.print (Pair.print Float.print Float.print) stderr tensions;
    Print.fprintf stderr p"\nalpha: %{float array}\n" alpha;
    Print.fprintf stderr p"beta: %{float array}\n" beta;
    Print.fprintf stderr p"turn: %{float array}\n" (Array.map deg turn);
    Print.fprintf stderr p"vector: %{Complex_point.t array}\n" vector;
    Print.fprintf stderr p"dir: %{Complex_point.t array}\n" (let angle_to_dir k =
        Complex_point.(dir vector.(k) * polar 1. solution.(k))  in Array.of_enum (map angle_to_dir (0 --^ n)));
    Print.fprintf stderr p"-----------------------------------------------------------------------\n";
*)

    let angle_to_dir k =
        Complex_point.(dir vector.(k) * polar 1. solution.(k))
    in
    Array.of_enum (map angle_to_dir (0 --^ n))
end

(*-----------------------------------------------------------------------*)

(* FIXME: Remove this Point_string stuff *)
module Point_string =
struct
  type elt =
    [
    | `Point of Complex.t
    | `Dir of Complex.t
    | `Curl of float
    | `Tension of float
    ]

  type t = elt list

  type parse_result = {
    points : Complex.t list;
    tensions : (float * float) list;
    in_dir : Complex.t option;
    out_dir : Complex.t option;
    in_curl : float option;
    out_curl : float option;
  }

  let rec parse_points s =
    match s with
      | [`Point p] -> {
        points = [p];
        tensions = [];
        in_dir = None;
        out_dir = None;
        in_curl = None;
        out_curl = None;
      }

      | [`Point p; `Dir d] -> {
        points = [p];
        tensions = [];
        in_dir = None;
        out_dir = Some d;
        in_curl = None;
        out_curl = None;
      }

      | [`Point p; `Curl u] ->
        {
          points = [p];
          tensions = [];
          in_dir = None;
          out_dir = None;
          in_curl = None;
          out_curl = Some u;
        }

      | `Point p1 :: `Tension t1 :: `Tension t2 :: (`Point p2 :: _ as remaining) ->
        let subresult = parse_points remaining in {
          subresult with
            points = p1 :: subresult.points;
            tensions = (t1,t2) :: subresult.tensions
        }

      | `Point p1 :: `Tension t :: (`Point p2 :: _ as remaining) ->
        let subresult = parse_points remaining in {
          subresult with
            points = p1 :: subresult.points;
            tensions = (t,t) :: subresult.tensions
        }

      | `Point p1 :: (`Point p2 :: _ as remaining) ->
        let subresult = parse_points remaining in {
          subresult with
            points = p1 :: subresult.points;
            tensions = (1.,1.) :: subresult.tensions
        }

      | _ -> failwith "parse_points"

  let parse s =
    match s with
      | `Dir d :: points -> { parse_points points with in_dir = Some d }
      | `Curl u :: points -> { parse_points points with in_curl = Some u }
      | points -> parse_points points
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
    _nodes_coincide ?tol (L.first contour) (L.last contour)

  let close ?tol contour =
    if is_closed ?tol contour then
      contour
    else
      contour @ [L.first contour]

  let unclose ?tol contour =
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

  let resolve_point_string ps =
    Point_string.(
      let p = parse ps in
      let tensions = Array.of_list p.tensions in
      let points = Array.of_list p.points in
      let dirs = (Direction_guessing.guess_directions
                    ?start_dir:p.in_dir ?end_dir:p.out_dir
                    ?start_curl:p.in_curl ?end_curl:p.out_curl
                    ~tensions points) in
      fold
        (fun contour k ->
          let (tension1, tension2) = tensions.(k) in
          let node = make_dir_node dirs.(k + 1) points.(k + 1) in
          join_with_tensions tension1 tension2 contour node
        )
        (make_dir_node dirs.(0) points.(0))
        (0 --^ (Array.length tensions))
    )

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

  let _knots_coincide ?tol (_,point1,_) (_,point2,_) =
    Cubic.points_coincide ?tol point1 point2

  let is_closed ?tol contour =
    _knots_coincide ?tol (Vect.at contour 0) (Vect.at contour (Vect.length contour - 1))

  let close ?tol contour =
    if is_closed ?tol contour then
      contour
    else
      Vect.append (Vect.at contour 0) contour

  let unclose ?tol contour =
    if is_closed ?tol contour then
      Vect.sub 0 (Vect.length contour - 1) contour
    else
      contour

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
          | (`Open _, _, `Open _) -> find_it (k + 1)
          | _ -> k
    in
    find_it 0

  let find_next_breakpoint contour k =
    let n = Vect.length contour in
    let rec find_it j =
      match Vect.at contour j with
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
          let no_inflection = tension1 < 0. in
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

  let guess_directions ?tol contour =
    let n = Vect.length contour in
    let contour' = join_coincident_knots ?tol contour in
    let first_bp = find_first_breakpoint contour' in
    if first_bp = n then
      (* There are no breakpoints. *)
      if is_closed ?tol contour' then
        fill_in_cycle_control_points ?tol contour'
      else
        failwith "Not yet implemented 1"
    else
      failwith "Not yet implemented 2"

  let to_cubic ?tol contour =
    let contour' = guess_directions ?tol contour in
    let node_vect =
      Vect.map
        (fun knot ->
          match knot with
            | (`Ctrl ctrl1, point1, `Ctrl ctrl2) -> (ctrl1, point1, ctrl2)
            | _ -> failwith "to_cubic"
        )
        contour'
    in
    Vect.to_list node_vect
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
