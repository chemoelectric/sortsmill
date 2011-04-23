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

  let dpolar norm arg = polar norm (rad arg)
  let rot theta = polar 1.0 (rad theta)
  let dir c = c / abs(c)

  let inner a b = (a.re *. b.re) +. (a.im *. b.im)

  let proj a b =
    let b' = dir b in
    of_float (inner a b') * b'

  let x_shear c angle = c + x'(c.re *. (dtan angle))

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

(* NOTE: It is not certain that we will keep this module. *)
module Parameterized_complex_point(Param : Parameter_type) =
struct
  module Cx = Extended_complex

  type bool' = Param.t -> Bool.t
  type int' = Param.t -> Int.t
  type float' = Param.t -> Float.t
  type string' = Param.t -> String.t

  type t = Param.t -> Cx.t

  let zero p = const Cx.zero p
  let one p = const Cx.one p
  let i p = const Cx.i p

  let neg v = Cx.neg -| v
  let conj v = Cx.conj -| v
  let sqrt v = Cx.sqrt -| v
  let norm2 v = Cx.norm2 -| v
  let norm v = Cx.norm -| v
  let arg v = Cx.arg -| v
  let exp v = Cx.exp -| v
  let log v = Cx.log -| v
  let inv v = Cx.inv -| v
  let succ v = Cx.succ -| v
  let pred v = Cx.pred -| v
  let abs v = Cx.abs -| v
  let dir v = Cx.dir -| v
  let round v = Cx.round -| v
  let rot angle = Cx.rot -| angle
  let x' r = Cx.x' -| r
  let y' r = Cx.y' -| r

  let of_int n = Cx.of_int -| n
  let to_int v = Cx.to_int -| v
  let of_float r = Cx.of_float -| r
  let to_float v = Cx.to_float -| v
  let of_string s = Cx.of_string -| s
  let to_string v = Cx.to_string -| v

  let add v w = fun p -> Cx.add (v p) (w p)
  let sub v w = fun p -> Cx.sub (v p) (w p)
  let mul v w = fun p -> Cx.mul (v p) (w p)
  let div v w = fun p -> Cx.div (v p) (w p)
  let modulo v w = fun p -> Cx.modulo (v p) (w p)
  let pow v w = fun p -> Cx.pow (v p) (w p)
  let proj v w = fun p -> Cx.proj (v p) (w p)
  let x_shear v r = fun p -> Cx.x_shear (v p) (r p)
  let min_bound v w = fun p -> Cx.min_bound (v p) (w p)
  let max_bound v w = fun p -> Cx.max_bound (v p) (w p)
  let inner v w = fun p -> Cx.inner (v p) (w p)
  let compare v w = fun p -> Cx.compare (v p) (w p)
  let dpolar r s = fun p -> Cx.dpolar (r p) (s p)
  let polar r s = fun p -> Cx.polar (r p) (s p)
  let ( + ) v w = fun p -> Cx.( + ) (v p) (w p)
  let ( - ) v w = fun p -> Cx.( - ) (v p) (w p)
  let ( * ) v w = fun p -> Cx.( * ) (v p) (w p)
  let ( / ) v w = fun p -> Cx.( / ) (v p) (w p)
  let ( ** ) v w = fun p -> Cx.( ** ) (v p) (w p)
  let ( <> ) v w = fun p -> Cx.( <> ) (v p) (w p)
  let ( >= ) v w = fun p -> Cx.( >= ) (v p) (w p)
  let ( <= ) v w = fun p -> Cx.( <= ) (v p) (w p)
  let ( > ) v w = fun p -> Cx.( > ) (v p) (w p)
  let ( < ) v w = fun p -> Cx.( < ) (v p) (w p)
  let ( = ) v w = fun p -> Cx.( = ) (v p) (w p)

  let print outp _v =
    output_string outp "fun: Param.t -> Fontdesign.Extended_complex.t"

  let t_printer paren outp v =
    if paren then IO.write outp '(';
    print outp v;
    if paren then IO.write outp ')'
end

(*-----------------------------------------------------------------------*)

module Cubic_base
  (L : module type of List)
  (P : Point_type) =
struct
  type point = P.t
  type t = (P.t * P.t * P.t) L.t

  let make_node rel_inhandle on_curve_point rel_outhandle =
    [P.(on_curve_point + rel_inhandle,
        on_curve_point,
        on_curve_point + rel_outhandle)]

  let make_pin on_curve_point =
    make_node P.zero on_curve_point P.zero

  let make_flat on_curve_point direction inhandle_length outhandle_length =
    let v = P.dir direction in
    let ilen = P.abs inhandle_length in
    let olen = P.abs outhandle_length in
    make_node P.(neg ilen * v) on_curve_point P.(olen * v)

  let make_right on_curve_point inhandle_length outhandle_length =
    make_flat on_curve_point P.one inhandle_length outhandle_length

  let make_left on_curve_point inhandle_length outhandle_length =
    make_flat on_curve_point P.(neg one) inhandle_length outhandle_length

  let make_up on_curve_point inhandle_length outhandle_length =
    make_flat on_curve_point P.i inhandle_length outhandle_length

  let make_down on_curve_point inhandle_length outhandle_length =
    make_flat on_curve_point (P.neg P.i) inhandle_length outhandle_length

  let is_empty contour = contour = []
  let is_singleton contour = contour <> [] && L.tl contour = []

  let rev contour = L.rev_map (fun (ih,oc,oh) -> (oh,oc,ih)) contour

  (* Nodewise and pointwise mapping. *)
  let nodewise = L.map
  let pointwise f = L.map (fun (ih,oc,oh) -> (f ih, f oc, f oh))

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
  let ( <*> ) contour pt = pointwise (P.mul pt) contour
  let ( </> ) contour pt = pointwise (P.div pt) contour
  let ( <+> ) contour pt = pointwise (P.add pt) contour
  let ( <-> ) contour pt = pointwise (P.sub pt) contour
end

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

  let _nodes_coincide ?(tol = !linear_tolerance) (_, p1, _) (_, p2, _) =
    P.(norm (p1 - p2)) <= tol

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

  let subdivide_curve ?(pos = 0) contour time =
    let c = L.drop pos contour in
    let (ih1, oc1, _) = L.hd c in
    let (_, oc2, oh2) = L.hd (L.tl c) in
    let bez = bezier_curve c in
    let (bez1, bez2) = Caml2geom.Cubic_bezier.subdivide bez time in
    let (_a0, a1, a2, a3) = bezier_curve_to_four_complexes bez1 in
    let (_b0, b1, b2, b3) = bezier_curve_to_four_complexes bez2 in
    let node1 = (ih1, oc1, a1) in
    let node2 = (a2, a3, b1) in
    let node3 = (b2, oc2, oh2) in
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
        let (_, oc1, oh1) = last1 in
        let (ih2, _, _) = first2 in
        let joined_node = (ih2, oc1, oh1) in
        L.rev_append (joined_node :: L.tl rev1) contour2
      else
        L.append contour1 contour2

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

  let ( <@@ ) contour t_or_f =
    (if t_or_f then close else unclose) contour

  let ( <@> ) contour1 = join contour1
end

(*-----------------------------------------------------------------------*)

module Parameterized_contour(Param : Parameter_type) =
struct

  (* NOTE: It is not certain that we will keep PComplex support. *)
  module PComplex = Parameterized_complex_point(Param)

  (* NOTE: It is not certain that we will keep PCubic support. *)
  module PCubic =
  struct
    include Cubic_base(List)(PComplex)
    module L = List

    let _nodes_coincide (_, p1, _) (_, p2, _) =
      p1 == p2

    let is_closed contour =
      _nodes_coincide (L.first contour) (L.last contour)

    let close contour =
      if is_closed contour then
        contour
      else
        contour @ [L.first contour]

    let unclose contour =
      if is_closed contour then
        L.drop (L.length contour - 1) contour
      else
        contour

    let join contour1 contour2 =
      if contour1 == contour2 then
        close contour1
      else
        let rev1 = L.rev contour1 in
        let last1 = L.first rev1 in
        let first2 = L.first contour2 in
        if _nodes_coincide last1 first2 then
          let (_, oc1, oh1) = last1 in
          let (ih2, _, _) = first2 in
          let joined_node = (ih2, oc1, oh1) in
          L.rev_append (joined_node :: L.tl rev1) contour2
        else
          L.append contour1 contour2

    let ( <@@ ) contour t_or_f =
      (if t_or_f then close else unclose) contour

    let ( <@> ) = join
  end

  type t =
    [
    | `Parameterized of Param.t -> t
    | `Cubic of Cubic.t
    | `PCubic of PCubic.t
    ]

  let resolve_pcubic contour param =
    PCubic.pointwise (fun pt -> pt param) contour

  let rec resolve contour param =
    (* Resolve to a de-parameterized cubic bezier contour. *)
    match contour with
      | `Parameterized c -> resolve (c param) param
      | `Cubic c -> c
      | `PCubic c -> resolve_pcubic c param

  let of_parameterized c = `Parameterized c
  let of_cubic c = `Cubic c
  let of_param_to_cubic c = `Parameterized (fun p -> `Cubic (c p))
  let of_pcubic c = `PCubic c

  let bounds2 ?fast contour =
    fun p -> Cubic.bounds ?fast (resolve contour p)

  let bounds ?fast contour =
    let b = bounds2 ?fast contour in
    ((fun p -> fst (b p)), (fun p -> snd (b p)))

  let overall_bounds2 ?fast enum_of_contours =
    fun p -> Cubic.overall_bounds ?fast (map (flip resolve p) enum_of_contours)

  let overall_bounds ?fast enum_of_contours =
    let ob = overall_bounds2 ?fast enum_of_contours in
    ((fun p -> fst (ob p)), (fun p -> snd (ob p)))
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
