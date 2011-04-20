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

let deg theta = (180. /. Float.pi) *. theta
let rad theta = (Float.pi /. 180.) *. theta

let dsin = sin -| rad
let dcos = cos -| rad
let dtan = tan -| rad

let adsin = deg -| asin
let adcos = deg -| acos
let adtan = deg -| atan
let adtan2 x y = deg (atan2 x y)

module Extended_complex =
(* Extensions for the Complex module. *)
struct
  (* The following type aliases are needed to make Extended_complex a
     valid Point_type. *)
  type bool = Bool.t
  type int = Int.t
  type float = Float.t
  type string = String.t

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

module type Point_type =
sig
  type bool
  type int
  type float
  type string
  type t
  val zero : t
  val one : t
  val i : t
  val neg : t -> t
  val conj : t -> t
  val sqrt : t -> t
  val norm2 : t -> float
  val norm : t -> float
  val arg : t -> float
  val exp : t -> t
  val log : t -> t
  val inv : t -> t
  val succ : t -> t
  val pred : t -> t
  val abs : t -> t
  val dir : t -> t
  val round : t -> t
  val rot : float -> t
  val x' : float -> t
  val y' : float -> t
  val of_int : int -> t
  val to_int : t -> int
  val of_float : float -> t
  val to_float : t -> float
  val of_string : string -> t
  val to_string : t -> string
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val modulo : t -> t -> t
  val pow : t -> t -> t
  val proj : t -> t -> t
  val min_bound : t -> t -> t
  val max_bound : t -> t -> t
  val inner : t -> t -> float
  val compare : t -> t -> int
  val dpolar : float -> float -> t
  val polar : float -> float -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( ** ) : t -> t -> t
  val ( <> ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( = ) : t -> t -> bool
  val print : unit BatIO.output -> t -> unit
  val t_printer : t Value_printer.t
end

module type Parameter_type =
sig
  type t
end

module Parameterized_complex(Param : Parameter_type) =
struct
  module Cx = Extended_complex

  type bool = Param.t -> Bool.t
  type int = Param.t -> Int.t
  type float = Param.t -> Float.t
  type string = Param.t -> String.t

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

module Complex_point =
struct
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

module type Node_type =
sig
  type element
  type t
  val apply : (element -> element) -> (t -> t)
  val print : unit IO.output -> t -> unit
  val t_printer : t Value_printer.t
end

module Cubic_node(P : Point_type) =
struct
  type point = P.t
  type element = point
  type t = { ih : point; oc : point; oh : point }

  let apply f p = { ih = f p.ih; oc = f p.oc; oh = f p.oh }

  let to_list p = [p.ih; p.oc; p.oh]
  let of_list lst = { ih = List.hd lst;
                      oc = List.hd (List.tl lst);
                      oh = List.hd (List.tl (List.tl lst)) }
  let to_list2 p = [(p.ih, false); (p.oc, true); (p.oh, false)]

  let make_node rel_inhandle on_curve_point rel_outhandle =
    P.({ ih = on_curve_point + rel_inhandle;
         oc = on_curve_point;
         oh = on_curve_point + rel_outhandle })

  let make_pin on_curve_point = make_node P.zero on_curve_point P.zero

  let make_flat on_curve_point direction inhandle_length outhandle_length =
    let v = P.(direction / P.abs direction) in
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

  let on_curve p = p.oc
  let inhandle p = p.ih
  let outhandle p = p.oh
  let rel_inhandle p = P.(p.ih - p.oc)
  let rel_outhandle p = P.(p.oh - p.oc)

  let print outp p =
    output_string outp "make_node (";
    P.print outp P.(p.ih - p.oc);
    output_string outp ") (";
    P.print outp p.oc;
    output_string outp ") (";
    P.print outp P.(p.oh - p.oc);
    output_string outp ")"

  let t_printer paren outp p =
    if paren then IO.write outp '(';
    print outp p;
    if paren then IO.write outp ')'
end

let bezier_curve_to_four_points bez =
  let points = Caml2geom.Bezier_curve.to_complex_array bez in
  let len = Array.length points in
  assert (len = 2 || len = 4); (* Only linears and cubics are allowed. *)
  if len = 2 then
    (points.(0), points.(0), points.(1), points.(1))
  else
    (points.(0), points.(1), points.(2), points.(3))

module Cubic_node_of_complex_point =
struct
  include Cubic_node(Complex_point)

  let bezier_curve node1 node2 =
    let p0 = Complex_point.to_bezier_point (on_curve node1) in
    let p1 = Complex_point.to_bezier_point (outhandle node1) in
    let p2 = Complex_point.to_bezier_point (inhandle node2) in
    let p3 = Complex_point.to_bezier_point (on_curve node2) in
    Caml2geom.Cubic_bezier.of_four_points p0 p1 p2 p3

  let of_bezier_curves bez1 bez2 =
    let (_, _, ih, oc) = bezier_curve_to_four_points bez1 in
    let (_, oh, _, _) = bezier_curve_to_four_points bez2 in
    { ih; oc; oh }

  let bounds ?(fast = false) node1 node2 =
    let bounds_func =
      if fast then
        Caml2geom.Cubic_bezier.bounds_fast
      else
        Caml2geom.Cubic_bezier.bounds_exact
    in
    let bez = bezier_curve node1 node2 in
    let rect = bounds_func bez in
    Complex_point.(of_bezier_point (Caml2geom.Rect.min rect),
                   of_bezier_point (Caml2geom.Rect.max rect))

  let subdivide node1 node2 time =
    let bez = bezier_curve node1 node2 in
    let (bez1, bez2) = Caml2geom.Cubic_bezier.subdivide bez time in
    let (_a0, a1, a2, a3) = bezier_curve_to_four_points bez1 in
    let (_b0, b1, b2, b3) = bezier_curve_to_four_points bez1 in
    let n1 = { ih = node1.ih; oc = node1.oc; oh = a1 } in
    let n2 = { ih = a2; oc = a3; oh = b1 } in
    let n3 = { ih = b2; oc = node2.oc; oh = node2.oh } in
    (n1, n2, n3)
end

module type Node_spline_type =
sig
  type +'node t

  include Enum.Enumerable with type 'node enumerable = 'node t
  val backwards : 'node t -> 'node Enum.t
  val of_backwards : 'node Enum.t -> 'node t

  include Interfaces.Mappable with type 'node mappable = 'node t
  val iter : ('node -> unit) -> 'node t -> unit

  val to_list : 'node t -> 'node list
  val of_list : 'node list -> 'node t

  val rev : 'node t -> 'node t
  val first : 'node t -> 'node
  val last : 'node t -> 'node
  val at : 'node t -> int -> 'node
  val append : 'node t -> 'node t -> 'node t
  val concat : 'node t list -> 'node t
  val flatten : 'node t list -> 'node t
  val split_at : int -> 'node t -> 'node t * 'node t
  val take : int -> 'node t -> 'node t
  val drop : int -> 'node t -> 'node t

  val print : ?first:string -> ?last:string -> ?sep:string ->
    ('a IO.output -> 'node -> unit) -> 'a IO.output -> 'node t -> unit
  val t_printer : 'node Value_printer.t -> 'node t Value_printer.t
end

module Node_spline : Node_spline_type =
struct
  type +'node t = 'node list
  type +'node mappable = 'node t
  type +'node enumerable = 'node t

  let map = List.map
  let iter = List.iter

  let of_enum = List.of_enum
  let enum = List.enum
  let of_backwards = List.of_backwards
  let backwards = List.backwards

  let to_list = identity
  let of_list = identity

  let rev = List.rev
  let first = List.first
  let last = List.last
  let at = List.at
  let append = List.append
  let concat = List.concat
  let flatten = List.flatten
  let split_at = List.split_at
  let take = List.take
  let drop = List.drop

  let print = List.print
  let t_printer = List.t_printer
end

let node_spline_to_cubic_beziers spline is_closed =
  let rec make_curves node_list =
    match node_list with
      | [] | [_] -> []
      | node1 :: node2 :: remaining ->
        Cubic_node_of_complex_point.bezier_curve node1 node2 ::
          make_curves (node2 :: remaining)
  in
  let node_list = Node_spline.to_list spline in
  if is_closed then
    make_curves (node_list @ [List.hd node_list])
  else
    make_curves node_list

module Node_contour(Node : Node_type) =
struct
  module Spline = Node_spline
  type t = 'node Spline.t * bool
  constraint 'node = Node.t

  let spline = fst
  let closed = snd

  let with_spline spline (_, is_closed) = (spline, is_closed)
  let with_closed is_closed (spline, _) = (spline, is_closed)

  let of_node_list node_list = (Spline.of_list node_list, false)
  let to_node_list (spline, _) = Spline.to_list spline

  let apply_spline_op contour spline_op =
    with_spline (spline_op (spline contour)) contour

  let apply_node_op contour node_op =
    apply_spline_op contour (Spline.map node_op)

  let rev contour = apply_spline_op contour Spline.rev

  let print_closed outp (_, is_closed) =
    if is_closed then
      output_string outp " <@@ true"
    else
      output_string outp " <@@ false"

  let print
      ?(first = "of_node_list [\n  ")
      ?(last = ";\n]")
      ?(sep = ";\n  ")
      outp contour =
    Spline.print ~first ~last ~sep Node.print outp (fst contour);
    print_closed outp contour

  let t_printer paren outp contour =
    if paren then IO.write outp '(';
    print outp contour;
    if paren then IO.write outp ')'

  let ( <@@ ) contour is_closed = with_closed is_closed contour
  let ( <@> ) (spline1, is_closed) (spline2, _) =
    (Node_spline.append spline1 spline2, is_closed)
end

module Cubic_contour(Point : Point_type) =
struct
  module Node = Cubic_node(Point)
  include Node_contour(Node)

  let to_point_bool_list contour =
    List.flatten (List.map Node.to_list2 (to_node_list contour))

  let ( <.> ) contour point_op = apply_node_op contour (Node.apply point_op)
  let ( <*> ) contour pt = apply_node_op contour (Node.apply (Point.mul pt))
  let ( </> ) contour pt = apply_node_op contour (Node.apply (Point.div pt))
  let ( <+> ) contour pt = apply_node_op contour (Node.apply (Point.add pt))
  let ( <-> ) contour pt = apply_node_op contour (Node.apply (Point.sub pt))
end

module Cubic =
struct
  include Cubic_contour(Complex_point)

  let to_cubic_beziers contour =
    node_spline_to_cubic_beziers (spline contour) (closed contour)

  let to_path contour =
    let curve_list = to_cubic_beziers contour in
    let first_node = Spline.first (spline contour) in
    let first_point = Node.on_curve first_node in
    let path = Caml2geom.Path.make (Complex_point.to_bezier_point first_point) in
    List.iter
      (Caml2geom.Path.append_curve ~stitch:Caml2geom.Path.NO_STITCHING path -|
          Caml2geom.Cubic_bezier.to_curve)
      curve_list;
    path

  let of_path
      ?(closed = false)
      ?(rel_inhandle = Complex_point.zero)
      ?(rel_outhandle = Complex_point.zero)
      ~tolerance path =
    let bez_curves = Caml2geom.Path.to_cubic_beziers_open path tolerance in
    let curves = List.map bezier_curve_to_four_points bez_curves in
    let first = List.first curves in
    let last = List.last curves in
    let curves' =
      if closed then
        last :: curves
      else
        let (a0, _, _, _) = first in
        let (_, _, _, b3) = last in
        let ih = Complex_point.(a0 + rel_inhandle) in
        let oh = Complex_point.(b3 + rel_outhandle) in
        let pre_first = Complex_point.(zero, zero, ih, a0) in
        let post_last = Complex_point.(b3, oh, zero, zero) in
        pre_first :: (curves @ [post_last])
    in
    let rec make_nodes =
      function
        | [] | [_] -> []
        | curve1 :: curve2 :: remaining ->
          let (_, _, ih, oc) = curve1 in
          let (_, oh, _, _) = curve2 in
          Node.({ ih; oc; oh }) ::
            make_nodes (curve2 :: remaining)
    in
    let spline' = Node_spline.of_list (make_nodes curves') in
    (spline', closed)

  let bounds ?(fast = false) contour =
    let most_positive = Complex_point.({ re = infinity; im = infinity }) in
    let most_negative = Complex_point.({ re = neg_infinity; im = neg_infinity }) in
    let rec get_bounds node_list =
      match node_list with
        | [] | [_] -> []
        | node1 :: node2 :: remaining ->
          Cubic_node_of_complex_point.bounds ~fast node1 node2 ::
            get_bounds (node2 :: remaining)
    in
    let node_list = Node_spline.to_list (spline contour) in
    let bounds_list =
      if closed contour then
        get_bounds (node_list @ [List.hd node_list])
      else
        get_bounds node_list
    in
    List.fold_left
      (fun (min1, max1) (min2, max2) -> Complex_point.(min_bound min1 min2,
                                                       max_bound max1 max2))
      (most_positive, most_negative)
      bounds_list

  let overall_bounds ?(fast = false) contour_enum =
    if Enum.is_empty contour_enum then
      raise Not_found
    else
      let most_positive = Complex_point.({ re = infinity; im = infinity }) in
      let most_negative = Complex_point.({ re = neg_infinity; im = neg_infinity }) in
      fold
        (fun (current_min, current_max) contour ->
          let (this_min, this_max) = bounds ~fast contour in
          Complex_point.(min_bound current_min this_min,
                         max_bound current_max this_max))
        (most_positive, most_negative)
        contour_enum

  let print_python_contour_code ?variable outp contour =
    let point_list = to_point_bool_list contour in
    let point_list = (List.tl point_list) @ [List.hd point_list] in
    match variable with
      | None ->
        (* This branch doesn't set the closedness. *)
        output_string outp "(fontforge.contour()";
        List.iter
          Complex_point.(fun (pt, on_curve) ->
            let oc_string = if on_curve then "True" else "False" in
            Print.fprintf outp p"+fontforge.point(%f,%f,%s)" pt.re pt.im oc_string
          )
          point_list;
        output_string outp ")"

      | Some var_name ->
        Print.fprintf outp p"%s = fontforge.contour()\n" var_name;
        List.iter
          Complex_point.(fun (pt, on_curve) ->
            if on_curve then
              Print.fprintf outp p"%s += fontforge.point(%f,%f)\n" var_name pt.re pt.im
            else
              Print.fprintf outp p"%s += fontforge.point(%f,%f, False)\n" var_name pt.re pt.im
          )
          point_list;
        Print.fprintf outp p"%s.closed = %s\n" var_name
          (if closed contour then "True" else "False")
end

module Parameterized_contour(Param : Parameter_type) =
struct
  module PComplex = Parameterized_complex(Param)
  module PCubic = Cubic_contour(PComplex)

  type t =
    [
    | `Parameterized of Param.t -> t
    | `Cubic of Cubic.t
    | `PCubic of PCubic.t
    ]

  let of_parameterized c = `Parameterized c
  let of_cubic c = `Cubic c
  let of_pcubic c = `PCubic c

  let resolve_pcontour_node pnode param =
    Cubic.Node.make_node
      ((PCubic.Node.rel_inhandle pnode) param)
      ((PCubic.Node.on_curve pnode) param)
      ((PCubic.Node.rel_outhandle pnode) param)

  let resolve_pcontour_spline spline param =
    PCubic.Spline.map (flip resolve_pcontour_node param) spline

  let rec resolve contour param =
    (* Resolve to a de-parameterized cubic bezier contour. *)
    match contour with
      | `Parameterized c -> resolve (c param) param
      | `Cubic c -> c
      | `PCubic c ->
        PCubic.with_spline (resolve_pcontour_spline (PCubic.spline c) param) c

  let bounds2 ?(fast = false) contour =
    fun p -> Cubic.bounds ~fast (resolve contour p)

  let bounds ?(fast = false) contour =
    let b = bounds2 ~fast contour in
    ((fun p -> fst (b p)), (fun p -> snd (b p)))

  let overall_bounds2 ?(fast = false) contour_enum =
    fun p -> Cubic.overall_bounds ~fast (map (flip resolve p) contour_enum)

  let overall_bounds ?(fast = false) contour_enum =
    let ob = overall_bounds2 ~fast contour_enum in
    ((fun p -> fst (ob p)), (fun p -> snd (ob p)))
end

let transform_option trans opt =
  match opt with
    | None -> None
    | Some v -> Some (trans v)

let transform_pair trans1 trans2 (a,b) = (trans1 a, trans2 b)

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

  let transform float_transformation contour_transformation glyph = {
    glyph with
      contours = List.map contour_transformation glyph.contours;
      lsb = transform_option float_transformation glyph.lsb;
      rsb = transform_option float_transformation glyph.rsb;
      hints = List.map (transform_pair float_transformation float_transformation) glyph.hints;
  }
end

module Cubic_glyph =
struct
  type t = {
    name : string;
    unicode : int option;
    contours : Cubic.t list;
    lsb : float option;
    rsb : float option;
    hints : (float * float) list; (* FIXME: really implement hints. *)
  }

  let to_glyph cg =
    Glyph.({
      name = cg.name;
      unicode = cg.unicode;
      contours = cg.contours;
      lsb = cg.lsb;
      rsb = cg.rsb;
      hints = cg.hints;
    })

  let of_glyph g = {
    name = g.Glyph.name;
    unicode = g.Glyph.unicode;
    contours = g.Glyph.contours;
    lsb = g.Glyph.lsb;
    rsb = g.Glyph.rsb;
    hints = g.Glyph.hints;
  }

  let empty = of_glyph Glyph.empty

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
