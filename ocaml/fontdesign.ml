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
end

module Complex_point =
struct
  include Extended_complex

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
end

module type Node_type =
sig
  type element
  type t
  val apply : (element -> element) -> (t -> t)
  val print : unit IO.output -> t -> unit
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
end

module type Contour_type =
sig
  type t
  val with_closed : bool -> t -> t
  val closed : t -> bool
  val print_closed : unit IO.output -> t -> unit
  val print : ?first:string -> ?last:string -> ?sep:string ->
    unit IO.output -> t -> unit
  val ( <@@ ) : t -> bool -> t
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

module Parameterized_cubics(Param : Parameter_type) =
struct
  module PComplex = Parameterized_complex(Param)
  module Contour = Cubic_contour(Complex_point)
  module PContour = Cubic_contour(PComplex)

  let parameterize_node node =
    PContour.Node.make_node
      (const (Contour.Node.rel_inhandle node))
      (const (Contour.Node.on_curve node))
      (const (Contour.Node.rel_outhandle node))

  let resolve_node pnode param =
    Contour.Node.make_node
      ((PContour.Node.rel_inhandle pnode) param)
      ((PContour.Node.on_curve pnode) param)
      ((PContour.Node.rel_outhandle pnode) param)

  let parameterize_spline = Contour.Spline.map parameterize_node
  let resolve_spline spline param = PContour.Spline.map (flip resolve_node param) spline

  let parameterize_contour contour =
    Contour.with_spline (parameterize_spline (Contour.spline contour)) contour

  let resolve_contour contour param =
    PContour.with_spline (resolve_spline (PContour.spline contour) param) contour

  let print_python_contour_code ?variable outp contour =
    let point_list = Contour.to_point_bool_list contour in
    let point_list = (List.tl point_list) @ [List.hd point_list] in
    match variable with
      | None ->
        (* This branch doesn't set the closedness. *)
        output_string outp "(fontforge.contour()";
        List.iter
          Complex.(fun (pt, on_curve) ->
            let oc_string = if on_curve then "True" else "False" in
            Print.fprintf outp p"+fontforge.point(%f,%f,%s)" pt.re pt.im oc_string
          )
          point_list;
        output_string outp ")"

      | Some var_name ->
        Print.fprintf outp p"%s = fontforge.contour()\n" var_name;
        List.iter
          Complex.(fun (pt, on_curve) ->
            if on_curve then
              Print.fprintf outp p"%s += fontforge.point(%f,%f)\n" var_name pt.re pt.im
            else
              Print.fprintf outp p"%s += fontforge.point(%f,%f, False)\n" var_name pt.re pt.im
          )
          point_list;
        Print.fprintf outp p"%s.closed = %s\n" var_name
          (if Contour.closed contour then "True" else "False")
end

module Contour_list =
struct
  include List

(* Here may go functions for intersection-related operations such as
   "overlay" and "punch". *)
end

module Glyph =
struct
  type ('float, 'contour) t = {
    name : string;
    unicode : int option;
    contours : 'contour Contour_list.t;
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

  let name glyph = glyph.name
  let with_name name glyph = { glyph with name }

  let has_unicode glyph = Option.is_some glyph.unicode
  let unicode glyph = Option.get glyph.unicode
  let with_unicode unicode glyph = { glyph with unicode = Some unicode }
  let without_unicode glyph = { glyph with unicode = None }

  let contours glyph = glyph.contours
  let with_contours contours glyph = { glyph with contours }

  let has_lsb glyph = Option.is_some glyph.lsb
  let lsb glyph = Option.get glyph.lsb
  let with_lsb lsb glyph = { glyph with lsb = Some lsb }
  let without_lsb glyph = { glyph with lsb = None }

  let has_rsb glyph = Option.is_some glyph.rsb
  let rsb glyph = Option.get glyph.rsb
  let with_rsb rsb glyph = { glyph with rsb = Some rsb }
  let without_rsb glyph = { glyph with rsb = None }

  let _print_python_glyph_code
      ~glyph_variable
      ~contour_variable
      print_float
      print_python_contour_code
      outp glyph =
    Print.fprintf outp p"%s.foreground = fontforge.layer()\n" glyph_variable;
    Contour_list.iter
      (fun contour ->
        print_python_contour_code ?variable:(Some contour_variable) outp contour;
        Print.fprintf outp p"%s.foreground += %s\n" glyph_variable contour_variable)
      glyph.contours;
    if Option.is_some glyph.lsb then
      begin
        Print.fprintf outp p"%s.transform(psMat.translate(" glyph_variable;
        print_float outp (Option.get glyph.lsb);
        output_string outp ",0))\n";
      end;
    if Option.is_some glyph.rsb then
      begin
        Print.fprintf outp p"%s.right_side_bearing = " glyph_variable; (* FIXME: Setting the width,
                                                                          given max x, might give
                                                                          more control. *)
        print_float outp (Option.get glyph.rsb);
        output_string outp "\n";
      end;
    Print.fprintf outp p"%s.canonicalContours()\n" glyph_variable;
    Print.fprintf outp p"%s.canonicalStart()\n" glyph_variable

  let print_python_glyph_code
      ?(font_variable = "my_font")
      ?(glyph_variable = "my_glyph")
      ?(contour_variable = "my_contour")
      print_float
      print_python_contour_code
      outp glyph =
    begin
      match glyph.unicode with
        | None ->
          Print.fprintf outp p"%s = %s.createChar(preferred_unicode(\"%s\"), \"%s\")\n"
            glyph_variable font_variable glyph.name glyph.name;
          Print.fprintf outp p"%s.unicode = preferred_unicode(\"%s\")\n"
            glyph_variable glyph.name
        | Some unicode ->
          Print.fprintf outp p"%s = %s.createChar(%d, \"%s\")\n"
            glyph_variable font_variable unicode glyph.name;
          Print.fprintf outp p"%s.unicode = %d\n" glyph_variable unicode
    end;
    _print_python_glyph_code
      ~glyph_variable:glyph_variable
      ~contour_variable:contour_variable
      print_float print_python_contour_code outp glyph

  let print_python_glyph_update_module print_float print_python_contour_code outp glyph =
    output_string outp "import fontforge\n";
    output_string outp "import psMat\n";
    output_string outp "my_glyph = fontforge.activeGlyph()\n";
    _print_python_glyph_code
      ~glyph_variable:"my_glyph"
      ~contour_variable:"my_contour"
      print_float print_python_contour_code outp glyph
end
