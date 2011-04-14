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

module type Node_type =
sig
  type element
  type t
  include Interfaces.Mappable with type 'a mappable = t
  val map : (element -> element) -> (t -> t)
  val print : unit IO.output -> t -> unit
end

module type Contour_type =
sig
  type 'node t
  include Interfaces.Mappable with type 'node mappable = 'node t
  include Enum.Enumerable with type 'node enumerable = 'node t
  val backwards : 'node t -> 'node Enum.t
  val of_backwards : 'node Enum.t -> 'node t
  val of_node_list : 'node list -> 'node t    
  val append : 'node t -> 'node t -> 'node t
  val closed : 'node t -> bool -> 'node t
  val is_closed : 'node t -> bool
  val print : (unit IO.output -> 'node -> unit) -> unit IO.output -> 'node t -> unit
  val ( <@> ) : 'node t -> 'node t -> 'node t
  val ( <@@ ) : 'node t -> bool -> 'node t
  val ( <.> ) : 'node1 t -> ('node1 -> 'node2) -> 'node2 t
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

module Cubic_node(P : Point_type) =
struct
  type point = P.t
  type _node_points = { ih : point; oc : point; oh : point }
  type element = point
  type t = _node_points
  type 'a mappable = t

  let map f p = { ih = f p.ih; oc = f p.oc; oh = f p.oh }

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

module Parameterized_cubic_node(Param : Parameter_type) =
struct
  type t
end

module Contour =
struct
  type 'node t = {
    spline : 'node list;
    closed : bool;
  }
  type 'node mappable = 'node t
  type 'node enumerable = 'node t

  let map f contour =
    { contour with spline = List.map f contour.spline }

  let enum contour = List.enum contour.spline
  let of_enum e = { spline = List.of_enum e; closed = false }
  let backwards contour = List.backwards contour.spline
  let of_backwards e = { spline = List.of_backwards e; closed = false }

  let of_node_list nlist = { spline = nlist; closed = false }
  let to_node_list contour = contour.spline

  let append contour1 contour2 =
    { contour1 with spline = contour1.spline @ contour2.spline }

  let closed contour true_or_false =
    { contour with closed = true_or_false }

  let is_closed contour = contour.closed

  let rec print_spline print_node outp spline =
    match spline with
      | [] -> assert false
      | [p] ->
        output_string outp "  ";
        print_node outp p;
        output_string outp ";\n"
      | p :: remaining ->
        output_string outp "  ";
        print_node outp p;
        output_string outp ";\n";
        print_spline print_node outp remaining

  let print_closed outp is_closed =
    if is_closed then
      output_string outp " <@@ true"
    else
      output_string outp " <@@ false"

  let print print_node outp contour =
    output_string outp "of_node_list [\n";
    print_spline print_node outp contour.spline;
    output_string outp "]";
    print_closed outp contour.closed

  let ( <@> ) = append
  let ( <@@ ) = closed
  let ( <.> ) contour trans = map trans contour
end

module Parameterized_cubics(Param : Parameter_type) =
struct
  module PComplex = Parameterized_complex(Param)
  module Node_base = Cubic_node(Complex_point)
  module PNode_base = Cubic_node(PComplex)

  module Node =
  struct 
    include Node_base

    let parameterize_node node =
      PNode_base.make_node
        (const (rel_inhandle node))
        (const (on_curve node))
        (const (rel_outhandle node))
  end

  module PNode =
  struct
    include PNode_base

    let resolve_node param pnode =
      Node_base.make_node
        ((rel_inhandle pnode) param)
        ((on_curve pnode) param)
        ((rel_outhandle pnode) param)
  end

  let point_list_of_contour contour =
    List.flatten (List.map Node.to_list2 (Contour.to_node_list contour))

  let point_list_of_pcontour contour =
    List.flatten (List.map PNode.to_list2 (Contour.to_node_list contour))

  let print_python_code_for_contour outp contour =
    let point_list = point_list_of_contour contour in
    let point_list = (List.tl point_list) @ [List.hd point_list] in
    output_string outp "fontforge.contour()";
    List.iter
      Complex.(fun (pt, on_curve) ->
        let oc_string = if on_curve then "True" else "False" in
        Print.fprintf outp p"+fontforge.point(%f,%f,%s)" pt.re pt.im oc_string)
      point_list;
end
