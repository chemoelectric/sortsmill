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

module type Generalized_numeric =
sig
  type int (* A generalized [int]; for example, a function returning [int]. *)
  type float (* A generalized [float]; for example, a function returning [float]. *)
  include Number.Numeric
end
    
module type Point_type =
sig
  include Generalized_numeric
  val print : unit IO.output -> t -> unit
end

module type Node_type =
sig
  type point
  type t
  include Interfaces.Mappable with type 'a mappable = t
  val map : (point -> point) -> (t -> t)
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

  module Ops :
  sig
    val ( <@> ) : 'node t -> 'node t -> 'node t
    val ( <@@ ) : 'node t -> bool -> 'node t
  end
end

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
  type int = Int.t
  type float = Float.t

  include Complex

  let of_pair (x,y) = { re = x; im = y }
  let to_pair c = (c.re, c.im)

  let x' x = { re = x; im = 0. }
  let y' y = { re = 0.; im = y }

  let dpolar norm arg = polar norm (rad arg)
  let rot theta = polar 1.0 (rad theta)
  let unit c = c / abs(c)

  let inner a b = (a.re *. b.re) +. (a.im *. b.im)

  let proj a b =
    let b' = unit b in
    of_float (inner a b') * b'
end

module Cubic_node(P : Point_type) =
struct
  type point = P.t
  type _node_points = { ih : point; oc : point; oh : point }
  type t = _node_points
  type 'a mappable = t

  let map f p = { ih = f p.ih; oc = f p.oc; oh = f p.oh }

  let make_node rel_inhandle on_curve_point rel_outhandle =
    P.({ ih = on_curve_point + rel_inhandle;
         oc = on_curve_point;
         oh = on_curve_point + rel_outhandle })

  let on_curve p = p.oc
  let inhandle p = p.ih
  let outhandle p = p.oh
  let rel_inhandle p = P.(p.ih - p.oc)
  let rel_outhandle p = P.(p.oh - p.oc)

  let print outp p =
    output_string outp "{ih=";
    P.print outp p.ih;
    output_string outp "; oc=";
    P.print outp p.oc;
    output_string outp "; oh=";
    P.print outp p.oh;
    output_string outp "}"
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

  let append contour1 contour2 =
    { contour1 with spline = contour1.spline @ contour2.spline }

  let closed contour true_or_false =
    { contour with closed = true_or_false }

  let is_closed contour = contour.closed

  let rec print_spline print_node outp spline =
    match spline with
      | [] -> assert false
      | [p] -> print_node outp p
      | p :: remaining ->
        print_node outp p;
        output_string outp "<@>";
        print_spline print_node outp remaining

  let print_closed outp is_closed =
    if is_closed then
      output_string outp "<**true"
    else
      output_string outp "<**false"

  let print print_node outp contour =
    print_spline print_node outp contour.spline;
    print_closed outp contour.closed

  module Ops =
  struct
    let ( <@> ) = append
    let ( <@@ ) = closed
  end
end
