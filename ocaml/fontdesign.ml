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

module type Point_type =
sig
  type t
  val zero : t
  val one : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val print : unit IO.output -> t -> unit
end

module type Point_transformation_type =
sig
  type t
  type point
  val ident : t
  val scale : float -> t
  val scalexy : float -> float -> t
  val translate : point -> t
  val rotate : float -> t
  val skew : float -> t
  val mul : t -> t -> t
  val compose : t list -> t
  val inv : t -> t
  val transform : point -> t -> point
  val print : unit IO.output -> t -> unit
end

module type Cubic_point_type =
sig
  type point
  type transformation
  type t = { ih : point; oc : point; oh : point }
  val add : t -> t -> t
  val sub : t -> t -> t
  val absolute : t -> t
  val relative : t -> t
  val transform : t -> transformation -> t
  val print : unit IO.output -> t -> unit
end

module type Cubic_contour_type =
sig
  type t
  type point
  type transformation
  type cubic_point
  val singleton : cubic_point -> t
  val construct : cubic_point list -> t    
  val transform : t -> transformation -> t
  val append : t -> t -> t
  val closed : t -> bool -> t
  val is_closed : t -> bool
  val print : unit IO.output -> t -> unit
  module Ops :
  sig
    val ( <@> ) : t -> t -> t
    val ( <*> ) : t -> transformation -> t
    val ( <@@ ) : t -> bool -> t
  end
end

module Mat =
struct
  include Psmat
  type point = Complex.t

  let translate pt =
    Psmat.translate (pt.Complex.re, pt.Complex.im)

  let transform pt trans =
    let pair = (pt.Complex.re, pt.Complex.im) in
    let (x,y) = Psmat.transform pair trans in
    { Complex.re = x; Complex.im = y }
end

module Cubic_point
  (P : Point_type)
  (T : (Point_transformation_type with type point = P.t)) =
struct
  type point = P.t
  type transformation = T.t
  type t = { ih : point; oc : point; oh : point }

  let add a b = { ih = P.add a.ih b.ih;
                  oc = P.add a.oc b.oc;
                  oh = P.add a.oh b.oh }

  let sub a b = { ih = P.sub a.ih b.ih;
                  oc = P.sub a.oc b.oc;
                  oh = P.sub a.oh b.oh }

  let absolute p = { ih = P.add p.oc p.ih;
                     oc = p.oc;
                     oh = P.add p.oc p.oh }

  let relative p = { ih = P.sub p.ih p.oc;
                     oc = p.oc;
                     oh = P.sub p.oh p.oc }

  let transform p trans = { ih = T.transform p.ih trans;
                            oc = T.transform p.oc trans;
                            oh = T.transform p.oh trans }

  let print outp p =
    output_string outp "{ih=";
    P.print outp p.ih;
    output_string outp "; oc=";
    P.print outp p.oc;
    output_string outp "; oh=";
    P.print outp p.oh;
    output_string outp "}"
end

module Cubic_contour(CP : Cubic_point_type) =
struct
  type point = CP.point
  type transformation = CP.transformation
  type cubic_point = CP.t
  type t = {
    spline : cubic_point list;
    closed : bool;
  }

  let singleton p = { spline = [CP.absolute p]; closed = false }

  let construct p_list =
    { spline = List.map CP.absolute p_list; closed = false }

  let transform contour trans =
    { contour with
      spline = List.map (fun p -> CP.transform p trans) contour.spline }

  let append contour1 contour2 =
    { contour1 with spline = contour1.spline @ contour2.spline }

  let closed contour true_or_false =
    { contour with closed = true_or_false }

  let is_closed contour = contour.closed

  let rec print_spline outp spline =
    match spline with
      | [] -> assert false
      | [p] -> CP.print outp (CP.relative p)
      | p :: remaining ->
        CP.print outp (CP.relative p);
        output_string outp "<@>";
        print_spline outp remaining

  let print_closed outp is_closed =
    if is_closed then
      output_string outp "<**true"
    else
      output_string outp "<**false"

  let print outp contour =
    print_spline outp contour.spline;
    print_closed outp contour.closed

  module Ops =
  struct
    let ( <@> ) = append
    let ( <*> ) = transform
    let ( <@@ ) = closed
  end
end

let cpx x y = { Complex.re = x; Complex.im = y }
let pol norm arg = Complex.polar norm ((Float.pi /. 180.) *. arg)

let x' x = { Complex.re = x; Complex.im = 0. }
let y' y = { Complex.re = 0.; Complex.im = y }
let dir theta = Complex.polar 1.0 ((Float.pi /. 180.) *. theta)
let deg theta = (180. /. Float.pi) *. theta
let rad theta = (Float.pi /. 180.) *. theta

let dsin = sin -| rad
let dcos = cos -| rad
let dtan = tan -| rad

let adsin = deg -| asin
let adcos = deg -| acos
let adtan = deg -| atan
let adtan2 x y = deg (atan2 x y)
