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
  val print : 'a IO.output -> t -> unit
end

module type Point_transformation_type =
sig
  type t
  type point
  val transform : point -> t -> point
  val scale : float -> t
end

module type Contour_type =
sig
  type t
  type point
  type transformation
  val singleton : point -> t
  val zero : t
  val one : t
  val transform : t -> transformation -> t
  val set_inhandle : t -> point -> t
  val set_outhandle : t -> point -> t
  module Ops :
  sig
    val ( !. ) : point -> t
    val ( |= ) : point -> t -> t
    val ( =| ) : t -> point -> t
    val ( <*> ) : t -> transformation -> t
  end
end

module Mat =
struct
  include Psmat
  type point = Complex.t

  let transform pt trans =
    let pair = (pt.Complex.re, pt.Complex.im) in
    let (x,y) = Psmat.transform pair trans in
    { Complex.re = x; Complex.im = y }
end

module Cubic_contour
  (P : Point_type)
  (T : (Point_transformation_type with type point = P.t)) =
struct
  type point = P.t
  type transformation = T.t
  type t = {
    spline : (point * point * point) list;
    closed : bool;
  }

  let singleton pt = {
    spline = [ (pt, pt, pt) ];
    closed = false;
  }

  let zero = singleton P.zero
  let one = singleton P.one

  let transform contour trans = {
    contour with
      spline =
      List.map
        (fun (inhandle, p, outhandle) ->
          (T.transform inhandle trans,
           T.transform p trans,
           T.transform outhandle trans))
        contour.spline
  }

  let set_inhandle contour rel_handle =
    let (_,p,outh) = List.hd contour.spline in
    let new_point = (P.add p rel_handle, p, outh) in
    { contour with spline = new_point :: (List.tl contour.spline) }

  let set_outhandle contour rel_handle =
    let spline = List.rev contour.spline in
    let (inh,p,_) = List.hd spline in
    let new_point = (inh, p, P.add p rel_handle) in
    let new_spline = new_point :: (List.tl spline) in
    { contour with spline = List.rev new_spline }

  module Ops =
  struct
    let ( !. ) = singleton
    let ( |= ) rel_handle my_mark = set_inhandle my_mark rel_handle
    let ( =| ) = set_outhandle
    let ( <*> ) = transform
    let ( *> ) contour a = transform contour (T.scale a)
  end
end

module TEST_SIGNATURE_CONSTRAINT : Contour_type = Cubic_contour(Complex)(Mat)

module Ops =
struct
  let ( +! ) x y = { Complex.re = x; Complex.im = y }
  let ( >! ) norm arg = Complex.polar norm ((Float.pi /. 180.) *. arg)
end
