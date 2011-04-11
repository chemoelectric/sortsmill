(** Sorts Mill font design modules. *)

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
  (** ih = incoming handle, oc = on-curve point, oh = outgoing handle *)

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
  (** An open contour consisting of a single on-curve point with
      handles. *)

  val construct : cubic_point list -> t    
  (** An open contour made from a list of on-curve points with
      handles. *)

  val transform : t -> transformation -> t
  (** [transform c m] transforms contour [c] by the transformation
      [m]. *)

  val append : t -> t -> t
  (** [append c1 c2] concatenates splines [c1] and [c2]. The result is
      closed if and only if [c1] is closed. *)

  val closed : t -> bool -> t
  val is_closed : t -> bool

  val print : unit IO.output -> t -> unit

  module Ops :
  sig
    val ( <@> ) : t -> t -> t
    (** [c1 <@> c2] concatenates splines [c1] and [c2]. The result is
        closed if and only if [c1] is closed. *)

    val ( <*> ) : t -> transformation -> t
    (** [c <*> m] transforms contour [c] by the transformation [m]. *)

    val ( <@@ ) : t -> bool -> t
  (** [c <@@ true] marks contour [c] as closed; [c <@@ false] marks it
      as open. *)
  end
end

module Mat :
sig
  type t = float * float * float * float * float * float
  type point = Complex.t
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

module Cubic_point
  (P : Point_type)
  (T : (Point_transformation_type with type point = P.t)) :
  Cubic_point_type with type point = P.t
                   and  type transformation = T.t

module Cubic_contour(CP : Cubic_point_type) :
  Cubic_contour_type with type point = CP.point
                     and type transformation = CP.transformation
                     and type cubic_point = CP.t

val cpx : float -> float -> Complex.t
(** [cpx x y] return the complex x + i.y. *)

val pol : float -> float -> Complex.t
(** [pol norm arg] returns the complex with norm [norm] and arg [arg],
    where [arg] is in degrees. *)

val x' : float -> Complex.t
(** [x' x] returns the complex x + i.0 *)

val y' : float -> Complex.t
(** [y' y] returns the complex 0 + i.y *)

val dir : float -> Complex.t
(** [dir theta] returns the complex of norm 1 and arg [theta], where
    [theta] is in degrees. *)

val deg : float -> float
(** Conversion from radians to degrees. *)

val rad : float -> float
(** Conversion from degrees to radians. *)

val dsin : float -> float
val dcos : float -> float
val dtan : float -> float
(** Trigonometric functions taking their arguments in degrees. *)

val adsin : float -> float
val adcos : float -> float
val adtan : float -> float
val adtan2 x y : float -> float -> float
(** Inverse trigonometric functions returning values in degrees. *)
