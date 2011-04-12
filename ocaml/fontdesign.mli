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
  (** Returns an open contour made from a list of nodes. *)

  val append : 'node t -> 'node t -> 'node t
  (** [append c1 c2] concatenates splines [c1] and [c2]. The result is
      closed if and only if [c1] is closed. *)

  val closed : 'node t -> bool -> 'node t
  val is_closed : 'node t -> bool

  val print : (unit IO.output -> 'node -> unit) -> unit IO.output -> 'node t -> unit

  module Ops :
  sig
    val ( <@> ) : 'node t -> 'node t -> 'node t
    (** [c1 <@> c2] concatenates splines [c1] and [c2]. The result is
        closed if and only if [c1] is closed. *)

    val ( <@@ ) : 'node t -> bool -> 'node t
  (** [c <@@ true] marks contour [c] as closed; [c <@@ false] marks it
      as open. *)
  end
end

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
val adtan2 : float -> float -> float
(** Inverse trigonometric functions returning values in degrees. *)

module Extended_complex :
(** A complex number type with extensions. (You could try
    [module Complex = Fontdesign.Extended_complex] in your code.) *)
sig
  type int = Int.t
  type float = Float.t
  (** Type aliases necessary to make [Extended_complex] a valid
      [Generalized_numeric]. It's also a valid [Point_type]. *)

  include module type of Complex

  val of_pair : float * float -> t
  val to_pair : t -> float * float

  val x' : float -> t
  (** [x' x] returns the complex x + i.0 *)

  val y' : float -> t
  (** [y' y] returns the complex 0 + i.y *)

  val dpolar : float -> float -> t
  (** [dpolar norm arg] returns the complex with norm [norm] and
      argument [arg], where [arg] is in degrees. *)

  val rot : float -> t
  (** [rot theta] returns the complex rotation of angle [theta], where
      [theta] is in degrees; [rot theta] is a shorthand for
      [dpolar 1.0 theta]. *)

  val unit : t -> t
  (** [unit c] returns the unit complex in the same direction as c;
      that is, c/|c|. *)

  val inner : t -> t -> float
  (** The inner product of two complexes regarded as real vectors. *)

  val proj : t -> t -> t
(** [proj a b] returns the geometric projection of complex [a] on
    complex [b]. *)
end

module Cubic_node(P : Point_type) :
sig
  type _node_points = { ih : P.t; oc : P.t; oh : P.t }
  include Node_type with type point = P.t and type t = _node_points
  val make_node : point -> point -> point -> t
  val on_curve : t -> point
  val inhandle : t -> point
  val outhandle : t -> point
  val rel_inhandle : t -> point
  val rel_outhandle : t -> point
end

module Contour : Contour_type
