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
  (** An open contour consisting of a single on-curve point with zero
      relative handles. *)

  val zero : t
  (** A singleton of the zero point. *)

  val one : t
  (** A singleton of the unit point. *)

  val transform : t -> transformation -> t
  (** [transform c m] transforms contour [c] by the transformation
      [m]. *)

  val set_inhandle : t -> point -> t
  (** [set_inhandle c h] returns contour [c] with its incoming handle
      set to [h] relative to the start point. *)

  val set_outhandle : t -> point -> t
  (** [set_outhandle c h] returns contour [c] with its outgoing handle
      set to [h] relative to the finish point. *)

  module Ops :
  sig
    val ( !. ) : point -> t
    (** [!.p] returns point [p] with zero relative handles. *)

    val ( |= ) : point -> t -> t
    (** [h |= c] returns contour [c] with its incoming handle set to [h]
        relative to the start point. *)

    val ( =| ) : t -> point -> t
    (** [c =| h] returns contour [c] with its outgoing handle set to [h]
        relative to the finish point. *)

    val ( <*> ) : t -> transformation -> t
  (** [c <*> m] transforms contour [c] by the transformation [m]. *)
  end
end

module Mat :
sig
  include Psmat.S
  type point = Complex.t
  val transform : point -> t -> point
end

module Cubic_contour
  (P : Point_type)
  (T : (Point_transformation_type with type point = P.t))
  : Contour_type with type point = P.t
                 and type transformation = T.t

module Ops :
sig
  val ( +! ) : float -> float -> Complex.t
  (** [(x +! y)] returns the complex x + iy. *)

  val ( >! ) : float -> float -> Complex.t
(** [(norm >! arg)] returns the complex having norm [norm] and arg
    [arg], where [arg] is given in degrees. *)
end
