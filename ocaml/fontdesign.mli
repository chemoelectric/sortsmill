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

(*-----------------------------------------------------------------------*)

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

(*-----------------------------------------------------------------------*)

module Extended_complex :
(** A complex number type with extensions. (You could try
    [module Complex = Fontdesign.Extended_complex] in your code.) *)
sig
  include module type of Complex

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

  val dir : t -> t
  (** [dir c] returns the unit complex in the same direction as c;
      that is, c/|c|. *)

  val round : t -> t
  (** [round c] rounds the components to integral values. *)

  val inner : t -> t -> float
  (** The inner product of two complexes regarded as real vectors. *)

  val proj : t -> t -> t
(** [proj a b] returns the geometric projection of complex [a] on
    complex [b]. *)

  val min_bound : t -> t -> t
  val max_bound : t -> t -> t

  val t_printer : t Value_printer.t
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

module Complex_point :
sig
  include Point_type with type bool' = Bool.t
                     and type int' = Int.t
                     and type float' = Float.t
                     and type string' = String.t
                     and type t = Complex.t
  val of_bezier_point : Caml2geom.Point.t -> t
  val to_bezier_point : t -> Caml2geom.Point.t
end

(*-----------------------------------------------------------------------*)

module Parameterized_complex_point(Param : Parameter_type) :
sig
  include Point_type with type bool' = Param.t -> Bool.t
                     and type int' = Param.t -> Int.t
                     and type float' = Param.t -> Float.t
                     and type string' = Param.t -> String.t
                     and type t = Param.t -> Complex.t
end
  
(*-----------------------------------------------------------------------*)

module Cubic_base
  (L : module type of List)
  (P : Point_type) :
sig
  type point = P.t
  type node = (P.t * P.t * P.t)
  type t = (P.t * P.t * P.t) L.t
  val make_node : P.t -> P.t -> P.t -> t
  val make_pin : P.t -> t
  val make_flat : P.t -> P.t -> P.t -> P.t -> t
  val make_right : P.t -> P.t -> P.t -> t
  val make_left : P.t -> P.t -> P.t -> t
  val make_up : P.t -> P.t -> P.t -> t
  val make_down : P.t -> P.t -> P.t -> t
  val is_empty : t -> bool
  val is_singleton : t -> bool
  val rev : t -> t
  val nodewise : ('a -> 'b) -> 'a L.t -> 'b L.t
  val pointwise : ('a -> 'b) -> ('a * 'a * 'a) L.t -> ('b * 'b * 'b) L.t
  val print_point : unit IO.output -> P.t -> unit
  val point_printer : P.t Value_printer.t
  val print_node : unit IO.output -> P.t * P.t * P.t -> unit
  val node_printer : bool -> unit IO.output -> P.t * P.t * P.t -> unit
  val print : ?first:string -> ?last:string -> ?sep:string ->
    unit IO.output -> t -> unit
  val t_printer : t Value_printer.t

  val ( <.> ) : ('a * 'a * 'a) L.t -> ('a -> 'b) -> ('b * 'b * 'b) L.t
  val ( <*> ) : (P.t * P.t * P.t) L.t -> P.t -> (P.t * P.t * P.t) L.t
  val ( </> ) : (P.t * P.t * P.t) L.t -> P.t -> (P.t * P.t * P.t) L.t
  val ( <+> ) : (P.t * P.t * P.t) L.t -> P.t -> (P.t * P.t * P.t) L.t
  val ( <-> ) : (P.t * P.t * P.t) L.t -> P.t -> (P.t * P.t * P.t) L.t
end

(*-----------------------------------------------------------------------*)

val bezier_curve_to_four_complexes :
  Caml2geom.Bezier_curve.t -> Complex.t * Complex.t * Complex.t * Complex.t

module Cubic :
sig
  include module type of Cubic_base(List)(Complex_point)

  val linear_tolerance : float ref
  val is_closed : ?tol:float -> t -> bool
  val close : ?tol:float -> t -> t
  val unclose : ?tol:float -> t -> t

  val basis_conversion_tolerance : float ref
  (** Default tolerance for transformation of the basis of a 2geom
      path. *)

  val time_error : exn
  (** Invalid_argument exception for an out-of-range time. *)

  val bezier_curve : ?pos:int -> t -> Caml2geom.Cubic_bezier.t
  (** From the nodes at pos and pos + 1, create a bezier curve. *)

  val of_bezier_curves : Caml2geom.Bezier_curve.t -> Caml2geom.Bezier_curve.t -> t
  (** From a pair of bezier curves, create a node. *)

  val curve_bounds : ?fast:bool -> ?pos:int -> t -> Complex.t * Complex.t
  (** Computes an xy-aligned bounding box of the bezier curve between
      two nodes. *)

  val subdivide_curve : ?pos:int -> t -> float -> t * t
  (** Cuts in two the bezier curve between two nodes, returning a pair
      of two-node contours. *)

  val to_cubic_beziers : t -> Caml2geom.Cubic_bezier.t list
  (** Creates a list of bezier curves from a contour. *)

  val to_path : t -> Caml2geom.Path.t
  (** Converts a contour to a 2geom path. *)
    
  val of_path : ?tol:float -> ?rel_inhandle:Complex.t ->
    ?rel_outhandle:Complex.t -> Caml2geom.Path.t -> t
  (** Converts a 2geom path to a contour. *)

  val bounds : ?fast:bool -> t -> Complex.t * Complex.t
  (** Computes an xy-aligned bounding box of a contour. *)

  val overall_bounds : ?fast:bool -> t Enum.t -> Complex.t * Complex.t
  (** Computes an xy-aligned bounding box of an enumeration of
      contours. *)

  val subdivide : t -> float -> t * t
  (** Cut a contour in two. *)

  val to_point_bool_list : t -> (Complex.t * bool) list
  (** Convert a contour to list of points marked true/false =
      on-curve/off-curve. *)

  val print_python_contour_code :
    ?variable:string -> unit IO.output -> t -> unit

  val ( <@@ ) : t -> bool -> t
end

(*-----------------------------------------------------------------------*)

module Parameterized_contour(Param : Parameter_type) :
sig
  module PComplex : module type of Parameterized_complex_point(Param)

  module PCubic :
  sig
    include module type of Cubic_base(List)(PComplex)
    val is_closed : 'a list -> bool
    val close : 'a list -> 'a list
    val unclose : 'a list -> 'a list
    val ( <@@ ) : 'a list -> bool -> 'a list
  end

  type t =
    [
    | `Parameterized of Param.t -> t
    | `Cubic of Cubic.t
    | `PCubic of PCubic.t
    ]

  val of_parameterized : 'a -> [> `Parameterized of 'a ]
  val of_cubic : 'a -> [> `Cubic of 'a ]
  val of_pcubic : 'a -> [> `PCubic of 'a ]

  val resolve_pcubic :
    (('a -> 'b) * ('a -> 'b) * ('a -> 'b)) list ->
    'a -> ('b * 'b * 'b) list
  val resolve :
    ([< `Cubic of ('b * 'b * 'b) list
     | `PCubic of (('c -> 'b) * ('c -> 'b) * ('c -> 'b)) list
     | `Parameterized of 'c -> 'a ]
        as 'a) ->
    'c -> ('b * 'b * 'b) list
  val bounds2 :
    ?fast:bool ->
    ([< `Cubic of Cubic.t
     | `PCubic of
         (('b -> Complex.t) * ('b -> Complex.t) * ('b -> Complex.t))
           list
     | `Parameterized of 'b -> 'a ]
        as 'a) ->
    'b -> Complex.t * Complex.t
  val bounds :
    ?fast:bool ->
    ([< `Cubic of Cubic.t
     | `PCubic of
         (('b -> Complex.t) * ('b -> Complex.t) * ('b -> Complex.t))
           list
     | `Parameterized of 'b -> 'a ]
        as 'a) ->
    ('b -> Complex.t) * ('b -> Complex.t)
  val overall_bounds2 :
    ?fast:bool ->
    ([< `Cubic of Cubic.t
     | `PCubic of
         (('b -> Complex.t) * ('b -> Complex.t) * ('b -> Complex.t))
           list
     | `Parameterized of 'b -> 'a ]
        as 'a)
      Enum.t -> 'b -> Complex.t * Complex.t
  val overall_bounds :
    ?fast:bool ->
    ([< `Cubic of Cubic.t
     | `PCubic of
         (('b -> Complex.t) * ('b -> Complex.t) * ('b -> Complex.t))
           list
     | `Parameterized of 'b -> 'a ]
        as 'a)
      Enum.t -> ('b -> Complex.t) * ('b -> Complex.t)
end

(*-----------------------------------------------------------------------*)

val transform_option : ('a -> 'b) -> 'a option -> 'b option
val transform_pair : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd

module Glyph :
sig
  type (+'float, +'contour) t = {
    name : string;
    unicode : int option;
    contours : 'contour list;
    lsb : 'float option;
    rsb : 'float option;
    hints : ('float * 'float) list;
  }

  val empty : ('float, 'contour) t
  val transform : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) t -> ('b, 'd) t
end

module Cubic_glyph :
sig
  type t = (float, Cubic.t) Glyph.t

  val _print_python_glyph_code :
    glyph_variable:string -> contour_variable:string ->
    unit IO.output -> t -> unit

  val print_python_glyph_code :
    ?font_variable:string -> ?glyph_variable:string ->
    ?contour_variable:string -> unit IO.output -> t -> unit

  val print_python_glyph_update_module : unit IO.output -> t -> unit
end

(*-----------------------------------------------------------------------*)
