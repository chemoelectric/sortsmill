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
module Crossing : module type of Caml2geom.Crossing

(*-----------------------------------------------------------------------*)

val posmod : int -> int -> int
(** [posmod n k] is similar to [k mod n] but always returns the
    positive modulus. *)

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

  val x_shear : t -> float -> t
  val y_shear : t -> float -> t

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
  val re : t -> float'
  val im : t -> float'
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
  val x_shear : t -> float' -> t
  val y_shear : t -> float' -> t
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

module Cubic_base
  (L : module type of List)
  (P : Point_type) :
sig
  type point = P.t
  type t = (P.t * P.t * P.t) L.t

  val make_node : P.t -> P.t -> P.t -> t
  (** Makes a node with handles specified as vectors relative to the
      on-curve point. (This case is where the "handle" metaphor makes
      the most sense. Another term for a handle is "direction
      point".) *)

  val make_vert_node : P.float' -> P.t -> P.float' -> (P.t * P.t * P.t) L.t
  (** Makes a vertical node with handles at given heights
      (y-coordinates). *)

  val make_horiz_node : P.float' -> P.t -> P.float' -> (P.t * P.t * P.t) L.t
  (** Makes a horizontal node with handles at given x-coordinates. *)

  val make_up_node : P.t -> t
  val make_down_node : P.t -> t
  val make_right_node : P.t -> t
  val make_left_node : P.t -> t
  val make_dir_node : P.t -> P.t -> t

  val is_empty : t -> bool
  val is_singleton : t -> bool
  val rev : t -> t
  val nodewise : ('a -> 'b) -> 'a L.t -> 'b L.t
  val pointwise : ('a -> 'b) -> ('a * 'a * 'a) L.t -> ('b * 'b * 'b) L.t
  val print_point : unit IO.output -> P.t -> unit
  val point_printer : P.t Value_printer.t
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

module Direction_guessing :
sig
  val guess_directions :
    ?start_dir:Complex.t -> ?end_dir:Complex.t ->
    ?start_curl:float -> ?end_curl:float ->
    ?tensions:(float * float) Array.t ->
    Complex.t array -> Complex.t array

  val guess_cycle_directions :
    ?tensions:(float * float) array ->
    Complex.t array -> Complex.t array
end

(*-----------------------------------------------------------------------*)

val find_intersection_of_lines :
  ?first_is_segment:bool ->
  ?second_is_segment:bool ->
  Complex.t * Complex.t ->
  Complex.t * Complex.t ->
  Complex.t * Complex.t

val bezier_curve_to_four_complexes :
  Caml2geom.Bezier_curve.t -> Complex.t * Complex.t * Complex.t * Complex.t

module Cubic :
sig
  include module type of Cubic_base(List)(Complex_point)

  val linear_tolerance : float ref
  val points_coincide : ?tol:float -> Complex.t -> Complex.t -> bool
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

  val curve_point_at : ?pos:int -> t -> float -> Complex.t

  val curve_times_at_x : ?pos:int -> t -> float -> float array
  val curve_times_at_y : ?pos:int -> t -> float -> float array

  val subdivide_curve : ?pos:int -> t -> float -> t * t
  (** Cuts in two the bezier curve between two nodes, returning a pair
      of two-node contours. *)

  val curve_crossings : ?pos1:int -> t -> ?pos2:int -> t -> Crossing.t array

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

  val join : ?tol:float -> t -> t -> t

  val point_at : t -> float -> Complex.t

  val times_at_x : t -> float -> float array
  val times_at_y : t -> float -> float array

  val crossings : t -> t -> Crossing.t array

  val modify_inhandle : t -> Complex.t -> t
  val modify_outhandle : t -> Complex.t -> t

  val remove_inflection_from_curve : ?pos:int -> t -> t

  val apply_tensions : ?pos:int -> ?no_inflection:bool -> t -> float -> float -> t
  val apply_tension : ?pos:int -> ?no_inflection:bool -> t -> float -> t
  val join_with_tensions : ?no_inflection:bool -> float -> float -> t -> t -> t
  val join_with_tension : ?no_inflection:bool -> float -> t -> t -> t
  val close_with_tensions : ?tol:float -> ?no_inflection:bool -> float -> float -> t -> t
  val close_with_tension : ?tol:float -> ?no_inflection:bool -> float -> t -> t

  val to_point_bool_list : t -> (Complex.t * bool) list
  (** Convert a contour to list of points marked true/false =
      on-curve/off-curve. *)

  val print_python_contour_code :
    ?variable:string -> unit IO.output -> t -> unit

  val ( <@> ) : t -> t -> t
  val ( <@@ ) : t -> bool -> t

  val ( <@~~.> ) : t -> float * float -> t -> t
  val ( <@--.> ) : t -> float * float -> t -> t
  val ( <@~.> ) : t -> float -> t -> t
  val ( <@-.> ) : t -> float -> t -> t
  val ( <.~~@> ) : ('a -> 'b) -> 'a -> 'b
  val ( <.~@> ) : ('a -> 'b) -> 'a -> 'b
  val ( <.--@> ) : ('a -> 'b) -> 'a -> 'b
  val ( <.-@> ) : ('a -> 'b) -> 'a -> 'b

  val ( <@~> ) : t -> t -> t
  (** Join contours, with tension 1. *)

  val ( <@-> ) : t -> t -> t
  (** Join contours, with tension "at least 1" (to suppress
      inflection). *)

  val ( <~~@@ ) : t -> float * float -> t
  val ( <--@@ ) : t -> float * float -> t
  val ( <~@@ ) : t -> float -> t
  val ( <-@@ ) : t -> float -> t
end

(*-----------------------------------------------------------------------*)

module Metacubic :
sig
  type knot_side =
    [
    | `Ctrl of Complex.t               (* control point *)
    | `Dir of Complex.t * float        (* (direction, tension) *)
    | `Curl of float * float           (* (curl parameter, tension) *)
    | `Open of float                   (* tension *)
    ]

  type knot = knot_side * Complex.t * knot_side
  type t = knot Vect.t

  val print_knot_side : unit IO.output -> knot_side -> unit
  val knot_side_printer : bool -> unit IO.output -> knot_side -> unit

  val print_knot : unit IO.output -> knot -> unit
  val knot_printer : bool -> unit IO.output -> knot -> unit

  val print : unit IO.output -> t -> unit
  val t_printer : bool -> unit IO.output -> t -> unit

  val knot_side_tension : knot_side -> float
  val set_knot_side_tension : knot_side -> float -> knot_side

  val is_closed : ?tol:float -> t -> bool
  val unclose : ?tol:float -> t -> t
  val close : ?tol:float -> ?in_tension:float -> ?out_tension:float ->
    ?tension:float -> t -> t
  val join : ?tol:float -> ?in_tension:float -> ?out_tension:float ->
    ?tension:float -> t -> t -> t
  val set_incoming_tension : ?tol:float -> t -> float -> t
  val set_outgoing_tension : ?tol:float -> t -> float -> t
  val join_coincident_knots : ?tol:float -> t -> t
  val to_cubic : ?tol:float -> t -> Cubic.t

  val knot :
    ?in_tension:float -> ?out_tension:float ->
    ?in_curl:float -> ?out_curl:float ->
    ?in_dir:Complex.t -> ?out_dir:Complex.t -> ?dir:Complex.t ->
    ?in_control:Complex.t -> ?out_control:Complex.t ->
    Complex.t -> knot
  val dir_knot : Complex.t -> ?in_tension:float -> ?out_tension:float -> Complex.t -> knot
  val left_knot : ?in_tension:float -> ?out_tension:float -> Complex.t -> knot
  val right_knot : ?in_tension:float -> ?out_tension:float -> Complex.t -> knot
  val up_knot : ?in_tension:float -> ?out_tension:float -> Complex.t -> knot
  val down_knot : ?in_tension:float -> ?out_tension:float -> Complex.t -> knot
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
