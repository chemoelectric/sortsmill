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
  type bool = Bool.t
  type int = Int.t
  type float = Float.t
  type string = String.t
  (** These type aliases are needed to make Extended_complex a valid
      Point_type. *)

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
  val round : t -> t
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
  val min_bound : t -> t -> t
  val max_bound : t -> t -> t
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
  val print : unit IO.output -> t -> unit
  val t_printer : t Value_printer.t
end

module type Parameter_type =
sig
  type t
end

module Parameterized_complex(Param : Parameter_type) :
sig
  include Point_type with type bool = Param.t -> Bool.t
                     and type int = Param.t -> Int.t
                     and type float = Param.t -> Float.t
                     and type string = Param.t -> String.t
                     and type t = Param.t -> Complex.t
end
  
module Complex_point :
sig
  include module type of Extended_complex
  val of_bezier_point : Caml2geom.Point.t -> t
  val to_bezier_point : t -> Caml2geom.Point.t
end

module type Node_type =
sig
  type element
  type t
  val apply : (element -> element) -> (t -> t)
  val print : unit IO.output -> t -> unit
  val t_printer : t Value_printer.t
end

module Cubic_node(P : Point_type) :
sig
  type point = P.t
  type t = { ih : point; oc : point; oh : point }
  include Node_type with type element = point and type t := t
  val to_list : t -> point list
  val of_list : point list -> t
  val to_list2 : t -> (point * bool) list
  val make_node : point -> point -> point -> t
  val rev : t -> t
  val make_pin : point -> t
  val make_flat : point -> point -> point -> point -> t
  val make_right : point -> point -> point -> t
  val make_left : point -> point -> point -> t
  val make_up : point -> point -> point -> t
  val make_down : point -> point -> point -> t
  val on_curve : t -> point
  val inhandle : t -> point
  val outhandle : t -> point
  val rel_inhandle : t -> point
  val rel_outhandle : t -> point
end

val bezier_curve_to_four_points : Caml2geom.Bezier_curve.t ->
  Complex.t * Complex.t * Complex.t * Complex.t

module Cubic_node_of_complex_point :
sig
  include module type of Cubic_node(Complex_point)
  val bezier_curve : t -> t -> Caml2geom.Cubic_bezier.t
  val of_bezier_curves : Caml2geom.Bezier_curve.t ->
    Caml2geom.Bezier_curve.t -> t
  val bounds : ?fast:bool -> t -> t -> Complex_point.t * Complex_point.t
  val subdivide : t -> t -> float -> t * t * t
end

module type Node_spline_type =
sig
  type +'node t

  include Enum.Enumerable with type 'node enumerable = 'node t
  val backwards : 'node t -> 'node Enum.t
  val of_backwards : 'node Enum.t -> 'node t

  include Interfaces.Mappable with type 'node mappable = 'node t
  val iter : ('node -> unit) -> 'node t -> unit

  val rev : 'node t -> 'node t
  val rev_map : ('node -> 'node) -> 'node t -> 'node t

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

module Node_spline : Node_spline_type

val node_spline_to_cubic_beziers :
  Cubic_node_of_complex_point.t Node_spline.t ->
  bool -> Caml2geom.Cubic_bezier.t list

module Node_contour(Node : Node_type) :
sig
  module Spline : Node_spline_type
  type t

  val with_closed : bool -> t -> t
  val closed : t -> bool
  val print_closed : unit IO.output -> t -> unit
  val print : ?first:string -> ?last:string -> ?sep:string ->
    unit IO.output -> t -> unit
  val t_printer : t Value_printer.t

  val spline : t -> Node.t Spline.t
  val with_spline : Node.t Spline.t -> t -> t

  val of_node_list : Node.t list -> t
  val to_node_list : t -> Node.t list

  val apply_spline_op : t -> (Node.t Spline.t -> Node.t Spline.t) -> t
  val apply_node_op : t -> (Node.t -> Node.t) -> t

  val ( <@@ ) : t -> bool -> t
  val ( <@> ) : t -> t -> t
end

module Cubic_contour(Point : Point_type) :
sig
  module Node : module type of Cubic_node(Point)
  include module type of Node_contour(Node)
  val to_point_bool_list : t -> (Point.t * bool) list
  val rev : t -> t
  val ( <.> ) : t -> (Point.t -> Point.t) -> t
  val ( <*> ) : t -> Point.t -> t
  val ( </> ) : t -> Point.t -> t
  val ( <+> ) : t -> Point.t -> t
  val ( <-> ) : t -> Point.t -> t
end

module Cubic :
sig
  include module type of Cubic_contour(Complex_point)

  module Extended_node :
  sig
    include module type of Node
    val bezier_curve : t -> t -> Caml2geom.Cubic_bezier.t
    val of_bezier_curves : Caml2geom.Bezier_curve.t ->
      Caml2geom.Bezier_curve.t -> t
    val bounds : ?fast:bool -> t -> t -> Complex_point.t * Complex_point.t
    val subdivide : t -> t -> float -> t * t * t
  end

  val to_cubic_beziers : t -> Caml2geom.Cubic_bezier.t list
  val to_path : t -> Caml2geom.Path.t
  val of_path :
    ?closed:bool ->
    ?rel_inhandle:Complex.t ->
    ?rel_outhandle:Complex.t ->
    tolerance:float -> Caml2geom.Path.t -> t
  val bounds : ?fast:bool -> t -> Complex_point.t * Complex_point.t
  val overall_bounds : ?fast:bool ->
    t Enum.t -> Complex_point.t * Complex_point.t
  val print_python_contour_code :
    ?variable:string -> unit BatIO.output -> t -> unit
end

module Parameterized_contour(Param : Parameter_type) :
sig
  module PComplex : module type of Parameterized_complex(Param)
  module PCubic : module type of Cubic_contour(PComplex)

  type t =
    [
    | `Parameterized of Param.t -> t
    | `Cubic of Cubic.t
    | `PCubic of PCubic.t
    ]

  val of_parameterized : (Param.t -> t) -> t
  val of_cubic : Cubic.t -> t
  val of_pcubic : PCubic.t -> t

  val resolve_pcontour_node : PCubic.Node.t -> Param.t -> Cubic.Node.t
  val resolve_pcontour_spline : PCubic.Node.t PCubic.Spline.t ->
    Param.t -> Cubic.Node.t Cubic.Spline.t

  val resolve : t -> Param.t -> Cubic.t

  val bounds2 : ?fast:bool ->
    ([< `Cubic of Cubic.Node.t Cubic.Spline.t * bool
     | `PCubic of PCubic.Node.t PCubic.Spline.t * bool
     | `Parameterized of Param.t -> 'a
     ] as 'a) ->
    (Param.t -> Complex_point.t * Complex_point.t)

  val bounds : ?fast:bool ->
    ([< `Cubic of Cubic.Node.t Cubic.Spline.t * bool
     | `PCubic of PCubic.Node.t PCubic.Spline.t * bool
     | `Parameterized of Param.t -> 'a
     ] as 'a) ->
    PComplex.t * PComplex.t

  val overall_bounds2 :
    ?fast:bool ->
    ([< `Cubic of Cubic.Extended_node.t Cubic.Spline.t * bool
     | `PCubic of PCubic.Node.t PCubic.Spline.t * bool
     | `Parameterized of Param.t -> 'a
     ] as 'a) Enum.t ->
    (Param.t -> Complex_point.t * Complex_point.t)

  val overall_bounds :
    ?fast:bool ->
    ([< `Cubic of Cubic.Extended_node.t Cubic.Spline.t * bool
     | `PCubic of PCubic.Node.t PCubic.Spline.t * bool
     | `Parameterized of Param.t -> 'a
     ] as 'a) Enum.t ->
    PComplex.t * PComplex.t
end

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
    hints : ('float * 'float) list; (* FIXME: really implement hints. *)
  }
  val empty : ('float, 'contour) t
  val transform : ('float1 -> 'float2) -> ('contour1 -> 'contour2) ->
    ('float1, 'contour1) t -> ('float2, 'contour2) t
end

module Cubic_glyph :
sig
  type t = {
    name : string;
    unicode : int option;
    contours : Cubic.t list;
    lsb : float option;
    rsb : float option;
    hints : (float * float) list;
  }
  val empty : t
  val to_glyph : ('a, 'b) Glyph.t -> ('a, 'b) Glyph.t
  val of_glyph : (float, Cubic.t) Glyph.t -> t
  val _print_python_glyph_code :
    glyph_variable:string ->
    contour_variable:string ->
    unit BatIO.output -> t -> unit
  val print_python_glyph_code :
    ?font_variable:string ->
    ?glyph_variable:string ->
    ?contour_variable:string ->
    unit BatIO.output -> t -> unit
  val print_python_glyph_update_module : unit BatIO.output -> t -> unit
end
