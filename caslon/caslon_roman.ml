(*
  Copyright (c) 2011 Barry Schwartz
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
  
  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.
  
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
*)

(*-----------------------------------------------------------------------*)

open Batteries
open OptParse
module StringMap = Map.StringMap

module Complex = Fontdesign.Extended_complex
open Fontdesign

module Param =
struct
  type bracket = {
    bracket_horiz : float;
    bracket_vert : float;
    bracket_horiz_tension : float;
    bracket_vert_tension : float;
  }

  type t = {
    version : string;

    fontname : string;
    familyname : string;
    fullname : string;
    weight : string;

    family : string;
    subfamily : string;
    preferred_family : string option;
    preferred_subfamily : string option;
    wws_family : string option;
    wws_subfamily : string option;

    os2_weight : int;

    contrast : float;                   (* 0. = "normal" contrast. *)
    extension : float;                  (* 0. = normal. *)
    design_size : float;
    space_width : float;
    x_height : float;
    curve_overshoot : float;
    curve_undershoot : float;
    flag_overshoot : float;
    e_crossbar_height : float;
    t_crossbar_height : float;
    t_top_height : float;
    i_dot_height : float;
    ascender_height : float;
    lc_stem_width : float;
    lc_serif_height : float;

    corner_radius : Random.State.t -> float;
    flag_corner_radius : Random.State.t -> float;
    flag_top_corner_radius : Random.State.t -> float;
    serif_end_angle : Random.State.t -> float;
    left_bracket: Random.State.t -> bracket;
    right_bracket: Random.State.t -> bracket;
  }

  module type Tools_module =
  sig
    val rand : Random.State.t
    val width : float
    val height : float
    val left_overlap : float
    val overshoot : float
    val undershoot : float
    val xrel : float -> float
    val yrel : float -> float
    val xpos : float -> float
    val ypos : float -> float
    val x'rel : float -> Complex.t
    val y'rel : float -> Complex.t
    val x'pos : float -> Complex.t
    val y'pos : float -> Complex.t
  end

  let make_tools
      ~glyph_name
      ?(width = nan)
      ?(height = nan)
      ?(left_overlap = 0.)
      ?(overshoot = 0.)
      ?(undershoot = 0.)
      ~param
      () =

    Complex_point.(
      let p = param in
      let rand =
        Random.State.make
          (Array.of_list (int_of_float p.design_size :: p.os2_weight ::
                            List.map Char.code (String.explode glyph_name)))
      in
      let extended_width = width *. (1. +. p.extension) in
      let xrel v = v *. extended_width in
      let yrel v = v *. height in
      let xpos v = xrel v -. left_overlap in
      let ypos v = yrel v -. undershoot in
      let x'rel = x' -| xrel in
      let y'rel = y' -| yrel in
      let x'pos = x' -| xpos in
      let y'pos = y' -| ypos in

      let module Tools =
          struct
            let rand = rand
            let width = extended_width
            let height = height
            let left_overlap = left_overlap
            let overshoot = overshoot
            let undershoot = undershoot
            let xrel = xrel
            let yrel = yrel
            let xpos = xpos
            let ypos = ypos
            let x'rel = x'rel
            let y'rel = y'rel
            let x'pos = x'pos
            let y'pos = y'pos
          end
      in
      (module Tools : Tools_module)
    )
end

open Param

(*-----------------------------------------------------------------------*)

let glyph_table : (Param.t -> (float, Cubic.t) Glyph.t) StringMap.t ref =
  ref StringMap.empty

let add_glyph glyph_name glyph = glyph_table := Glyph.(StringMap.add glyph_name glyph !glyph_table)
let have_glyph name = StringMap.mem name !glyph_table
let get_glyph name = StringMap.find name !glyph_table
let enum_glyphs () = map snd (StringMap.enum !glyph_table)
let get_resolve_glyph name param = (get_glyph name) param
let enum_resolve_glyphs param = map (fun glyph -> glyph param) (enum_glyphs ())
;;

(*-----------------------------------------------------------------------*)

let huge = 1e100 ;;

let make_dot diameter =
  Complex_point.(Metacubic.(
    let radius = 0.5 *. diameter in
    (point ~dir:upward (x'(-.radius)) <@> point ~dir:rightward (y' radius)
     <@> point ~dir:downward (x' radius) <@> point ~dir:leftward (y'(-.radius)) |> close)
  ))

let bend_points ~corner_point ~radius ~in_dir ~out_dir =
  let theta = Complex.(arg (out_dir / in_dir)) in
  let tangent_distance = Complex.of_float (abs_float (radius *. tan (0.5 *. theta))) in
  let point1 = Complex.(corner_point - tangent_distance * in_dir) in
  let point2 = Complex.(corner_point + tangent_distance * out_dir) in
  (point1, point2)
  
let extremum_free_bend ~point1 ~point2 ~in_dir ~out_dir =
  let v12 = Complex.(point2 - point1) in
  if v12 = Complex.zero then
    Metacubic.point ~in_dir ~out_dir point1
  else
    let (inx, iny) = Complex.(in_dir.re, in_dir.im) in
    let (outx, outy) = Complex.(out_dir.re, out_dir.im) in
    let (vx, vy) = Complex.(v12.re, v12.im) in
    let dir1x = if inx *. vx < 0. then 0. else inx in
    let dir1y = if iny *. vy < 0. then 0. else iny in
    let dir2x = if outx *. vx < 0. then 0. else outx in
    let dir2y = if outy *. vy < 0. then 0. else outy in
    Metacubic.(Complex_point.(
      point ~in_dir ~out_dir:(x' dir1x + y' dir1y) point1
      <@-> point ~in_dir:(x' dir2x + y' dir2y) ~out_dir point2
    ))

let make_flat_cut ~corner1 ~corner2 ~radius1 ~radius2 ~tension ~in_dir ~out_dir =
  let cut_dir = Complex_point.(dir (corner2 - corner1)) in
  let (p1, p2) = bend_points ~corner_point:corner1 ~radius:radius1 ~in_dir ~out_dir:cut_dir in
  let (p3, p4) = bend_points ~corner_point:corner2 ~radius:radius2 ~in_dir:cut_dir ~out_dir in
  let bend1 = extremum_free_bend ~point1:p1 ~point2:p2 ~in_dir ~out_dir:cut_dir in
  let bend2 = extremum_free_bend ~point1:p3 ~point2:p4 ~in_dir:cut_dir ~out_dir in
  Metacubic.(bend1 <@-.> tension <.-@> bend2)

let make_end_of_left_serif ~height ~shear_angle ~bottom_radius ~top_radius ~tension =
  let shear_offset = 0.5 *. height *. dtan shear_angle in
  let corner1 = Complex_point.(x'(-.shear_offset)) in
  let corner2 = Complex_point.(x' shear_offset + y' height) in
  make_flat_cut ~corner1 ~corner2 ~radius1:bottom_radius ~radius2:top_radius ~tension
    ~in_dir:Complex_point.leftward ~out_dir:Complex_point.rightward

let make_end_of_right_serif ~height ~shear_angle ~bottom_radius ~top_radius ~tension =
  let shear_offset = 0.5 *. height *. dtan shear_angle in
  let corner1 = Complex_point.(x' shear_offset + y' height) in
  let corner2 = Complex_point.(x'(-.shear_offset)) in
  make_flat_cut ~corner1 ~corner2 ~radius1:top_radius ~radius2:bottom_radius ~tension
    ~in_dir:Complex_point.rightward ~out_dir:Complex_point.leftward

let make_flag
    ~top_corner ~right_point
    ~left_notch ~left_point
    ~flag_corner
    ~top_corner_radius ~flag_corner_radius (* Radii from corner to on-curve point. *)
    ~top_cupping =
  Complex_point.(
    let lower_dir = dir (flag_corner - left_notch) in
    let upper_dir = dir (top_corner - flag_corner) in
    let top_point = top_corner - x' top_corner_radius * upper_dir in
    let flag_upper = flag_corner + x' flag_corner_radius * upper_dir in
    let flag_lower = flag_corner - x' flag_corner_radius * lower_dir in
    let notch_point = left_notch + x' 20. * lower_dir in
    let cupping_vector = x' top_cupping * upper_dir * rot (-.90.) in
    Metacubic.(
      point left_point
      <@> point notch_point
      <@~.> 1.5 <.~@> point ~dir:lower_dir flag_lower
      <@> point ~out_control:(x' 0.35 * flag_upper + x' 0.65 * top_point + cupping_vector) flag_upper
      <@> point ~in_control:(top_point - x' (0.3 *. top_corner_radius)) top_point
      <@-> point ~in_dir:downward ~out_control:(left_point - i) right_point
    )
  )

(*-----------------------------------------------------------------------*)

(* The letter "c" *)

let letter_c_contours glyph_name p =

  let tools =
    let overshoot = p.curve_overshoot +. 3. in
    let undershoot = p.curve_undershoot +. 3. in
    make_tools
      ~glyph_name
      ~width:320.
      ~height:(p.x_height +. undershoot +. overshoot)
      ~undershoot:undershoot
      ~overshoot:overshoot
      ~param:p
      ()
  in
  let module Tools = (val tools : Tools_module) in
  
  Tools.(Cubic.(Complex_point.(

    let contour =

      let left_breadth = 1.12 *. p.lc_stem_width in
      let bottom_breadth = 0.92 *. p.lc_stem_width /. (1. +. p.contrast) in
      let top_breadth = 0.64 *. p.lc_stem_width /. (1. +. p.contrast) in
      let head_breadth = 1.15 *. p.lc_stem_width /. (1. +. 0.3 *. p.contrast) in
      let tail_breadth = 0.25 *. p.lc_stem_width in

      let tail_cut_angle = -35. in
      let tail1 = x'pos 0.98 + y'pos 0.18 in
      let tail2 = tail1 - y_shear (x' tail_breadth) tail_cut_angle in

      let head1 = x'pos 0.98 + y'pos 0.84 in

      let outer =
        Metacubic.(
          set_dirs (
            point ~out_curl:1.5 tail1                    (* tail *)
            <@-> point ~dir:leftward (x'pos 0.56 + y'pos 0.00) (* bottom *)
            <@-.> 0.95 <.-@> point ~dir:upward (x'pos 0.0 + y'pos 0.50) (* left *)
            <@-> point ~dir:rightward (x'pos 0.62 + y'pos 1.00) (* top *)
            <@-> point ~in_curl:1.5 head1                (* head *)
          )   
        )
      in
      let across_head_dir = Metacubic.outgoing_dir outer / rot 90. in
      let head2 = head1 + (x' head_breadth) * across_head_dir in
      let top = x'pos 0.60 + y'pos 1.00 - y' top_breadth in
      let partway_pt = head2 + x' 0.5 * (top - head2) + x'(0.1 *. (re head2 -. re top))  in
      let inner =
        Metacubic.(
          set_dirs (
            point head2
            <@-.> 1000. <.-@> point partway_pt
            <@-.> 0.75 <.-@> point ~dir:leftward top   (* inner top *)
            <@-> point ~dir:downward (x'pos 0.00 + x' left_breadth + y'pos 0.52) (* inner left *)
            <@-> point ~dir:rightward (x'pos 0.64 + y'pos 0.00 + y' bottom_breadth) (* inner bottom *)
            <@-> point ~in_curl:1.5 tail2 (* tail *)
          )
        )
      in
      let outer' = Metacubic.to_cubic outer in
      let inner' = Metacubic.to_cubic inner in
      let c = outer' <@-> inner' in
      let (x_times, y_times) = curve_extrema_and_inflections ~pos:Int.(Vect.length outer - 1) c in
      let y_time = y_times.(0) in
      let x_time = x_times.(0) /. y_time in
      let (c2,c3) = subdivide c (float_of_int Int.(Vect.length outer - 1) +. y_time) in
      let (c1,c2) = subdivide c2 (float_of_int Int.(Vect.length outer - 1) +. x_time) in
      c1 <@> c2 <@> c3 <-@@ 1.
    in
    [contour |> Cubic.round]
  )))
;;

(*.......................................................................*)

(* The letter "e" *)

let letter_e_contours glyph_name p =

  let tools =
    let overshoot = p.curve_overshoot +. 2. in
    let undershoot = p.curve_undershoot +. 2. in
    make_tools
      ~glyph_name
      ~width:354.
      ~height:(p.x_height +. undershoot +. overshoot)
      ~undershoot:p.curve_undershoot
      ~overshoot:p.curve_overshoot
      ~param:p
      ()
  in
  let module Tools = (val tools : Tools_module) in

  Tools.(Cubic.(Complex_point.(

    let top_breadth = 0.41 *. p.lc_stem_width /. (1. +. p.contrast) in
    let left_breadth = 1.01 *. p.lc_stem_width in
    let right_breadth = 1.68 *. p.lc_stem_width /. (1. +. 0.5 *. p.contrast) in
    let bottom_breadth = 1.00 *. p.lc_stem_width /. (1. +. p.contrast) in
    let tail_breadth = 0.37 *. p.lc_stem_width /. (1. +. p.contrast) in

    let crossbar_breadth = 0.42 *. p.lc_stem_width /. (1. +. p.contrast) in
    let crossbar_height = p.e_crossbar_height in
    let crossbar_top = p.e_crossbar_height +. crossbar_breadth in
    let crossbar_fillet_size = crossbar_breadth in
    let crossbar_top0 = x'pos 1.00 - x' right_breadth + y' crossbar_top in

    let tail_cut_angle = 145. in
    let tail1 = x'pos 1.00 + y'pos 0.25 in
    let tail2 = tail1 + x' tail_breadth * rot tail_cut_angle in

    let counter =
      Metacubic.(
        to_cubic (
          point ~dir:upward (crossbar_top0 + y' crossbar_fillet_size)
          <@-> point ~dir:leftward (x'pos 0.50 + y'pos 1.00 - y' top_breadth) (* eye top *)
          <@-> point ~dir:downward (x'pos 0.00 + x' left_breadth + y'pos 0.52) (* inner left *)
          <@-> point ~dir:rightward (x'pos 0.60 + y'pos 0.00 + y' bottom_breadth) (* inner bottom *)
          <@-> point ~in_curl:1.0 tail2 (* inner tail *)
        )
      )
    in
    let crossbar_height_point =
      let time = (curve_times_at_y counter ~pos:1 crossbar_height).(0) in
      curve_point_at counter ~pos:1 time
    in
    let (upper, lower) =
      let intersection_time =
        (curve_times_at_y ~pos:1 counter (crossbar_height -. crossbar_fillet_size)).(0) +. 1.
      in
      subdivide counter intersection_time
    in
    let main_contour =
        Metacubic.(
          to_cubic (
            point ~out_curl:1.0 tail1 (* outer tail *)
            <@-> point ~dir:leftward (x'pos 0.54 + y'pos 0.00) (* bottom *)
            <@-.> 0.9 <.-@> point ~dir:upward (x'pos 0.00 + y'pos 0.51) (* left *)
            <@-> point ~dir:rightward (x'pos 0.52 + y'pos 1.00) (* top *)
            <@-> point ~dir:downward (x'pos 0.95 + y' crossbar_height + y'rel 0.02) (* right *)
            <@-> point ~dir:leftward (x'pos 0.85 + y' crossbar_height) (* crossbar right *)
            <@-.> huge <.-@> point ~dir:leftward (crossbar_height_point + x' crossbar_fillet_size) (* crossbar left *)
            <@-> set_dirs (of_cubic lower) (* inner bowl *)
            |> close ~tension:2.
          )
        )
    in
    let crossbar_top1 =
      let time = (curve_times_at_y upper ~pos:1 crossbar_top).(0) in
      curve_point_at upper ~pos:1 time
    in
    let eye_upper =
      let left_intersection_time =
        (curve_times_at_y upper ~pos:1 (crossbar_top +. crossbar_fillet_size)).(0) +. 1.
      in
      let (upper', _) = subdivide upper left_intersection_time in
      upper'
    in
    let eye_contour =
        Metacubic.(
          to_cubic (
            point ~dir:rightward (crossbar_top1 + x' crossbar_fillet_size) (* crossbar top left *)
            <@-.> huge <.-@> point ~dir:rightward (crossbar_top0 - x' crossbar_fillet_size) (* crossbar top right *)
            <@-> set_dirs ~guess:false (of_cubic eye_upper) |> close
          )
        )
    in
    [main_contour |> Cubic.round;
     eye_contour |> Cubic.round]
  )))
;;

(*.......................................................................*)

(* Letters such as "dotlessi" and "l" *)

let contours_similar_to_letter_l
    ~height
    ~left_side_shear
    ~right_side_shear
    ~left_serif_width
    ~right_serif_width
    ~left_bracket
    ~right_bracket
    ~extra_stem_width
    ~top_cupping
    glyph_name p =

  let tools =
    make_tools
      ~glyph_name
      ~height
      ~overshoot:p.flag_overshoot
      ~param:p
      ()
  in
  let module Tools = (val tools : Tools_module) in

  Tools.(Cubic.(Complex_point.(

    let stem_width = p.lc_stem_width +. extra_stem_width in
    let serif_height = p.lc_serif_height in
    let left_pos = (-0.5) *. stem_width in
    let right_pos = 0.5 *. stem_width in
    let overshot_height = height +. overshoot in
    let serif_to_top = overshot_height -. serif_height in

    let serif_end_angle () = p.serif_end_angle rand in
    let corner_radius () = p.corner_radius rand in
    let left_serif_end =
      make_end_of_left_serif
        ~height:(serif_height +. 4.)
        ~shear_angle:(serif_end_angle ())
        ~bottom_radius:(corner_radius ())
        ~top_radius:(corner_radius ())
        ~tension:huge |>
            (Metacubic.translate (x' left_pos - x' left_serif_width - y' 3.))
    in
    let right_serif_end =
      make_end_of_right_serif
        ~height:(serif_height +. 4.)
        ~shear_angle:(serif_end_angle ())
        ~bottom_radius:(corner_radius ())
        ~top_radius:(corner_radius ())
        ~tension:huge |>
            (Metacubic.translate (x' right_pos + x' right_serif_width - y' 3.))
    in

    let top_corner_radius = p.flag_top_corner_radius rand in
    let flag_corner_radius = p.flag_corner_radius rand in
    
    let left_notch =
      x' left_pos + y' serif_height + (x_shear (y'(serif_to_top -. 105.)) left_side_shear)
    in
    let left_point =
      x' left_pos + y' serif_height + (x_shear (y'(serif_to_top -. 105. -. 40.)) left_side_shear)
    in
    let top_corner =
      x' right_pos + y' serif_height + (x_shear (y' serif_to_top) right_side_shear)
    in
    let right_point =
      x' right_pos + y' serif_height + (x_shear (y'(serif_to_top -. top_corner_radius)) right_side_shear)
    in
    let flag_corner =
      x'(re left_notch -. 70.) + y'(im top_corner -. 58.)
    in
    let flag =
      make_flag
        ~top_corner ~right_point
        ~left_notch ~left_point
        ~flag_corner
        ~top_corner_radius ~flag_corner_radius
        ~top_cupping
    in
    let leftbrack = left_bracket rand in
    let rightbrack = right_bracket rand in
    let left_side =
      Metacubic.(
        point ~dir:leftward zero
        <@--.> (1.,2.) <.--@> left_serif_end
        <@--.> (2.,1.) <.--@> point ~dir:rightward (x' left_pos + y' serif_height - x' leftbrack.bracket_horiz)
        <@--.>
          (leftbrack.bracket_horiz_tension,
           leftbrack.bracket_vert_tension)
        <.--@> point ~dir:upward (x' left_pos + y' serif_height + y' leftbrack.bracket_vert)
        <@-> point left_point
      )
    in
    let right_stem_point = x' right_pos + y' serif_height + y' rightbrack.bracket_vert in
    let right_side =
      Metacubic.(
        point ~in_dir:downward ~out_control:(right_point - y' 20.) right_point
        <@> point ~in_control:(right_stem_point + y'(0.7 *. serif_to_top)) ~out_dir:downward right_stem_point
        <@--.>
          (rightbrack.bracket_vert_tension,
           rightbrack.bracket_horiz_tension)
        <.--@> point ~dir:rightward (x' right_pos + y' serif_height + x' rightbrack.bracket_horiz)
        <@--.> (1.,2.) <.--@> right_serif_end
        <@--.> (2.,1.) <.--@> point ~dir:leftward zero
      )
    in
    let contour =
      Metacubic.(
        to_cubic (flag <@> right_side <@> left_side)
      )
    in
    [contour |> Cubic.round]
  )))
;;

(*.......................................................................*)

(* The letter "dotlessi" *)

let letter_dotlessi_contours glyph_name p =
  contours_similar_to_letter_l
    ~height:(p.x_height +. p.flag_overshoot)
    ~left_side_shear:0.
    ~right_side_shear:1.2
    ~left_serif_width:65.
    ~right_serif_width:60.
    ~left_bracket:(fun state ->
      let lbrack = p.left_bracket state in
      { lbrack with bracket_horiz = lbrack.bracket_horiz +. 5. }
    )
    ~right_bracket:p.left_bracket
    ~extra_stem_width:0.
    ~top_cupping:4.
    glyph_name p

(*.......................................................................*)

(* The letter "i" *)

let letter_i_contours glyph_name p =

  let tools =
    make_tools
      ~glyph_name
      ~param:p
      ()
  in
  let module Tools = (val tools : Tools_module) in

  Tools.(Cubic.(Complex_point.(
    let dot_contour =
      Metacubic.to_cubic (make_dot 108.) <+> y' p.i_dot_height - x' 10.
    in
    letter_dotlessi_contours glyph_name p @ [dot_contour]
  )))
;;

(*.......................................................................*)

(* The letter "l" *)

let letter_l_contours glyph_name p =
  contours_similar_to_letter_l
    ~height:(p.ascender_height +. 2.)
    ~left_side_shear:(-0.5)
    ~right_side_shear:1.2
    ~left_serif_width:105.
    ~right_serif_width:85.
    ~left_bracket:p.left_bracket
    ~right_bracket:p.left_bracket
    ~extra_stem_width:3.
    ~top_cupping:2.
    glyph_name p

(*.......................................................................*)

(* The letter "o" *)

let letter_o_contours glyph_name p =

  let tools =
    let overshoot = p.curve_overshoot in
    let undershoot = p.curve_undershoot in
    make_tools
      ~glyph_name
      ~width:383.
      ~height:(p.x_height +. undershoot +. overshoot)
      ~undershoot:undershoot
      ~overshoot:overshoot
      ~param:p
      ()
  in
  let module Tools = (val tools : Tools_module) in

  Tools.(Cubic.(Complex_point.(

    let left_breadth = 1.16 *. p.lc_stem_width in
    let right_breadth = 1.16 *. p.lc_stem_width in
    let bottom_breadth = 0.58 *. p.lc_stem_width /. (1. +. p.contrast) in
    let top_breadth = 0.54 *. p.lc_stem_width /. (1. +. p.contrast) in

    let outer_contour =
      make_up_node (x'pos 0.00 + y'pos 0.50) (* left *)
      <@-> make_right_node (x'pos 0.50 + y'pos 1.00) (* top *)
      <@-> make_down_node (x'pos 1.00 + y'pos 0.50)  (* right *)
      <@-> make_left_node (x'pos 0.49 + y'pos 0.00)  (* bottom *)
      <-@@ 0.95
    in
    let inner_contour =
      make_down_node (x'pos 0.00 + x' left_breadth + y'pos 0.52) (* left *)
      <@-.> 0.98 <.-@> make_right_node (x'pos 0.49 + y'pos 0.00 + y' bottom_breadth) (* bottom *)
      <@-> make_up_node (x'pos 1.00 - x' right_breadth + y'pos 0.48) (* right *)
      <@-> make_left_node (x'pos 0.48 + y'pos 1.00 - y' top_breadth) (* top *)
      <-@@ 1.0
    in
    [outer_contour |> Cubic.round;
     inner_contour |> Cubic.round]
  )))
;;

(*-----------------------------------------------------------------------*)

(* The letter "t" *)

let letter_t_contours glyph_name p =

  let tools =
    let undershoot = p.curve_undershoot +. 2. in
    make_tools
      ~glyph_name
      ~width:265.
      ~height:(p.t_top_height +. undershoot)
      ~undershoot:undershoot
      ~param:p
      ()
  in
  let module Tools = (val tools : Tools_module) in

  Tools.(Metacubic.(Complex_point.(

    let stem_width = p.lc_stem_width in
    let sheared_terminal_width = 55. in
    let left_pos = x'(-.0.5 *. stem_width) in
    let right_pos = x'(0.5 *. stem_width) in
    let crossbar_height = p.t_crossbar_height in
    let crossbar_breadth = 0.80 *. stem_width /. (1. +. p.contrast) in
    let bottom_breadth = 0.60 *. stem_width /. (1. +. p.contrast) in
    let tail_breadth = 0.40 *. stem_width /. (1. +. p.contrast) in
    let tail_cut_angle = 100. in

    let tail1 = x'(width -. 0.5 *. stem_width -. sheared_terminal_width) + y'pos 0.00 + y'(bottom_breadth +. 5.) in
    let tail2 = tail1 + x' tail_breadth * rot tail_cut_angle in
    let lower_bowl_width = re (x'pos 1.00 - x'(stem_width +. sheared_terminal_width)) in
    let upper_bowl_width = re tail2 -. re right_pos in

    let top_right = right_pos + x' 18. + y'pos 1.00 in
    let crossbar_bend = right_pos + y' (crossbar_height -. crossbar_breadth) in
    let bowl_point = right_pos + x'(0.55 *. upper_bowl_width) + y'pos 0.00 + y' bottom_breadth in
    let bottom_point = left_pos + x'(0.50 *. lower_bowl_width) + y'pos 0.00 in

    let right_side =
      point ~out_curl:1. top_right
      <@-.> 5.0 <.-@> point ~dir:downward crossbar_bend
      <@-> point ~dir:downward (right_pos + y'pos 0.30)
      <@-> point ~dir:rightward bowl_point
      <@-> point ~in_curl:1. tail2
    in 
    let left_side =
      point ~out_curl:1. tail1
      <@-> point ~dir:leftward bottom_point
      <@-> point ~dir:upward (left_pos + y'pos 0.30)
    in
    [to_cubic (set_dirs right_side <@> set_dirs left_side)]
  )))
;;

(*-----------------------------------------------------------------------*)

add_glyph "space"
  Glyph.(fun p -> {
    empty with
      name = "space";
      rsb = Some p.space_width;
  })
;;

add_glyph "c"
  Glyph.(fun p -> {
    empty with
      name = "c";
      contours = letter_c_contours "c" p;
      lsb = Some 0.;
      rsb = Some 50.;
  })
;;

add_glyph "e"
  Glyph.(fun p -> {
    empty with
      name = "e";
      contours = letter_e_contours "e" p;
      lsb = Some 0.;
      rsb = Some 50.;
  })
;;

add_glyph "dotlessi"
  Glyph.(fun p -> {
    empty with
      name = "dotlessi";
      contours = letter_dotlessi_contours "dotlessi" p;
      lsb = Some 0.;
      rsb = Some 50.;
  })
;;

add_glyph "i"
  Glyph.(fun p -> {
    empty with
      name = "i";
      contours = letter_i_contours "i" p;
      lsb = Some 0.;
      rsb = Some 50.;
  })
;;

add_glyph "l"
  Glyph.(fun p -> {
    empty with
      name = "l";
      contours = letter_l_contours "l" p;
      lsb = Some 0.;
      rsb = Some 50.;
  })
;;

add_glyph "o"
  Glyph.(fun p -> {
    empty with
      name = "o";
      contours = letter_o_contours "o" p;
      lsb = Some 0.;
      rsb = Some 50.;
  })
;;

add_glyph "t"
  Glyph.(fun p -> {
    empty with
      name = "t";
      contours = letter_t_contours "t" p;
      lsb = Some 0.;
      rsb = Some 50.;
  })
;;

(*-----------------------------------------------------------------------*)

let run_command ?(outp = stdout) param =
  let glyph_opt = StdOpt.str_option () in
  let font_opt = StdOpt.str_option () in
  let font_flags_opt = StdOpt.str_option () in
  let sfd_opt = StdOpt.str_option () in
  let opt_parser = OptParser.make () in
  OptParser.add ~short_name:'g' ~long_name:"glyph" opt_parser glyph_opt;
  OptParser.add ~short_name:'f' ~long_name:"font" opt_parser font_opt;
  OptParser.add ~short_name:'l' ~long_name:"flags" opt_parser font_flags_opt;
  OptParser.add ~short_name:'s' ~long_name:"sfd" opt_parser sfd_opt;
  let _args = OptParser.parse_argv opt_parser in
  match Opt.opt glyph_opt with

    | None ->
      output_string outp "#!/usr/bin/env python\n";
      output_string outp "\n";
      output_string outp "import fontforge\n";
      output_string outp "import psMat\n";
      output_string outp "\n";
      output_string outp "my_font = fontforge.font()\n";
      output_string outp "\n";
      Print.printf p"my_font.version = '%s'\n" param.version;
      Print.printf p"my_font.fontname = '%s'\n" param.fontname;
      Print.printf p"my_font.familyname = '%s'\n" param.familyname;
      Print.printf p"my_font.fullname = '%s'\n" param.fullname;
      Print.printf p"my_font.weight = '%s'\n" param.weight;
      Print.printf p"my_font.appendSFNTName('English (US)', 'Family', '%s')\n" param.family;
      Print.printf p"my_font.appendSFNTName('English (US)', 'SubFamily', '%s')\n" param.subfamily;
      if Option.is_some param.preferred_family then
        Print.printf p"my_font.appendSFNTName('English (US)', 'Preferred Family', '%s')\n"
          (Option.get param.preferred_family);
      if Option.is_some param.preferred_subfamily then
        Print.printf p"my_font.appendSFNTName('English (US)', 'Preferred Styles', '%s')\n"
          (Option.get param.preferred_subfamily);
      if Option.is_some param.wws_family then
        Print.printf p"my_font.appendSFNTName('English (US)', 'WWS Family', '%s')\n"
          (Option.get param.wws_family);
      if Option.is_some param.wws_subfamily then
        Print.printf p"my_font.appendSFNTName('English (US)', 'WWS Subfamily', '%s')\n"
          (Option.get param.wws_subfamily);
      output_string outp "\n";
      Print.printf p"my_font.size_feature = (%F,)\n" param.design_size;
      output_string outp "\n";
      Print.printf p"my_font.os2_weight = %i\n" param.os2_weight;
      output_string outp "\n";
      output_string outp "def preferred_unicode(glyphname):\n";
      output_string outp "    if '_' in glyphname:\n";
      output_string outp "        uni = -1\n";
      output_string outp "    else:\n";
      output_string outp "        uni = fontforge.unicodeFromName(glyphname)\n";
      output_string outp "    return uni\n";
      output_string outp "\n";

      iter
        (Cubic_glyph.print_python_glyph_code outp)
        (enum_resolve_glyphs param);

      output_string outp "\n";
      output_string outp "my_font.encoding = 'UnicodeBMP'\n";
      output_string outp "\n";
      if Option.is_some (Opt.opt sfd_opt) then
        Print.printf p"my_font.save('%s')\n" (Option.get (Opt.opt sfd_opt));
      if Option.is_some (Opt.opt font_opt) then
        if Option.is_some (Opt.opt font_flags_opt) then
          Print.printf p"my_font.generate('%s', flags=[%s])\n"
            (Option.get (Opt.opt font_opt))
            (Option.get (Opt.opt font_flags_opt))
        else
          Print.printf p"my_font.generate('%s', flags=['opentype',])\n"
            (Option.get (Opt.opt font_opt))

    | Some glyph_name ->
      if have_glyph glyph_name then
        let glyph = get_resolve_glyph glyph_name param in
        Cubic_glyph.print_python_glyph_update_module outp glyph

(*-----------------------------------------------------------------------*)
