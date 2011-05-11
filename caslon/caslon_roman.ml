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
    tail_end_angle : Random.State.t -> float;
    left_bracket : Random.State.t -> bracket;
    right_bracket : Random.State.t -> bracket;
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

type bend_kind = [ `With_extrema | `With_points_at_extrema | `Without_extrema ]

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

let bend_with_possible_extrema ~point1 ~point2 ~in_dir ~out_dir =
  if Complex.(point2 - point1 = zero) then
    Metacubic.point ~in_dir ~out_dir point1
  else
    Metacubic.(point ~dir:in_dir point1 <@-> point ~dir:out_dir point2)

let bend_with_points_at_extrema ~point1 ~point2 ~in_dir ~out_dir =
  if Complex.(point2 - point1 = zero) then
    Metacubic.point ~in_dir ~out_dir point1
  else
    (* FIXME: Make the following leave out inflection points. *)
    let bend = Metacubic.(point ~dir:in_dir point1 <@-> point ~dir:out_dir point2) in
    let cubic = Metacubic.to_cubic bend in
    let (x_times, y_times) = Cubic.curve_extrema_and_inflections cubic in
    let times = Array.append x_times y_times in
    let times_count = Array.length times in
    if times_count = 0 then
      bend
    else
      begin
        Array.sort Float.compare times;
        let cubic' =
          fold
            Cubic.(fun result k -> result <@> portion cubic times.(k) times.(k + 1))
            (Cubic.portion cubic 0. times.(0))
            (1 --^ (times_count - 1))
        in
        Cubic.(cubic' <@> Cubic.portion cubic times.(times_count - 1) 1.) |>
            Metacubic.of_cubic |> Metacubic.set_dirs
      end

let contour_bend ~kind ~point1 ~point2 ~in_dir ~out_dir =
  match kind with
    | `With_extrema -> bend_with_possible_extrema ~point1 ~point2 ~in_dir ~out_dir
    | `With_points_at_extrema -> bend_with_points_at_extrema ~point1 ~point2 ~in_dir ~out_dir
    | `Without_extrema -> extremum_free_bend ~point1 ~point2 ~in_dir ~out_dir

let make_extremum_free_bend ~corner_point ~radius ~tension ~in_dir ~out_dir =
  let (point1, point2) = bend_points ~corner_point ~radius ~in_dir ~out_dir in
  let bend = extremum_free_bend ~point1 ~point2 ~in_dir ~out_dir in
  bend

let make_bend_with_possible_extrema ~corner_point ~radius ~tension ~in_dir ~out_dir =
  let (point1, point2) = bend_points ~corner_point ~radius ~in_dir ~out_dir in
  let bend = bend_with_possible_extrema ~point1 ~point2 ~in_dir ~out_dir in
  bend

let make_bend_with_points_at_extrema ~corner_point ~radius ~tension ~in_dir ~out_dir =
  let (point1, point2) = bend_points ~corner_point ~radius ~in_dir ~out_dir in
  let bend = bend_with_points_at_extrema ~point1 ~point2 ~in_dir ~out_dir in
  bend

let make_contour_bend ~kind ~corner_point ~radius ~tension ~in_dir ~out_dir =
  match kind with
    | `With_extrema ->
      make_bend_with_possible_extrema ~corner_point ~radius ~tension ~in_dir ~out_dir
    | `With_points_at_extrema ->
      make_bend_with_points_at_extrema ~corner_point ~radius ~tension ~in_dir ~out_dir
    | `Without_extrema ->
      make_extremum_free_bend ~corner_point ~radius ~tension ~in_dir ~out_dir

let flat_cut_points ~corner1 ~corner2 ~radius1 ~radius2 ~in_dir ~out_dir =
  let cut_dir = Complex_point.(dir (corner2 - corner1)) in
  let (point1, point2) = bend_points ~corner_point:corner1 ~radius:radius1 ~in_dir ~out_dir:cut_dir in
  let (point3, point4) = bend_points ~corner_point:corner2 ~radius:radius2 ~in_dir:cut_dir ~out_dir in
  (point1, point2, point3, point4, cut_dir)

let flat_cut
    ~point1 ~point2 ~point3 ~point4
    ~tension ~in_dir ~cut_dir ~out_dir
    ~bend_kind1 ~bend_kind2 =
  if Complex_point.(inner (point3 - point2) cut_dir) <= 0. then
    let midpoint = Complex_point.(x' 0.5 * (point2 + point3)) in
    let bend1 = contour_bend ~kind:bend_kind1 ~point1 ~point2:midpoint ~in_dir ~out_dir:cut_dir in
    let bend2 = contour_bend ~kind:bend_kind2 ~point1:midpoint ~point2:point4 ~in_dir:cut_dir ~out_dir in
    Metacubic.(bend1 <@> bend2)
  else
    let bend1 = contour_bend ~kind:bend_kind1 ~point1 ~point2 ~in_dir ~out_dir:cut_dir in
    let bend2 = contour_bend ~kind:bend_kind2 ~point1:point3 ~point2:point4 ~in_dir:cut_dir ~out_dir in
    Metacubic.(bend1 <@-.> tension <.-@> bend2)

let make_flat_cut
    ~corner1 ~corner2
    ~radius1 ~radius2 ~tension
    ~in_dir ~out_dir
    ~bend_kind1 ~bend_kind2 =
  let (point1, point2, point3, point4, cut_dir) =
    flat_cut_points ~corner1 ~corner2 ~radius1 ~radius2 ~in_dir ~out_dir
  in
  flat_cut
    ~point1 ~point2 ~point3 ~point4
    ~tension ~in_dir ~cut_dir ~out_dir
    ~bend_kind1 ~bend_kind2

let make_flat_cut_for_contours
    ~contour1 ~contour2
    ~corner_time1 ~corner_time2
    ~radius1 ~radius2 ~tension
    ~bend_kind1 ~bend_kind2 =
  let corner1 = Cubic.point_at contour1 corner_time1 in
  let corner2 = Cubic.point_at contour2 corner_time2 in
  let (in_dir,_) = Cubic.tangents_at contour1 corner_time1 in
  let (_, out_dir) = Cubic.tangents_at contour2 corner_time2 in
  let (point1, point2, point3, point4, cut_dir) =
    flat_cut_points ~corner1 ~corner2 ~radius1 ~radius2 ~in_dir ~out_dir
  in
  let time1 = Cubic.time_at_nearest_point contour1 point1 in
  let time2 = Cubic.time_at_nearest_point contour2 point4 in
  let point1 = Cubic.point_at contour1 time1 in
  let point4 = Cubic.point_at contour2 time2 in
  let (in_dir,_) = Cubic.tangents_at contour1 time1 in
  let (_, out_dir) = Cubic.tangents_at contour2 time2 in
  (flat_cut
     ~point1 ~point2 ~point3 ~point4
     ~tension ~in_dir ~cut_dir ~out_dir
     ~bend_kind1 ~bend_kind2,
   time1, time2)

let make_end_of_left_serif ~height ~shear_angle ~bottom_radius ~top_radius ~tension =
  let shear_offset = 0.5 *. height *. dtan shear_angle in
  let corner1 = Complex_point.(x'(-.shear_offset)) in
  let corner2 = Complex_point.(x' shear_offset + y' height) in
  make_flat_cut ~corner1 ~corner2 ~radius1:bottom_radius ~radius2:top_radius ~tension
    ~in_dir:Complex_point.leftward ~out_dir:Complex_point.rightward
    ~bend_kind1:`Without_extrema ~bend_kind2:`Without_extrema

let make_end_of_right_serif ~height ~shear_angle ~bottom_radius ~top_radius ~tension =
  let shear_offset = 0.5 *. height *. dtan shear_angle in
  let corner1 = Complex_point.(x' shear_offset + y' height) in
  let corner2 = Complex_point.(x'(-.shear_offset)) in
  make_flat_cut ~corner1 ~corner2 ~radius1:top_radius ~radius2:bottom_radius ~tension
    ~in_dir:Complex_point.rightward ~out_dir:Complex_point.leftward
    ~bend_kind1:`Without_extrema ~bend_kind2:`Without_extrema

let make_flag
    ~top_corner ~right_point
    ~left_notch ~left_point
    ~flag_corner
    ~top_corner_radius ~flag_corner_radius
    ~top_cupping =
  Complex_point.(
    let lower_dir = dir (flag_corner - left_notch) in
    let upper_dir = dir (top_corner - flag_corner) in
    let notch_point = left_notch + x' 20. * lower_dir in
    let cupping_vector = x' top_cupping * upper_dir * rot (-.90.) in
    let flag_bend =
      make_bend_with_points_at_extrema
        ~corner_point:flag_corner ~radius:flag_corner_radius
        ~tension:1. ~in_dir:lower_dir ~out_dir:upper_dir
    in
    let top_bend =
      make_extremum_free_bend
        ~corner_point:top_corner ~radius:top_corner_radius
        ~tension:1. ~in_dir:upper_dir ~out_dir:downward
    in
    Metacubic.(
      point left_point
      <@> point notch_point
      <@~.> 1.5 <.~@> flag_bend
      <@--.> (huge,0.75) <.--@> point (x' 0.40 * flag_corner + x' 0.60 * top_corner + cupping_vector)
      <@--.> (0.75,huge) <.--@> top_bend
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
  
  Tools.(Complex_point.(

    let contour =

      let tail_end_angle = p.tail_end_angle rand in
      let tail_corner_radius1 = p.corner_radius rand +. 1. in
      let tail_corner_radius2 = p.corner_radius rand +. 1. in

      let left_breadth = 1.12 *. p.lc_stem_width in
      let bottom_breadth = 0.92 *. p.lc_stem_width /. (1. +. p.contrast) in
      let top_breadth = 0.64 *. p.lc_stem_width /. (1. +. p.contrast) in
      let head_breadth = 1.15 *. p.lc_stem_width /. (1. +. 0.3 *. p.contrast) in
      let tail_breadth = 0.25 *. p.lc_stem_width in

      let tail1 = x'pos 0.98 + y'pos 0.19 in
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

      let sketch_inner tail2 =
        Metacubic.(
          point head2
          <@-.> 1000. <.-@> point partway_pt
          <@-.> 0.75 <.-@> point ~dir:leftward top   (* inner top *)
          <@-> point ~dir:downward (x'pos 0.00 + x' left_breadth + y'pos 0.52) (* inner left *)
          <@-> point ~dir:rightward (x'pos 0.64 + y'pos 0.00 + y' bottom_breadth) (* inner bottom *)
          <@-> point ~in_curl:1.5 tail2 (* tail *)
                      |> to_cubic
        )
      in

      (* Roughly locate the sketched counter. *)
      let (_, tangent) = Cubic.tangents_at (Metacubic.to_cubic outer) 0. in
      let tail2 = tail1 + x' tail_breadth * tangent * rot (-100.) in
      let inner = sketch_inner tail2 in

      (* Now use the rough sketch to get a better estimate of the
         crosscut vector. *)
      let (tangent2, _) = Cubic.tangents_at inner (float_of_int Int.(List.length inner - 1)) in
      let crosscut_dir = dir (tangent - tangent2) * rot (-90.) in
      let tail2' = tail1 + x' tail_breadth * crosscut_dir in
      let shear_vector = x'(tail_breadth *. dtan tail_end_angle) * crosscut_dir * rot (-90.) in
      let tail2 = tail2' + shear_vector in
      let inner = sketch_inner tail2 in

      let outer' = Metacubic.to_cubic outer in
      let c = Cubic.(outer' <@-.> 1.2 <.-@> inner) in
      let (x_times, y_times) =
        Cubic.curve_extrema_and_inflections ~pos:Int.(Vect.length outer - 1) c
      in
      let y_time = y_times.(0) in
      let x_time = x_times.(0) /. y_time in
      let (c2,c3) = Cubic.subdivide c (float_of_int Int.(Vect.length outer - 1) +. y_time) in
      let (c1,c2) = Cubic.subdivide c2 (float_of_int Int.(Vect.length outer - 1) +. x_time) in
      let (tail_cut, inner_time, outer_time) =
        make_flat_cut_for_contours
          ~contour1:c3 ~contour2:c1
          ~corner_time1:(float_of_int Int.(List.length c3 - 1))
          ~corner_time2:0.
          ~radius1:tail_corner_radius1 ~radius2:tail_corner_radius2 ~tension:huge
          ~bend_kind1:`With_extrema ~bend_kind2:`With_extrema
      in
      let (_,c1) = Cubic.subdivide c1 outer_time in
      let (c3,_) = Cubic.subdivide c3 inner_time in
      Cubic.(c1 <@> c2 <@> c3 <@> Metacubic.to_cubic tail_cut)
    in
    [contour |> Cubic.round]
  ))
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

  Tools.(Complex_point.(

    let tail_end_angle = p.tail_end_angle rand in
    let tail_corner_radius1 = p.corner_radius rand +. 1. in
    let tail_corner_radius2 = p.corner_radius rand +. 1. in
    let crossbar_fillet_variation1 = Random.State.float rand 0.2 +. 0.9 in
    let crossbar_fillet_variation2 = Random.State.float rand 0.2 +. 0.9 in
    let crossbar_fillet_variation3 = Random.State.float rand 0.2 +. 0.9 in
    let crossbar_fillet_variation4 = Random.State.float rand 0.2 +. 0.9 in
    let crossbar_fillet_variation5 = Random.State.float rand 0.2 +. 0.9 in
    let crossbar_fillet_variation6 = Random.State.float rand 0.2 +. 0.9 in

    let top_breadth = 0.41 *. p.lc_stem_width /. (1. +. p.contrast) in
    let left_breadth = 1.01 *. p.lc_stem_width in
    let right_breadth = 1.68 *. p.lc_stem_width /. (1. +. 0.5 *. p.contrast) in
    let bottom_breadth = 1.00 *. p.lc_stem_width /. (1. +. p.contrast) in
    let tail_breadth = 0.34 *. p.lc_stem_width /. (1. +. p.contrast) in

    let crossbar_breadth = 0.42 *. p.lc_stem_width /. (1. +. p.contrast) in
    let crossbar_height = p.e_crossbar_height in
    let crossbar_top = p.e_crossbar_height +. crossbar_breadth in
    let crossbar_fillet_size1 = crossbar_breadth *. crossbar_fillet_variation1 in
    let crossbar_fillet_size2 = crossbar_breadth *. crossbar_fillet_variation2 in
    let crossbar_fillet_size3 = crossbar_breadth *. crossbar_fillet_variation3 in
    let crossbar_fillet_size4 = crossbar_breadth *. crossbar_fillet_variation4 in
    let crossbar_fillet_size5 = crossbar_breadth *. crossbar_fillet_variation5 in
    let crossbar_fillet_size6 = crossbar_breadth *. crossbar_fillet_variation6 in
    let crossbar_top0 = x'pos 1.00 - x' right_breadth + y' crossbar_top in

    let tail1 = x'pos 1.00 + y'pos 0.26 in

    let outer_left =
      Metacubic.(
        point ~out_curl:1.0 tail1       (* outer tail *)
        <@-> point ~dir:leftward (x'pos 0.54 + y'pos 0.00) (* bottom *)
        <@-.> 0.9 <.-@> point ~dir:upward (x'pos 0.00 + y'pos 0.51) (* left *)
        <@-> point ~dir:rightward (x'pos 0.52 + y'pos 1.00) (* top *)
      )
    in

    let sketch_counter tail2 =
      Metacubic.(
        point ~dir:upward (crossbar_top0 + y' crossbar_fillet_size1)
        <@-> point ~dir:leftward (x'pos 0.50 + y'pos 1.00 - y' top_breadth) (* eye top *)
        <@-> point ~dir:downward (x'pos 0.00 + x' left_breadth + y'pos 0.52) (* inner left *)
        <@-> point ~dir:rightward (x'pos 0.60 + y'pos 0.00 + y' bottom_breadth) (* inner bottom *)
        <@-> point ~in_curl:1.0 tail2 (* inner tail *)
                    |> to_cubic
      )
    in

    (* Roughly locate the sketched counter. *)
    let (_, tangent) = Cubic.tangents_at (Metacubic.to_cubic outer_left) 0. in
    let tail2 = tail1 + x' tail_breadth * tangent * rot (-95.) in
    let counter = sketch_counter tail2 in

    (* Now use the rough sketch to get a better estimate of the
       crosscut vector. *)
    let (tangent2, _) = Cubic.tangents_at counter (float_of_int Int.(List.length counter - 1)) in
    let crosscut_dir = dir (tangent - tangent2) * rot (-90.) in
    let tail2' = tail1 + x' tail_breadth * crosscut_dir in
    let shear_vector = x'(tail_breadth *. dtan tail_end_angle) * crosscut_dir * rot (-90.) in
    let tail2 = tail2' + shear_vector in
    let counter = sketch_counter tail2 in

    let crossbar_height_point =
      let time = (Cubic.curve_times_at_y counter ~pos:1 crossbar_height).(0) in
      Cubic.curve_point_at counter ~pos:1 time
    in
    let (upper, lower) =
      let intersection_time =
        (Cubic.curve_times_at_y ~pos:1 counter (crossbar_height -. crossbar_fillet_size2)).(0) +. 1.
      in
      Cubic.subdivide counter intersection_time
    in
    let outer =
      Metacubic.(
        outer_left
        <@-> point ~dir:downward (x'pos 0.95 + y' crossbar_height + y'rel 0.02) (* right *)
        <@-> point ~dir:leftward (x'pos 0.85 + y' crossbar_height) (* crossbar right *)
        <@-.> huge <.-@> point ~dir:leftward (crossbar_height_point + x' crossbar_fillet_size3) (* crossbar left *)
                    |> to_cubic
      )
    in
    let (tail_cut, lower_time, outer_time) =
      make_flat_cut_for_contours
        ~contour1:lower ~contour2:outer
        ~corner_time1:(float_of_int Int.(List.length lower - 1))
        ~corner_time2:0.
        ~radius1:tail_corner_radius1 ~radius2:tail_corner_radius2 ~tension:huge
        ~bend_kind1:`With_extrema ~bend_kind2:`With_extrema
    in
    let lower' = fst (Cubic.subdivide lower lower_time) |> Metacubic.of_cubic |> Metacubic.set_dirs in
    let outer' = snd (Cubic.subdivide outer outer_time) |>
        Metacubic.of_cubic |> Metacubic.set_dirs ~out_dir:leftward
    in
    let main_contour = Metacubic.(lower' <@> tail_cut <@> outer' |> close ~tension:(-1.) |> to_cubic) in
    let crossbar_top1 =
      let time = (Cubic.curve_times_at_y upper ~pos:1 crossbar_top).(0) in
      Cubic.curve_point_at upper ~pos:1 time
    in
    let eye_upper =
      let left_intersection_time =
        (Cubic.curve_times_at_y upper ~pos:1 (crossbar_top +. crossbar_fillet_size4)).(0) +. 1.
      in
      let (upper', _) = Cubic.subdivide upper left_intersection_time in
      upper'
    in
    let eye_contour =
      Metacubic.(
        to_cubic (
          point ~dir:rightward (crossbar_top1 + x' crossbar_fillet_size5) (* crossbar top left *)
          <@-.> huge <.-@> point ~dir:rightward (crossbar_top0 - x' crossbar_fillet_size6) (* crossbar top right *)
          <@-> set_dirs ~guess:false (of_cubic eye_upper) |> close
        )
      )
    in
    [main_contour |> Cubic.round;
     eye_contour |> Cubic.round]
  ))
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

  Tools.(Metacubic.(Complex_point.(

    let left_serif_end_angle = p.serif_end_angle rand in
    let right_serif_end_angle = p.serif_end_angle rand in
    let left_serif_bottom_radius = p.corner_radius rand in
    let left_serif_top_radius = p.corner_radius rand in
    let right_serif_bottom_radius = p.corner_radius rand in
    let right_serif_top_radius = p.corner_radius rand in
    let top_corner_radius = p.flag_top_corner_radius rand in
    let flag_corner_radius = p.flag_corner_radius rand in
    let leftbrack = left_bracket rand in
    let rightbrack = right_bracket rand in
    
    let stem_width = p.lc_stem_width +. extra_stem_width in
    let serif_height = p.lc_serif_height in
    let left_pos = (-0.5) *. stem_width in
    let right_pos = 0.5 *. stem_width in
    let overshot_height = height +. overshoot in
    let serif_to_top = overshot_height -. serif_height in

    let left_serif_end =
      make_end_of_left_serif
        ~height:(serif_height +. 4.)
        ~shear_angle:left_serif_end_angle
        ~bottom_radius:left_serif_bottom_radius
        ~top_radius:left_serif_top_radius
        ~tension:huge |>
            (Metacubic.translate (x' left_pos - x' left_serif_width - y' 3.))
    in
    let right_serif_end =
      make_end_of_right_serif
        ~height:(serif_height +. 4.)
        ~shear_angle:right_serif_end_angle
        ~bottom_radius:right_serif_bottom_radius
        ~top_radius:right_serif_top_radius
        ~tension:huge |>
            (Metacubic.translate (x' right_pos + x' right_serif_width - y' 3.))
    in

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
    let (_,flag_end,_) = Vect.at flag Int.(Vect.length flag - 1) in
    let right_side =
      Metacubic.(
        point ~in_dir:downward ~out_control:(right_point - y' 20.) flag_end
        <@> point ~in_control:(right_stem_point + y'(0.7 *. serif_to_top)) ~out_dir:downward right_stem_point
        <@--.>
          (rightbrack.bracket_vert_tension,
           rightbrack.bracket_horiz_tension)
        <.--@> point ~dir:rightward (x' right_pos + y' serif_height + x' rightbrack.bracket_horiz)
        <@--.> (1.,2.) <.--@> right_serif_end
        <@--.> (2.,1.) <.--@> point ~dir:leftward zero
      )
    in
    let contour = to_cubic (flag <@> right_side <@> left_side) in
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
