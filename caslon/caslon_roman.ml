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
    e_crossbar_height : float;
    ascender_height : float;
    lc_stem_width : float;
    lc_serif_height : float;

    corner_radius : Random.State.t -> float;
    serif_end_angle : Random.State.t -> float;
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

(*
let round_together points =
  let rec find_min_distance point_list current_min current_index k =
    match point_list with
      | [] -> current_index
      | pt :: remaining ->
        let d = Complex_point.(norm (round pt - pt)) in
        if d < current_min then
          find_min_distance remaining d k (k + 1)
        else
          find_min_distance remaining current_min current_index (k + 1)
  in
  let min_distance_index = find_min_distance points infinity (-1) 0 in
  if min_distance_index < 0 then
    failwith "round_together";
  let min_distance_pt = List.at points min_distance_index in
  let vector = Complex_point.(round min_distance_pt - min_distance_pt) in
  List.map (fun pt -> Complex_point.(round (pt + vector))) points
*)

let make_end_of_left_serif height bottom_corner_radius top_corner_radius shear_angle =
  let flat_height = height -. bottom_corner_radius -. top_corner_radius in
  let shear_vector = floor ((flat_height -. 1.) *. dtan shear_angle +. 0.5) in
  Complex_point.(Metacubic.(
    let point1 = Complex_point.(y' bottom_corner_radius) in
    let point2 = Complex_point.(point1 + x' shear_vector + y' flat_height) in
    let metacubic =
      if Float.(re point2 = re point1) then
        (left (x' bottom_corner_radius)
         <@-> up point1
         <@-.> huge <.-@> up point2
         <@-> right (x' top_corner_radius + y' height))
      else
        let bottom_ratio = x'(bottom_corner_radius /. flat_height) in
        let top_ratio = x'(top_corner_radius /. flat_height) in
        let bottom_corner_vector = bottom_ratio * (point1 - point2) in
        let top_corner_vector = top_ratio * (point2 - point1) in
        let bottom_corner = point1 + bottom_corner_vector in
        let top_corner = point2 + top_corner_vector in
        let start_point = round (bottom_corner + x' bottom_corner_radius) in
        let end_point = round (top_corner + x' top_corner_radius) in
        if Float.(re point1 < re point2) then
          (left start_point
           <@-> point ~in_dir:i ~out_control:(point1 + i) point1
           <@> point ~in_control:(point1 + i) ~out_dir:(point2 - point1 - i) point2
           <@-> right end_point)
        else
          (left start_point
           <@-> point ~in_dir:(point2 - i - point1) ~out_control:(point2 - i) point1
           <@> point ~in_control:(point2 - i) ~out_dir:i point2
           <@-> right end_point)
    in
    let cubic = to_cubic metacubic in
    let offset = Float.(floor (0.5 *. (re point1 -. re point2) +. 0.5)) in
    Cubic.(cubic <+> x' offset <.> round)
  ))

let make_end_of_right_serif height bottom_corner_radius top_corner_radius shear_angle =
  let left_serif =
    make_end_of_left_serif height top_corner_radius bottom_corner_radius (-.shear_angle)
  in
  Cubic.(Complex_point.(left_serif <*> rot 180. <+> y' height))

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
            <@-> left (x'pos 0.56 + y'pos 0.00)          (* bottom *)
            <@-.> 0.95 <.-@> up (x'pos 0.0 + y'pos 0.50) (* left *)
            <@-> right (x'pos 0.62 + y'pos 1.00)         (* top *)
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
            <@-.> 0.75 <.-@> left top   (* inner top *)
            <@-> down (x'pos 0.00 + x' left_breadth + y'pos 0.52) (* inner left *)
            <@-> right (x'pos 0.64 + y'pos 0.00 + y' bottom_breadth) (* inner bottom *)
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
      let c = c1 <@> c2 <@> c3 <-@@ 1. in
      c <.> round
    in
    [contour]
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
          up (crossbar_top0 + y' crossbar_fillet_size)
          <@-> left (x'pos 0.50 + y'pos 1.00 - y' top_breadth) (* eye top *)
          <@-> down (x'pos 0.00 + x' left_breadth + y'pos 0.52) (* inner left *)
          <@-> right (x'pos 0.60 + y'pos 0.00 + y' bottom_breadth) (* inner bottom *)
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
            <@-> left (x'pos 0.54 + y'pos 0.00) (* bottom *)
            <@-.> 0.9 <.-@> up (x'pos 0.00 + y'pos 0.51) (* left *)
            <@-> right (x'pos 0.52 + y'pos 1.00)         (* top *)
            <@-> down (x'pos 0.95 + y' crossbar_height + y'rel 0.02) (* right *)
            <@-> left (x'pos 0.85 + y' crossbar_height) (* crossbar right *)
            <@-.> huge <.-@> left (crossbar_height_point + x' crossbar_fillet_size) (* crossbar left *)
            <@-> set_dirs (of_cubic lower) (* inner bowl *)
            |> close ~tension:2.
          )
        ) <.> round
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
            right (crossbar_top1 + x' crossbar_fillet_size) (* crossbar top left *)
            <@-.> huge <.-@> right (crossbar_top0 - x' crossbar_fillet_size) (* crossbar top right *)
            <@-> set_dirs ~guess:false (of_cubic eye_upper) |> close
          )
        ) <.> round
    in
    [main_contour; eye_contour]
  )))
;;

(*.......................................................................*)

(* The letter "l" *)

let letter_l_contours glyph_name p =

  let tools =
    make_tools
      ~glyph_name
      ~param:p
      ()
  in
  let module Tools = (val tools : Tools_module) in

  Tools.(Cubic.(Complex_point.(

    let stem_width = p.lc_stem_width +. 4. in
    let serif_height = p.lc_serif_height in
    let bracket_width = p.lc_serif_height in
    let left_pos = (-0.5) *. stem_width in
    let right_pos = 0.5 *. stem_width in
    let serif_to_top = p.ascender_height -. serif_height in
    let left_serif_width = 105. in
    let right_serif_width = 85. in
    
    let rand_init =
      int_of_float p.design_size :: p.os2_weight ::
        List.map Char.code (String.explode "l")
    in
    let rand = Random.State.make (Array.of_list rand_init) in
    let rand_angle () = p.serif_end_angle rand in
    let rand_radius () = p.corner_radius rand in
    let left_serif_end =
      Cubic.(make_end_of_left_serif (serif_height +. 3.)
               (rand_radius ()) (rand_radius ()) (rand_angle ())
             <+> x' left_pos - x' left_serif_width - y' 2.)
                                  |> Metacubic.of_cubic |> Metacubic.set_dirs
    in
    let right_serif_end =
      Cubic.(make_end_of_right_serif (serif_height +. 3.)
               (rand_radius ()) (rand_radius ()) (rand_angle ())
             <+> x' right_pos + x' right_serif_width - y' 2.)
                                  |> Metacubic.of_cubic |> Metacubic.set_dirs
    in
    let left_side =
      Metacubic.(
        left zero
        <@-> left_serif_end
        <@-> right (x' left_pos + y' serif_height - x' 20.)
        <@-> up (x' left_pos + y' serif_height + y' 45.)
        <@> point (x' left_pos + y' serif_height + (x_shear (y' serif_to_top) (-0.5)))
      )
    in
    let right_side =
      Metacubic.(
        point (x' right_pos + y' serif_height + (x_shear (y' serif_to_top) 1.0))
        <@-> down (x' right_pos + y' serif_height + y' 45.)
        <@-> right (x' right_pos + y' serif_height + x' 20.)
        <@-> right_serif_end
        <@-> left zero
      )
    in
    let contour =
      Metacubic.to_cubic right_side <@> Metacubic.to_cubic left_side |> close <.> round
    in
    let (lower_left, _) = bounds contour in
    [contour <-> x' (re lower_left)]
  )))
;;

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
      <-@@ 0.95 <.> round
    in
    let inner_contour =
      make_down_node (x'pos 0.00 + x' left_breadth + y'pos 0.52) (* left *)
      <@-.> 0.98 <.-@> make_right_node (x'pos 0.49 + y'pos 0.00 + y' bottom_breadth) (* bottom *)
      <@-> make_up_node (x'pos 1.00 - x' right_breadth + y'pos 0.48) (* right *)
      <@-> make_left_node (x'pos 0.48 + y'pos 1.00 - y' top_breadth) (* top *)
      <-@@ 1.0 <.> round
    in
    [outer_contour; inner_contour]
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
