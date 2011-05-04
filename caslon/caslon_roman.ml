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

    contrast : float;                   (* 1.0 = "normal" contrast. *)
    extension : float;                  (* 1.0 = normal. *)
    design_size : float;
    space_width : float;
    x_height : float;
    curve_overshoot : float;
    curve_undershoot : float;
    e_crossbar_height : float;
    lc_stem_width : float;
  }

  module type Tools_module =
  sig
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
      ~width
      ~height
      ?(left_overlap = 0.)
      ?(overshoot = 0.)
      ?(undershoot = 0.)
      ~param
      () =

    Complex_point.(
      let p = param in
      let extended_width = width *. p.extension in
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

(* The letter "c" *)

let letter_c_contours p =

  let tools =
    let overshoot = p.curve_overshoot +. 3. in
    let undershoot = p.curve_undershoot +. 3. in
    make_tools
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
      let bottom_breadth = 0.92 *. p.lc_stem_width /. p.contrast in
      let top_breadth = 0.64 *. p.lc_stem_width /. p.contrast in
      let head_breadth = 1.15 *. p.lc_stem_width in
      let tail_breadth = 0.24 *. p.lc_stem_width in

      let tail_cut_angle = -37. in
      let tail1 = x'pos 0.99 + y'pos 0.20 in
      let tail2 = tail1 - y_shear (x' tail_breadth) tail_cut_angle in

      let head1 = x'pos 0.97 + y'pos 0.86 in

      let outer =
        Metacubic.(
          set_dirs (
            Vect.of_list [
              knot ~out_curl:1.5 tail1;   (* tail *)
              left_knot ~in_tension:1.1 ~out_tension:0.93 (x'pos 0.56 + y'pos 0.00); (* bottom *)
              up_knot ~in_tension:0.93 (x'pos 0.0 + y'pos 0.49); (* left *)
              right_knot ~out_tension:1.(* 1  *) (x'pos 0.62 + y'pos 1.00); (* top *)
              knot ~in_curl:2. ~in_tension:1. (* 1 *) head1; (* head *)
            ]
          )   
        )
      in
      let across_head_dir = Metacubic.outgoing_dir outer / rot 90. in
      let head2 = head1 + (x' head_breadth) * across_head_dir in
      let inner =
        Metacubic.(
          set_dirs (
            Vect.of_list [
              knot ~dir:(across_head_dir / rot 85.) head2; (* head *)
              left_knot (x'pos 0.56 + y'pos 1.00 - y' top_breadth); (* inner top *)
              down_knot (x'pos 0.00 + x' left_breadth + y'pos 0.52); (* inner left *)
              right_knot (x'pos 0.64 + y'pos 0.00 + y' bottom_breadth); (* inner bottom *)
              knot ~in_curl:1.5 tail2;    (* tail *)
            ]
          )
        )
      in
      let c = Metacubic.join outer inner in
      let c = Metacubic.close ~tension:2.0 c in
      Metacubic.to_cubic c <.> round
    in
    [contour]
  )))
;;

(*.......................................................................*)

(* The letter "e" *)

let letter_e_contours p =

  let tools =
    let overshoot = p.curve_overshoot +. 2. in
    let undershoot = p.curve_undershoot +. 2. in
    make_tools
      ~width:354.
      ~height:(p.x_height +. undershoot +. overshoot)
      ~undershoot:p.curve_undershoot
      ~overshoot:p.curve_overshoot
      ~param:p
      ()
  in
  let module Tools = (val tools : Tools_module) in

  Tools.(Cubic.(Complex_point.(

    let top_breadth = 0.41 *. p.lc_stem_width /. p.contrast in
    let left_breadth = 1.01 *. p.lc_stem_width in
    let right_breadth = 1.68 *. p.lc_stem_width in
    let bottom_breadth = 1.00 *. p.lc_stem_width /. p.contrast in
    let tail_breadth = 0.37 *. p.lc_stem_width /. p.contrast in

    let crossbar_breadth = 0.42 *. p.lc_stem_width /. p.contrast in
    let crossbar_height = p.e_crossbar_height in
    let crossbar_top = p.e_crossbar_height +. crossbar_breadth in
    let crossbar_fillet_size = crossbar_breadth in
    let crossbar_top0 = x'pos 1.00 - x' right_breadth + y' crossbar_top in

    let tail_cut_angle = 145. in
    let tail1 = x'pos 1.00 + y'pos 0.25 in
    let tail2 = tail1 + x' tail_breadth * rot tail_cut_angle in
    let tail1_angle = 65. in
    let tail2_angle = 52. in

    let counter =
      make_up_node (crossbar_top0 + y' crossbar_fillet_size)
      <@-> make_left_node (x'pos 0.50 + y'pos 1.00 - y' top_breadth) (* eye top *)
      <@-.> 1.05 <.-@> make_down_node (x'pos 0.00 + x' left_breadth + y'pos 0.52) (* inner left *)
      <@-> make_right_node (x'pos 0.60 + y'pos 0.00 + y' bottom_breadth) (* inner bottom *)
      <@-> make_dir_node (rot tail2_angle) tail2 (* inner tail *)
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
      make_dir_node (neg (rot tail1_angle)) tail1   (* outer tail *)
      <@-> make_left_node (x'pos 0.54 + y'pos 0.00) (* bottom *)
      <@-.> 0.9 <.-@> make_up_node (x'pos 0.00 + y'pos 0.51) (* left *)
      <@-> make_right_node (x'pos 0.52 + y'pos 1.00)         (* top *)
      <@-> make_down_node (x'pos 0.95 + y' crossbar_height + y'rel 0.02) (* right *)
      <@-> make_left_node (x'pos 0.85 + y' crossbar_height) (* crossbar right *)
      <@-.> infinity <.-@>
           make_left_node (crossbar_height_point + x' crossbar_fillet_size) (* crossbar left *)
      <@-> lower                        (* inner bowl *)
      <-@@ 2.                           (* tail end *)
      <.> round
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
      make_right_node (crossbar_top1 + x' crossbar_fillet_size) (* crossbar top left *)
      <@-.> infinity <.-@>
        make_right_node (crossbar_top0 - x' crossbar_fillet_size) (* crossbar top right *)
      <@-> eye_upper
      <-@@ 1.                           (* Close with tension 1.0. *)
      <.> round
    in
    [main_contour; eye_contour]
  )))
;;

(*.......................................................................*)

(* The letter "o" *)

let letter_o_contours p =

  let tools =
    let overshoot = p.curve_overshoot in
    let undershoot = p.curve_undershoot in
    make_tools
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
    let bottom_breadth = 0.58 *. p.lc_stem_width /. p.contrast in
    let top_breadth = 0.54 *. p.lc_stem_width /. p.contrast in

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
      contours = letter_c_contours p;
      lsb = Some 0.;
      rsb = Some 50.;
  })
;;

add_glyph "e"
  Glyph.(fun p -> {
    empty with
      name = "e";
      contours = letter_e_contours p;
      lsb = Some 0.;
      rsb = Some 50.;
  })
;;

add_glyph "o"
  Glyph.(fun p -> {
    empty with
      name = "o";
      contours = letter_o_contours p;
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
