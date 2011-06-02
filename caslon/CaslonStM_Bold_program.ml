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

open Batteries
open Caslon_roman
open Fontdesign
open Param

let params ~glyph_name =

  let tools = make_tools ~glyph_name () in
  let module Tools = (val tools : Tools_module) in
  let open Tools in
      set_ps "version" "0.1";

      set_pi "os2_weight" 700;
      set_pf "design_size" 12.;
      initialize_hash "hash";

      set_ps "fontname" "CaslonStM-Bold";
      set_ps "familyname" "Sorts Mill Caslon";
      set_ps "fullname" "Sorts Mill Caslon Bold";
      set_ps "weight" "Regular";

      set_ps "family" "Sorts Mill Caslon";
      set_ps "subfamily" "Regular";
      set_ps_func "preferred_family" null_func;
      set_ps_func "preferred_subfamily" null_func;
      set_ps_func "wws_family" null_func;
      set_ps_func "wws_subfamily" null_func;

      set_pf "space_width" 200.;

      set_pf "contrast" 0.5;
      set_pf "extension" 0.1;

      set_pf "stem_width" 90.;
      set_pf "serif_height" 30.;

      set_pf_func "corner_radius" (fun () -> Some (float_hash "34612" 3.6 5.4));
      set_pf_func "serif_end_angle" (fun () -> Some (float_hash "63555" (-5.5) 5.5));
      set_pf_func "tail_end_angle" (fun () -> Some (float_hash "63421" 0. 10.));

      set_pf_func "flag_corner_angle" (fun () -> Some (float_hash "34643" (-5.) 5.));
      set_pf "flag_corner_cut_length" 15.;
      set_pf_func "flag_corner_corner_radius" (fun () -> Some (pf "corner_radius" +. 2.));
      set_pf "flag_top_radius" 8.;
      set_pf "flag_top_tension" 2.;
      set_pf "flag_cupping_angle1" 3.;
      set_pf "flag_cupping_angle2" 3.;

      set_pm_func "left_bracket"
        (fun () ->
          let left_pos =
            if Set.mem glyph_name i_letters then
              (-23.)
            else
              (-20.)
          in
          let horiz_tension = 1.5 in
          let vert_tension = 1.5 in
          Some (Metacubic.(
            Cpx.(point ~dir:rightward (x' left_pos))
            |> dput ~tensions:(horiz_tension, vert_tension) Cpx.(point ~dir:upward (y' 70.))
          )));

      set_pm_func "right_bracket"
        (fun () ->
          let left_pos = 20. in
          let horiz_tension = 1.5 in
          let vert_tension = 1.5 in
          Some (Metacubic.(
            Cpx.(point ~dir:downward (y' 70.))
            |> dput ~tensions:(vert_tension, horiz_tension) Cpx.(point ~dir:rightward (x' left_pos))
          )));

      if Set.mem glyph_name c_letters then (
        set_pf "width" 352.;
        set_pf "height" 425.;
        set_pf "bottom_overlap" 13.;
        set_pf "left_breadth" (1.12 *. pf "stem_width");
        set_pf "bottom_breadth" (0.92 *. pf "stem_width" /. (1. +. pf "contrast"));
        set_pf "top_breadth" (0.64 *. pf "stem_width" /. (1. +. pf "contrast"));
        set_pf "head_breadth" (1.15 *. pf "stem_width" /. (1. +. 0.3 *. pf "contrast"));
        set_pf "tail_breadth" (0.25 *. pf "stem_width");
        set_pc_func "tail1" (fun () -> Some Cpx.(x'pos 0.98 + y'pos 0.19));
        set_pc_func "head1" (fun () -> Some Cpx.(x'pos 0.98 + y'pos 0.84));
      );

      if Set.mem glyph_name e_letters then (
        set_pf "width" 389.;
        set_pf "height" 423.;
        set_pf "bottom_overlap" 10.;
        set_pf "crossbar_height" 258.;
        set_pf "top_breadth" (0.41 *. pf "stem_width" /. (1. +. pf "contrast"));
        set_pf "left_breadth" (1.01 *. pf "stem_width");
        set_pf "right_breadth" (1.68 *. pf "stem_width" /. (1. +. 0.5 *. pf "contrast"));
        set_pf "bottom_breadth" (1.00 *. pf "stem_width" /. (1. +. pf "contrast"));
        set_pf "tail_breadth" (0.34 *. pf "stem_width" /. (1. +. pf "contrast"));
        set_pf "crossbar_height" (pf "crossbar_height");
        set_pf "crossbar_breadth" (0.42 *. pf "stem_width" /. (1. +. pf "contrast"));
        set_pf_func "crossbar_fillet_size"
          (fun () -> Some (float_hash "31315" 0.7 0.9 *. pf "crossbar_breadth"));
        set_pc "tail1" Cpx.(x'pos 1.00 + y'pos 0.26);
      );

      if Set.mem glyph_name i_letters then (
        set_pf "height" 415.;
        set_pf "left_serif_width" 65.;
        set_pf "right_serif_width" 60.;
        set_pf "right_side_shear" 1.2;
        set_pf "flag_width" 70.;
        set_pf "flag_drop" 58.;
        set_pc "left_notch_drop" Cpx.(y'(-105.));
        set_pc "dot_point" Cpx.(x'(-7.) + y' 623.);
        set_pm "dot" (
          let radius = 61. in
          Metacubic.(
            Cpx.(point ~dir:upward (x'(-.radius)))
            |> put Cpx.(point ~dir:rightward (y' radius))
            |> put Cpx.(point ~dir:downward (x' radius))
            |> put Cpx.(point ~dir:leftward (y'(-.radius)))
            |> close
          )
        )
      );

      if Set.mem glyph_name l_letters then (
        set_pf "height" 707.;
        set_pf "stem_width" (pf "stem_width" +. 3.);
        set_pf "left_serif_width" 105.;
        set_pf "right_serif_width" 85.;
        set_pf "right_side_shear" 1.2;
        set_pf "flag_width" 70.;
        set_pf "flag_drop" 58.;
        set_pc "left_notch_drop" Cpx.(y'(-105.));
      );

      if Set.mem glyph_name o_letters then (
        set_pf "width" 421.;
        set_pf "height" 419.;
        set_pf "bottom_overlap" 10.;
        set_pf "left_breadth" (1.16 *. pf "stem_width");
        set_pf "right_breadth" (1.16 *. pf "stem_width");
        set_pf "bottom_breadth" (0.58 *. pf "stem_width" /. (1. +. pf "contrast"));
        set_pf "top_breadth" (0.54 *. pf "stem_width" /. (1. +. pf "contrast"));
      );

      if Set.mem glyph_name r_letters then (
        set_pf "height" 420.;
        set_pf "stem_width" (pf "stem_width" +. 2.);
        set_pf "left_serif_width" 65.;
        set_pf "right_serif_width" 67.;
        set_pf "right_side_shear" 0.2;
        set_pc "left_notch_drop" Cpx.(y'(-107.));
        set_pf "flag_width" 70.;
        set_pf "flag_drop" 70.;
        set_pm "shoulder"
          Metacubic.(
            Cpx.(point ~out_curl:0.1 (y' 309.))
            |> put Cpx.(point ~dir:rightward (x' 145. + y' 407.))
          );
        set_pm "arm_end"
          Metacubic.(
            Cpx.(point ~dir:downward (x' 212. + y' 350.))
          );
        set_pm_func "arm_lower"
          (fun () ->
            Some (Metacubic.(
              Cpx.(point ~dir:leftward (x' 164. + y' 301.))
              |> put Cpx.(point ~dir:leftward (x' 79. + y' 330.))
              |> put ~tension:1.15 Cpx.(point ~dir:downward (y' 255.))
            )));
      );

      if Set.mem glyph_name t_letters then (
        set_pf "width" 303.;
        set_pf "height" 562.;
        set_pf "bottom_overlap" 12.;
        set_pf "bottom_breadth" (0.70 *. pf "stem_width" /. (1. +. pf "contrast"));
        set_pf "crossbar_height" 402.;
        set_pf "crossbar_breadth" (floor (0.85 *. pf "stem_width" /. (1. +. 0.7 *. pf "contrast") +. 0.5));
        set_pf "sheared_terminal_width" 70.;
        set_pf "top_corner_height" 550.;
        set_pf "tail_breadth" (0.34 *. pf "stem_width" /. (1. +. pf "contrast"));
        set_pf "sheared_terminal_drop" (-3.);
        set_pf_func "tail_end_angle" (fun () -> Some (float_hash "25413" 10. 15.));
        set_pf "tail_cut_angle" 100.;
      );

      !p_ref

  in
  run_command params
