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

      set_pi "os2_weight" 400;
      set_pf "design_size" 12.;
      initialize_state "state";

      set_ps "fontname" "CaslonStM";
      set_ps "familyname" "Sorts Mill Caslon";
      set_ps "fullname" "Sorts Mill Caslon";
      set_ps "weight" "Regular";

      set_ps "family" "Sorts Mill Caslon";
      set_ps "subfamily" "Regular";
      set_ps_func "preferred_family" null_func;
      set_ps_func "preferred_subfamily" null_func;
      set_ps_func "wws_family" null_func;
      set_ps_func "wws_subfamily" null_func;

      set_pf "space_width" 200.;

      set_pf "contrast" 0.;
      set_pf "extension" 0.;

      set_pf "stem_width" 57.;
      set_pf "serif_height" 24.;

      set_pf_func "corner_radius" (fun () -> Some (float_of_int (Random.State.int (prs "state") 2 + 3)));
      set_pf_func "serif_end_angle" (fun () -> Some (float_of_int (Random.State.int (prs "state") 101 - 50) /. 9.));
      set_pf_func "tail_end_angle" (fun () -> Some (float_of_int (Random.State.int (prs "state") 101) /. 10.));

      set_pm_func "left_bracket"
        (fun () ->
          let left_pos = -.(Random.State.float (prs "state") 10. +. 15.) in
          let horiz_tension = 1. +. Random.State.float (prs "state") 0.5 in
          let vert_tension = 1. +. Random.State.float (prs "state") 0.5 in
          Some (Metacubic.(
            Cpx.(point ~dir:rightward (x' left_pos |> round))
            |> dput ~tensions:(horiz_tension, vert_tension) Cpx.(point ~dir:upward (y' 70. |> round))
          )));

      set_pm_func "right_bracket"
        (fun () ->
          let right_pos = Random.State.float (prs "state") 10. +. 15. in
          let horiz_tension = 1. +. Random.State.float (prs "state") 0.5 in
          let vert_tension = 1. +. Random.State.float (prs "state") 0.5 in
          Some (Metacubic.(
            Cpx.(point ~dir:downward (y' 70. |> round))
            |> dput ~tensions:(vert_tension, horiz_tension) Cpx.(point ~dir:rightward (x' right_pos |> round))
          )));

      if Set.mem glyph_name c_letters then (
        set_pf "width" 320.;
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
        set_pf "width" 354.;
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
          (fun () -> Some ((Random.State.float (prs "state") 0.2 +. 0.7) *. pf "crossbar_breadth"));
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
        set_pc "dot_point" Cpx.(x'(-10.) + y' 623.);
        set_pm "dot" (
          let radius = 54. in
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
        set_pf "width" 383.;
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
            Cpx.(point ~out_curl:0.1 (y' 310.))
            |> put Cpx.(point ~dir:rightward (x' 140. + y' 397.))
          );
        set_pm "arm_end"
          Metacubic.(
            Cpx.(point ~dir:downward (x' 207. + y' 350.))
          );
        set_pm_func "arm_lower"
          (fun () ->
            Some (Metacubic.(
              Cpx.(point ~dir:leftward (x' 164. + y' 306.))
              |> put Cpx.(point ~dir:leftward (x' 79. + y' 335.))
              |> put ~tension:1.15 Cpx.(point ~dir:downward (y' 260.))
            )));
      );

      if Set.mem glyph_name t_letters then (
        set_pf "width" 275.;
        set_pf "height" 562.;
        set_pf "bottom_overlap" 12.;
        set_pf "crossbar_height" 402.;
        set_pf "top_corner_height" 550.;
        set_pf_func "tail_end_angle" (fun () -> Some (float_of_int (Random.State.int (prs "state") 101) /. 10. +. 5.));
      );

      !p_ref
  in
  run_command params

(*
  contrast = 0.1;
  extension = -0.22;
  space_width = 200.;
  x_height = 350.;
  curve_overshoot = 10.;
  curve_undershoot = 10.;
  e_crossbar_height = 240.;
  lc_stem_width = 50.;
*)
