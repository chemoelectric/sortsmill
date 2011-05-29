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

  let tools = make_tools () in
  let module Tools = (val tools : Tools_module) in
  let open Tools in
      set_ps "version" "0.1";

      set_pi "os2_weight" 400;
      set_pf "design_size" 12.;

      let state =
        Random.State.make
          (Array.of_list
             (int_of_float (float !p_ref "design_size") ::
                int !p_ref "os2_weight" ::
                List.map Char.code (String.explode glyph_name)))
      in
      set_prs "state" state;

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

      (* ?????????????????????????????????????????????????????????????????????????????????????*)
      set_pf "contrast" 0.;
      set_pf "extension" 0.;
      (* ?????????????????????????????????????????????????????????????????????????????????????*)

      set_pf "x_height" 399.;
      set_pf "curve_overshoot" 10.;
      set_pf "curve_undershoot" 10.;
      set_pf "flag_overshoot" 8.;
      set_pf "e_crossbar_height" 258.;
      set_pf "t_crossbar_height" 402.;
      set_pf "t_top_corner_height" 550.;
      set_pf "i_dot_height" 623.;
      set_pf "ascender_height" 697.;
      set_pf "stem_width" 57.;
      set_pf "serif_height" 24.;

      set_pf_func "corner_radius" (fun () -> Some (float_of_int (Random.State.int state 3 + 2)));
      set_pf_func "serif_end_angle" (fun () -> Some (float_of_int (Random.State.int state 101 - 50) /. 9.));
      set_pf_func "tail_end_angle" (fun () -> Some (float_of_int (Random.State.int state 101) /. 10.));

      set_pm_func "left_bracket"
        (fun () ->
          let left_pos = -. (float_of_int (Random.State.int state 11 + 15)) in
          let horiz_tension = 0.01 *. float_of_int (Random.State.int state 51 + 100) in
          let vert_tension = 0.01 *. float_of_int (Random.State.int state 51 + 100) in
          Some (Metacubic.(Complex_point.(
            point ~dir:rightward (x' left_pos)
            |> dput ~tensions:(horiz_tension, vert_tension) (point ~dir:upward (y' 70.))
          ))));

      set_pm_func "right_bracket"
        (fun () ->
          let left_pos = (float_of_int (Random.State.int state 11 + 15)) in
          let horiz_tension = 0.01 *. float_of_int (Random.State.int state 51 + 100) in
          let vert_tension = 0.01 *. float_of_int (Random.State.int state 51 + 100) in
          Some (Metacubic.(Complex_point.(
            point ~dir:downward (y' 70.)
            |> dput ~tensions:(vert_tension, horiz_tension) (point ~dir:rightward (x' left_pos))
          ))));

      set_pm "r_shoulder"
        (Complex_point.(Metacubic.(
          point ~out_curl:0.1 (y' 310.)
          |> put (point ~dir:rightward (x' 140. + y' 397.))
         )));

      set_pm "r_arm_end"
        (Complex_point.(Metacubic.(
          point ~dir:downward (x' 207. + y' 350.)
         )));

      set_pm_func "r_arm_lower"
        (fun () ->
          Some (Complex_point.(Metacubic.(
            point ~dir:leftward (x' 164. + y' 306.)
            |> put (point ~dir:leftward (x' 79. + y' 335.))
            |> put ~tension:1.15 (point ~dir:downward (y' 260.))
          ))));

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
