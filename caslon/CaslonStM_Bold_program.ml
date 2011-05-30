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
      initialize_state "state";

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

      (* ?????????????????????????????????????????????????????????????????????????????????????*)
      set_pf "contrast" 0.5;
      set_pf "extension" 0.1;
      (* ?????????????????????????????????????????????????????????????????????????????????????*)

      set_pf "stem_width" 90.;
      set_pf "serif_height" 30.;

      set_pf_func "corner_radius" (fun () -> Some (float_of_int (Random.State.int (prs "state") 3 + 4)));

      set_pf_func "serif_end_angle" (fun () -> Some (float_of_int (Random.State.int (prs "state") 101 - 50) /. 9.));
      set_pf_func "tail_end_angle" (fun () -> Some (float_of_int (Random.State.int (prs "state") 101) /. 10.));

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
      );

      if Set.mem glyph_name e_letters then (
        set_pf "width" 389.;
        set_pf "height" 423.;
        set_pf "bottom_overlap" 10.;
        set_pf "crossbar_height" 258.;
      );

      if Set.mem glyph_name i_letters then (
        set_pf "height" 415.;
        set_pf "dot_height" 623.;
      );

      if Set.mem glyph_name l_letters then (
        set_pf "height" 707.;
      );

      if Set.mem glyph_name o_letters then (
        set_pf "width" 421.;
        set_pf "height" 419.;
        set_pf "bottom_overlap" 10.;
      );

      if Set.mem glyph_name r_letters then (
        set_pf "height" 420.;
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
        set_pf "crossbar_height" 402.;
        set_pf "top_corner_height" 550.;
      );

      !p_ref

  in
  run_command params
