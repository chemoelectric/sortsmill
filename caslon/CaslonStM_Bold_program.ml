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

  let p_ref : Param.t ref = ref no_parameters in

  let pi_func name ifunc = p_ref := put_int_func !p_ref name ifunc in
  let pf_func name ffunc = p_ref := put_float_func !p_ref name ffunc in
  let ps_func name sfunc = p_ref := put_string_func !p_ref name sfunc in
  let pm_func name mfunc = p_ref := put_metacubic_func !p_ref name mfunc in
  let pi name i = p_ref := put_int !p_ref name i in
  let pf name v = p_ref := put_float !p_ref name v in
  let ps name s = p_ref := put_string !p_ref name s in
  let pm name m = p_ref := put_metacubic !p_ref name m in
  let null = (fun () -> None) in

  ps "version" "0.1";

  pi "os2_weight" 700;
  pf "design_size" 12.;

  let state =
    Random.State.make
      (Array.of_list
         (int_of_float (float !p_ref "design_size") ::
            int !p_ref "os2_weight" ::
            List.map Char.code (String.explode glyph_name)))
  in

  ps "fontname" "CaslonStM-Bold";
  ps "familyname" "Sorts Mill Caslon";
  ps "fullname" "Sorts Mill Caslon Bold";
  ps "weight" "Regular";

  ps "family" "Sorts Mill Caslon";
  ps "subfamily" "Regular";
  ps_func "preferred_family" null;
  ps_func "preferred_subfamily" null;
  ps_func "wws_family" null;
  ps_func "wws_subfamily" null;

  pf "contrast" 0.5;
  pf "extension" 0.1;
  pf "space_width" 200.;
  pf "x_height" 399.;
  pf "curve_overshoot" 10.;
  pf "curve_undershoot" 10.;
  pf "flag_overshoot" 10.;
  pf "e_crossbar_height" 258.;
  pf "t_crossbar_height" 402.;
  pf "t_top_corner_height" 550.;
  pf "i_dot_height" 623.;
  pf "ascender_height" 700.;
  pf "stem_width" 90.;
  pf "serif_height" 30.;

  pf_func "corner_radius" (fun () -> Some (float_of_int (Random.State.int state 3 + 4)));

  pf_func "serif_end_angle" (fun () -> Some (float_of_int (Random.State.int state 101 - 50) /. 9.));
  pf_func "tail_end_angle" (fun () -> Some (float_of_int (Random.State.int state 101) /. 10.));

  pm_func "left_bracket"
    (fun () ->
      let left_pos =
        if List.exists (( = ) glyph_name) i_letters then
          (-23.)
        else
          (-20.)
      in
      let horiz_tension = 1.5 in
      let vert_tension = 1.5 in
      Some (Metacubic.(Complex_point.(
        point ~dir:rightward (x' left_pos)
        |> dput ~tensions:(horiz_tension, vert_tension) (point ~dir:upward (y' 70.))
      ))));

  pm_func "right_bracket"
    (fun () ->
      let left_pos = 20. in
      let horiz_tension = 1.5 in
      let vert_tension = 1.5 in
      Some (Metacubic.(Complex_point.(
        point ~dir:downward (y' 70.)
        |> dput ~tensions:(vert_tension, horiz_tension) (point ~dir:rightward (x' left_pos))
      ))));

  pm_func "r_shoulder"
    (fun () ->
      Some (Complex_point.(Metacubic.(
        point ~out_curl:0.1 (y' 309.)
        |> put (point ~dir:rightward (x' 145. + y' 407.))
      ))));

  pm_func "r_arm_end"
    (fun () -> 
      Some (Complex_point.(Metacubic.(
        point ~dir:downward (x' 212. + y' 350.)
      ))));

  pm_func "r_arm_lower"
    (fun () ->
      Some (Complex_point.(Metacubic.(
        point ~dir:leftward (x' 164. + y' 301.)
        |> put (point ~dir:leftward (x' 79. + y' 330.))
        |> put ~tension:1.15 (point ~dir:downward (y' 255.))
      ))));

  !p_ref

in
run_command params
