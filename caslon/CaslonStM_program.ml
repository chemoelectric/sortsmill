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

let param = {
  version = "0.1";

  fontname = "CaslonStM";
  familyname = "Sorts Mill Caslon";
  fullname = "Sorts Mill Caslon";
  weight = "Regular";

  family = "Sorts Mill Caslon";
  subfamily = "Regular";
  preferred_family = None;
  preferred_subfamily = None;
  wws_family = None;
  wws_subfamily = None;

  os2_weight = 400;
  design_size = 12.;

  contrast = 0.;
  extension = 0.;
  space_width = 200.;
  x_height = 399.;
  curve_overshoot = 10.;
  curve_undershoot = 10.;
  flag_overshoot = 8.;
  e_crossbar_height = 258.;
  t_crossbar_height = 402.;
  t_top_corner_height = 560.;
  i_dot_height = 623.;
  ascender_height = 697.;
  stem_width = (fun _name -> 57.);
  serif_height = (fun _name -> 24.);

  corner_radius = (fun state -> float_of_int (Random.State.int state 3 + 2));
  serif_end_angle = (fun state -> float_of_int (Random.State.int state 101 - 50) /. 9.);
  tail_end_angle = (fun state -> float_of_int (Random.State.int state 101) /. 10.);

  left_bracket =
    (fun _name state ->
      let left_pos = -. (float_of_int (Random.State.int state 11 + 15)) in
      let horiz_tension = 0.01 *. float_of_int (Random.State.int state 51 + 100) in
      let vert_tension = 0.01 *. float_of_int (Random.State.int state 51 + 100) in
      Metacubic.(Complex_point.(
        point ~dir:rightward (x' left_pos)
        |> dput ~tensions:(horiz_tension, vert_tension) (point ~dir:upward (y' 70.))
      )));

  right_bracket =
    (fun _name state ->
      let left_pos = (float_of_int (Random.State.int state 11 + 15)) in
      let horiz_tension = 0.01 *. float_of_int (Random.State.int state 51 + 100) in
      let vert_tension = 0.01 *. float_of_int (Random.State.int state 51 + 100) in
      Metacubic.(Complex_point.(
        point ~dir:downward (y' 70.)
        |> dput ~tensions:(vert_tension, horiz_tension) (point ~dir:rightward (x' left_pos))
      )));

  r_shoulder =
    Complex_point.(Metacubic.(
      fun _name _state ->
        point ~out_curl:0.1 (y' 310.)
        |> put (point ~dir:rightward (x' 140. + y' 397.))
    ));

  r_arm_lower =
    Complex_point.(Metacubic.(
      fun _name _state ->
        point ~dir:downward (x' 207. + y' 350.)
        |> put (point ~dir:leftward (x' 164. + y' 306.))
        |> put (point ~dir:leftward (x' 79. + y' 335.))
        |> put ~tension:1.15 (point ~dir:downward (y' 260.))
    ));

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
}
in
run_command param
