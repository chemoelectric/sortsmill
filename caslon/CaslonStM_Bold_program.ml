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

  fontname = "CaslonStM-Bold";
  familyname = "Sorts Mill Caslon";
  fullname = "Sorts Mill Caslon Bold";
  weight = "Regular";

  family = "Sorts Mill Caslon";
  subfamily = "Regular";
  preferred_family = None;
  preferred_subfamily = None;
  wws_family = None;
  wws_subfamily = None;

  os2_weight = 700;
  design_size = 12.;

  contrast = 0.5;
  extension = 0.1;
  space_width = 200.;
  x_height = 399.;
  curve_overshoot = 10.;
  curve_undershoot = 10.;
  flag_overshoot = 10.;
  e_crossbar_height = 258.;
  t_crossbar_height = 402.;
  t_top_corner_height = 560.;
  i_dot_height = 623.;
  ascender_height = 700.;
  stem_width = (fun _name -> 90.);
  serif_height = (fun _name -> 30.);

  corner_radius = (fun state -> float_of_int (Random.State.int state 3 + 4));
(*
  flag_corner_radius = (fun state -> 10.);
  flag_top_radius = (fun state -> 10.);
*)

  serif_end_angle = (fun state -> float_of_int (Random.State.int state 101 - 50) /. 9.);
  tail_end_angle = (fun state -> float_of_int (Random.State.int state 101) /. 10.);

  left_bracket =
    (fun name state ->
      let left_pos =
        if List.exists (( = ) name) i_letters then
          (-23.)
        else
          (-20.)
      in
      let horiz_tension = 1.5 in
      let vert_tension = 1.5 in
      Metacubic.(Complex_point.(
        point ~dir:rightward (x' left_pos)
        |> putd ~tensions:(horiz_tension, vert_tension) (point ~dir:upward (y' 70.))
      )));

  right_bracket =
    (fun _name state ->
      let left_pos = 20. in
      let horiz_tension = 1.5 in
      let vert_tension = 1.5 in
      Metacubic.(Complex_point.(
        point ~dir:downward (y' 70.)
        |> putd ~tensions:(vert_tension, horiz_tension) (point ~dir:rightward (x' left_pos))
      )));
}
in
run_command param
