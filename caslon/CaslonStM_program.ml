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
  e_crossbar_height = 258.;
  ascender_height = 700.;
  lc_stem_width = 57.;
  lc_serif_height = 24.;

  serif_end_angle = (fun state -> float_of_int (Random.State.int state 101 - 50) /. 9.);
  corner_radius = (fun state -> float_of_int (Random.State.int state 3 + 4));

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
