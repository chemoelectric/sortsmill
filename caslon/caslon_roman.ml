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

module Contour = Parameterized_contour(Param)
open Contour
open Param

(*-----------------------------------------------------------------------*)

let glyph_table : (Param.t -> float, Contour.t) Glyph.t StringMap.t ref =
  ref StringMap.empty

let resolve_glyph glyph param =
  (Glyph.transform (fun r -> r param) (flip resolve param) glyph)

let add_glyph glyph = glyph_table := Glyph.(StringMap.add glyph.name glyph !glyph_table)
let have_glyph name = StringMap.mem name !glyph_table
let get_glyph name = StringMap.find name !glyph_table
let enum_glyphs () = map snd (StringMap.enum !glyph_table)
let get_resolve_glyph name param = resolve_glyph (get_glyph name) param
let enum_resolve_glyphs param = map (flip resolve_glyph param) (enum_glyphs ())
;;

(*-----------------------------------------------------------------------*)

let letter_c_contour p =

  let overshoot = p.curve_overshoot +. 3. in
  let undershoot = p.curve_undershoot +. 3. in

  let tools p =
    make_tools
          ~width:320.
          ~height:(p.x_height +. undershoot +. overshoot)
          ~undershoot:undershoot
          ~overshoot:overshoot
          ~param:p
          ()
  in

  let module Tools = (val tools p : Tools_module) in
  
  Tools.(Cubic.(Complex_point.(

    let left_breadth = 1.12 *. p.lc_stem_width in
    let bottom_breadth = 0.92 *. p.lc_stem_width /. p.contrast in
    let top_breadth = 0.64 *. p.lc_stem_width /. p.contrast in
    let head_breadth = 1.00 *. p.lc_stem_width in
    let tail_breadth = 0.20 *. p.lc_stem_width in

    let tail_cut_angle = -35. in
    let tail1 = x'pos 0.99 + y'pos 0.20 in
    let tail2 = tail1 - y_shear (x' tail_breadth) tail_cut_angle in

    let head_cut_angle = 20. in
    let head1 = x'pos 0.98 + y'pos 0.82 in
    let head2 = head1 - y_shear (x' head_breadth) head_cut_angle in

    let outer =
      let tail_angle = 75. in
      let head_angle = 100. in
      make_node                         (* tail *)
        (x' 3. * rot tail_angle)
        tail1
        (neg (x_shear (y'rel 0.09) (90. -. tail_angle)))
      <@> make_horiz_node               (* bottom *)
        (xpos 0.78)
        (x'pos 0.56 + y'pos 0.00)
        (xpos 0.19)
      <@> make_vert_node                (* left *)
        (ypos 0.23)
        (x'pos 0.0 + y'pos 0.49)
        (ypos 0.79)
      <@> make_horiz_node               (* top *)
        (xpos 0.29)
        (x'pos 0.62 + y'pos 1.00)
        (xpos 0.82)
      <@> make_node                     (* head *)
        (x_shear (y'rel 0.08) (90. -. head_angle))
        head1
        (neg (x_shear (y'rel 0.05) (90. -. head_angle)))
    in
    let inner =
      let tail_angle = 50. in
      let head_angle = 130. in
      make_node                         (* head *)
        (neg (x_shear (y'rel 0.05) (90. -. head_angle)))
        head2
        (x_shear (y'rel 0.05) (90. -. head_angle))
      <@> make_horiz_node               (* inner top *)
        (xpos 0.68)
        (x'pos 0.59 + y'pos 1.00 - y' top_breadth)
        (xpos 0.40)
      <@> make_vert_node                (* inner left *)
        (ypos 0.79)
        (x'pos 0.00 + x' left_breadth + y'pos 0.52)
        (ypos (0.32 +. p.contrast *. 0.01))
      <@> make_horiz_node               (* inner bottom *)
        (xpos (0.33 +. p.contrast *. 0.05))
        (x'pos 0.64 + y'pos 0.00 + y' bottom_breadth)
        (xpos 0.78)
      <@> make_node                     (* tail *)
        (neg (x_shear (y'rel 0.06) (90. -. tail_angle)))
        tail2
        (x' 3. * rot tail_angle)
    in

    outer <@> inner
    <@@ true <.> round
  )))

let letter_o_contours =

  let tools p =
    make_tools
          ~width:383.
          ~height:(p.x_height +. p.curve_undershoot +. p.curve_overshoot)
          ~undershoot:p.curve_undershoot
          ~overshoot:p.curve_overshoot
          ~param:p
          ()
  in

  let outer_contour p =

    let module Tools = (val tools p : Tools_module) in

    Tools.(Cubic.(Complex_point.(

      let left = 1.16 *. p.lc_stem_width in
      let right = 1.16 *. p.lc_stem_width in
      let bottom = 0.58 *. p.lc_stem_width /. p.contrast in
      let top = 0.54 *. p.lc_stem_width /. p.contrast in

      make_vert_node                    (* left *)
        (ypos 0.23)
        (x'pos 0.00 + y'pos 0.50)
        (ypos 0.82)
      <@> make_horiz_node               (* top *)
        (xpos 0.28)
        (x'pos 0.50 + y'pos 1.00)
        (xpos 0.78)
      <@> make_vert_node                (* right *)
        (ypos 0.77)
        (x'pos 1.00 + y'pos 0.50)
        (ypos 0.22)
      <@> make_horiz_node               (* bottom *)
        (xpos 0.78)
        (x'pos 0.49 + y'pos 0.00)
        (xpos 0.19)
      <@@ true <.> round
    )))
  in
  let inner_contour p =

    let module Tools = (val tools p : Tools_module) in

    Tools.(Cubic.(Complex_point.(

      let left = 1.16 *. p.lc_stem_width in
      let right = 1.16 *. p.lc_stem_width in
      let bottom = 0.58 *. p.lc_stem_width /. p.contrast in
      let top = 0.54 *. p.lc_stem_width /. p.contrast in

      make_vert_node                    (* left *)
        (ypos 0.74)
        (x'pos 0.00 + x' left + y'pos 0.52)
        (ypos 0.22)
      <@> make_horiz_node               (* top *)
        (xpos 0.33)
        (x'pos 0.49 + y'pos 0.00 + y' bottom)
        (xpos 0.63)
      <@> make_vert_node                (* right *)
        (ypos 0.20)
        (x'pos 1.00 - x' right + y'pos 0.48)
        (ypos 0.72)
      <@> make_horiz_node               (* bottom *)
        (xpos 0.70)
        (x'pos 0.48 + y'pos 1.00 - y' top)
        (xpos 0.29)
      <@@ true <.> round
    )))
  in

  [outer_contour; inner_contour]
;;

(*-----------------------------------------------------------------------*)

(* Space character *)
add_glyph Glyph.({
  empty with
    name = "space";
    rsb = Some (fun p -> p.space_width);
})
;;

(* Letter "c" *)
add_glyph Glyph.({
  empty with
    name = "c";
    contours = [Contour.of_param_to_cubic letter_c_contour];
    lsb = Some (const 0.);
    rsb = Some (const 50.);
})
;;

(* Letter "o" *)
add_glyph Glyph.({
  empty with
    name = "o";
    contours = List.map Contour.of_param_to_cubic letter_o_contours;
    lsb = Some (const 0.);
    rsb = Some (const 50.);
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
