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
      let xrel amount = amount *. extended_width in
      let yrel amount = amount *. height in
      let x'rel = x' -| xrel in
      let y'rel = y' -| yrel in
      let x'pos v = x'rel v - x' left_overlap in
      let y'pos v = y'rel v - y' undershoot in

(*
      let xy'rel angle amount = (x'rel (amount *. dcos angle) + y'rel (amount *. dsin angle)) in
      let xyrel angle amount = norm (xy'rel angle amount) in
      let xy'pos angle amount = xy'rel angle amount - y' undershoot in
*)

      let module Tools =
          struct
            let width = extended_width
            let height = height
            let left_overlap = left_overlap
            let overshoot = overshoot
            let undershoot = undershoot
            let xrel = xrel
            let yrel = yrel
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

  let overshoot = p.curve_overshoot +. 2. in
  let undershoot = p.curve_undershoot +. 2. in

  let tools =
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

    let left_breadth = 1.07 *. p.lc_stem_width in
    let bottom_breadth = 0.92 *. p.lc_stem_width in
    let top_breadth = 0.64 *. p.lc_stem_width /. p.contrast in

    let bottom_pt = x'pos 0.56 + y'pos 0.00 in
    let top_pt = x'pos 0.63 + y'pos 1.00 in
    let left_pt = x'pos 0.0 + y'pos 0.5 in

    let tail_cut_angle = -40. in
    let tail2 = x'pos 0.98 + y' 69.0000 in
    let tail1 = tail2 + y_shear (x'(-13.)) tail_cut_angle in

    let outer =
      make_node                         (* tail *)
        (x' 3. * rot (70.))
        tail2
        (x' 40. * rot (70. -. 180.))
      <@> make_node                     (* bottom *)
        (x'rel 0.20)
        bottom_pt
        (x'rel (-0.37))
      <@> make_node                     (* left *)
        (y'rel (-0.27))
        left_pt
        (y'rel 0.30)
      <@> make_node                     (* top *)
        (x'rel (-0.36))
        top_pt
        (x'rel 0.19)
    in
    let terminal =
      make_node                         (* right *)
        (y'rel 0.07)
        (x'pos 0.98 + y'pos 0.82)
        (y'rel (-0.04))
      <@> make_node                     (* turning inward *)
        (x'rel 0.05)
        (x'pos 0.89 + y'pos 0.75)
        (x'rel (-0.13))
    in
    let inner =
      make_node                         (* inner top *)
        (x'rel 0.17)
        (top_pt - x'rel 0.05 - y' top_breadth)
        (x'rel (-0.19))
      <@> make_node                     (* inner left *)
        (y'rel 0.26)
        (left_pt + x' left_breadth + y'pos 0.05)
        (y'rel (-0.21))
      <@> make_node                     (* inner bottom *)
        (x'rel (-0.29))
        (bottom_pt + x'rel 0.09 + y' bottom_breadth)
        (x'rel 0.10)
      <@> make_node                     (* tail *)
        (x' 35.3553 * rot 230.)
        tail1
        (x' 3. * rot (230. -. 180.))
    in

(*
    let midpoint = x' 0.5 * (tail1 + tail2) in
    let cut = make_node (x' 0.8 * (tail1 - midpoint)) midpoint (x' 0.8 * (tail2 - midpoint)) in

    let inner' = inner <-> midpoint <*> rot (-90. -. tail_cut_angle) in
    let times = times_at_x inner' (-7.) in
    let time_inner = times.(Int.pred (Array.length times)) in
    let outer' = outer <-> midpoint <*> rot (-90. -. tail_cut_angle) in
    let time_outer = (times_at_x outer' (-7.)).(0) in
    let (inner, _) = subdivide inner time_inner in
    let (_, outer) = subdivide outer time_outer in
    let inner = modify_outhandle inner (x' 5.) in
    let outer = modify_inhandle outer (x' 5.) in
*)

    outer <@> terminal <@> inner
    <@@ true <.> round
  )))

let letter_o_contours = 

  let width = 383. in

  let outer_contour p =
    Cubic.(Complex_point.(

      let width = width *. p.extension in
      let overshoot = p.curve_overshoot in
      let undershoot = p.curve_undershoot in
      let height = p.x_height +. undershoot +. overshoot in
      let y_coord v = y'(v -. undershoot) in
      let xrel amount = amount *. width in
      let yrel amount = amount *. height in

      let left = 1.16 *. p.lc_stem_width in
      let right = 1.16 *. p.lc_stem_width in
      let bottom = 0.58 *. p.lc_stem_width /. p.contrast in
      let top = 0.54 *. p.lc_stem_width /. p.contrast in

      make_node
        (y'(-0.27 *. height))
        (y_coord(0.50 *. height))
        (y'(0.32 *. height))
      <@> make_node
        (x'(-0.22 *. width))
        (x'(0.50 *. width) + y'(p.x_height +. overshoot))
        (x'(0.28 *. width))
      <@> make_node
        (y'(0.27 *. height))
        (x' width + y_coord(0.50 *. height))
        (y'(-0.28 *. height))
      <@> make_node
        (x'(0.29 *. width))
        (x'(0.49 *. width) + y'(-. undershoot))
        (x'(-0.30 *. width))
      <@@ true <.> round
    ))
  in
  let inner_contour p =

    Cubic.(Complex_point.(

      let width = width *. p.extension in
      let overshoot = p.curve_overshoot in
      let undershoot = p.curve_undershoot in
      let height = p.x_height +. undershoot +. overshoot in
      let y_coord v = y'(v -. undershoot) in
      let xrel amount = amount *. width in
      let yrel amount = amount *. height in

      let left = 1.16 *. p.lc_stem_width in
      let right = 1.16 *. p.lc_stem_width in
      let bottom = 0.58 *. p.lc_stem_width /. p.contrast in
      let top = 0.54 *. p.lc_stem_width /. p.contrast in

      make_node
        (y'(yrel 0.22))
        (x' left + y_coord (yrel 0.52))
        (y'(yrel (-0.30)))
      <@> make_node
        (x'(xrel (-0.16)))
        (x'(xrel 0.49) - y' undershoot + y' bottom)
        (x'(xrel 0.14))
      <@> make_node
        (y'(yrel (-0.28)))
        (x' width - x' right + y_coord (yrel 0.48))
        (y'(yrel 0.24))
      <@> make_node
        (x'(xrel 0.22))
        (x'(xrel 0.48) + y'(p.x_height +. overshoot) - y' top)
        (x'(xrel (-0.19)))
      <@@ true <.> round
    ))
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

