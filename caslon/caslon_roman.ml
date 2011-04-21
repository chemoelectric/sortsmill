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

    design_size : float;
    space_width : float;
    x_height : float;
    curve_overshoot : float;
    curve_undershoot : float;
    lc_stem_width : float;
  }
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

let letter_o_contours = 
  let width = 383. in
  let outer_contour =
    PCubic.(PComplex.(
      List.flatten [
        make_up
          (y'(fun p -> 0.50 *. p.x_height))
          (x'(fun p -> 0.29 *. p.x_height))
          (x'(fun p -> 0.34 *. p.x_height));
        make_right
          (x'(fun _ -> 0.50 *. width) + y'(fun p -> p.x_height +. p.curve_overshoot))
          (x'(fun _ -> 0.22 *. width))
          (x'(fun _ -> 0.28 *. width));
        make_down
          (x'(const width) + y'(fun p -> 0.5 *. p.x_height))
          (x'(fun p -> 0.28 *. p.x_height))
          (x'(fun p -> 0.30 *. p.x_height));
        make_left
          (x'(fun _ -> 0.49 *. width) + y'(fun p -> -. p.curve_undershoot))
          (x'(fun _ -> 0.29 *. width))
          (x'(fun _ -> 0.30 *. width))
      ] |> close <.> round
    ))
  in
  let inner_contour =
    let left = (fun p -> 1.16 *. p.lc_stem_width) in
    let right = (fun p -> 1.16 *. p.lc_stem_width) in
    let bottom = (fun p -> 0.58 *. p.lc_stem_width) in
    let top = (fun p -> 0.54 *. p.lc_stem_width) in
    PCubic.(PComplex.(
      List.flatten [
        make_down
          (x' left + y'(fun p -> 0.52 *. p.x_height))
          (x'(fun p -> 0.23 *. p.x_height))
          (x'(fun p -> 0.32 *. p.x_height));
        make_right
          (x'(fun _ -> 0.49 *. width) + y'(fun p -> -. p.curve_undershoot) + y' bottom)
          (x'(fun _ -> 0.16 *. width))
          (x'(fun _ -> 0.14 *. width));
        make_up
          (x'(fun _ -> width) - x' right + y'(fun p -> 0.48 *. p.x_height))
          (x'(fun p -> 0.30 *. p.x_height))
          (x'(fun p -> 0.25 *. p.x_height));
        make_left
          (x'(fun _ -> 0.48 *. width) + y'(fun p -> p.x_height +. p.curve_overshoot) - y' top)
          (x'(fun _ -> 0.22 *. width))
          (x'(fun _ -> 0.19 *. width));
      ] |> close <.> round
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

(* Letter "o" *)
add_glyph Glyph.({
  empty with
    name = "o";
    contours = List.map of_pcubic letter_o_contours;
    lsb = Some (const 25.);
    rsb = Some (const 35.);
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

