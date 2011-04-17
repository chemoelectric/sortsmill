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

module Complex = Fontdesign.Extended_complex
open Fontdesign

module Param =
struct
  type t = {
    space_width : float;
    x_height : float;
    curve_overshoot : float;
    curve_undershoot : float;
    lc_stem_width : float;
  }
end

open Param
module PC = Parameterized_cubics(Param)
open PC

(*-----------------------------------------------------------------------*)

let glyph_table = ref PMap.empty

let resolve_glyph glyph param =
  Glyph.transform (fun r -> r param) (flip resolve_contour param) glyph

let add_glyph glyph = glyph_table := Glyph.(PMap.add glyph.name glyph !glyph_table)
let have_glyph name = PMap.mem name !glyph_table
let get_glyph name = PMap.find name !glyph_table
let enum_glyphs () = map snd (PMap.enum !glyph_table)
let get_resolve_glyph name param = resolve_glyph (get_glyph name) param
let enum_resolve_glyphs param = map (flip resolve_glyph param) (enum_glyphs ())
;;

(*-----------------------------------------------------------------------*)

(* Space character *)
add_glyph Glyph.({
  empty with
    name = "space";
    lsb = Some (const 0.);
    rsb = Some (fun p -> p.space_width);
})
;;

(* Letter "o" *)
let width = 383. in
let outer_contour =
  PContour.(Node.(PComplex.(
    of_node_list [
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
        (x'(fun _ -> 0.49 *. width) + y'(fun p -> -. p.curve_overshoot))
        (x'(fun _ -> 0.29 *. width))
        (x'(fun _ -> 0.30 *. width));
    ] <@@ true <.> round
  )))
in
let inner_contour =
  let left = (fun p -> 1.16 *. p.lc_stem_width) in
  let right = (fun p -> 1.16 *. p.lc_stem_width) in
  let bottom = (fun p -> 0.58 *. p.lc_stem_width) in
  let top = (fun p -> 0.54 *. p.lc_stem_width) in
  PContour.(Node.(PComplex.(
    of_node_list [
      make_down
        (x' left + y'(fun p -> 0.52 *. p.x_height))
        (x'(fun p -> 0.23 *. p.x_height))
        (x'(fun p -> 0.32 *. p.x_height));
      make_right
        (x'(fun _ -> 0.49 *. width) + y'(fun p -> -. p.curve_overshoot) + y' bottom)
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
    ] <@@ true <.> round
  )))
in
add_glyph Glyph.({
  empty with
    name = "o";
    contours = [outer_contour; inner_contour];
})
;;

(*-----------------------------------------------------------------------*)

let run_command param =
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
      print_endline "#!/usr/bin/env python";
      print_endline "";
      print_endline "import fontforge";
      print_endline "import psMat";
      print_endline "";
      print_endline "my_font = fontforge.font()";
      print_endline "";
      print_endline "my_font.version = '0.1'";
      print_endline "my_font.fontname = 'CaslonStM'";
      print_endline "my_font.familyname = 'Sorts Mill Caslon'";
      print_endline "my_font.fullname = 'Sorts Mill Caslon'";
      print_endline "my_font.weight = 'Regular'";
      print_endline "my_font.appendSFNTName('English (US)', 'Family', 'Sorts Mill Caslon')";
      print_endline "my_font.appendSFNTName('English (US)', 'SubFamily', 'Regular')";
      print_endline "";
      print_endline "my_font.size_feature = (12,)";
      print_endline "";
      print_endline "my_font.os2_weight = 400";
      print_endline "";
      print_endline "def preferred_unicode(glyphname):";
      print_endline "    if '_' in glyphname:";
      print_endline "        uni = -1";
      print_endline "    else:";
      print_endline "        uni = fontforge.unicodeFromName(glyphname)";
      print_endline "    return uni";
      print_endline "";

(*
      Enum.iter
        (Glyph.print_python_glyph_code Float.print Contour.print_python_contour_code stdout)
        (Hashtbl.values glyphs);
*)

      print_endline "";
      print_endline "my_font.encoding = 'UnicodeBMP'";
      print_endline "";
      if Option.is_some (Opt.opt sfd_opt) then
        Print.printf p"my_font.save('%s')" (Option.get (Opt.opt sfd_opt));
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
        Glyph.print_python_glyph_update_module
          Float.print print_python_contour_code
          stdout glyph

(*-----------------------------------------------------------------------*)
