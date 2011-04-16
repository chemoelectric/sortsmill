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
module Complex = Fontdesign.Extended_complex
open Fontdesign
open OptParse

module PCF = Parameterized_cubics(Float)
open PCF
;;

let glyph_opt = StdOpt.str_option () ;;
let font_opt = StdOpt.str_option () ;;
let font_flags_opt = StdOpt.str_option () ;;
let sfd_opt = StdOpt.str_option () ;;
let opt_parser = OptParser.make () ;;
OptParser.add ~short_name:'g' ~long_name:"glyph" opt_parser glyph_opt ;;
OptParser.add ~short_name:'f' ~long_name:"font" opt_parser font_opt ;;
OptParser.add ~short_name:'l' ~long_name:"flags" opt_parser font_flags_opt ;;
OptParser.add ~short_name:'s' ~long_name:"sfd" opt_parser sfd_opt ;;
let args = OptParser.parse_argv opt_parser ;;

let glyphs = Hashtbl.create 1000 ;;

let c_glyph =
  let c12 =
    Complex.(Contour.(Node.(
      of_node_list [
        make_node (y'(-55.5498)) (y' 100.0000) (zero);
        make_node (x'(-55.5498) + y'(-30.6602)) (x' 100.0000 + y' 200.0000) (x' 55.5498);
        make_pin (x' 200.0000 + y' 100.0000);
        make_flat (x' 100.0000) (x'(-0.9417) + y' 0.3363) (x' 59.4639) (x' 55.5494);
      ] <@@ true
    ))) in
  Glyph.(empty |> with_name "c" |> with_contours [c12] |> with_rsb 25. (*  |> with_lsb 25. *) )
in
Hashtbl.replace glyphs (Glyph.name c_glyph) c_glyph
;;

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

    Enum.iter
      (Glyph.print_python_glyph_code Float.print print_python_contour_code stdout)
      (Hashtbl.values glyphs);

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
    if Hashtbl.mem glyphs glyph_name then
      let glyph = Hashtbl.find glyphs glyph_name in
      Glyph.print_python_glyph_update_module Float.print print_python_contour_code stdout glyph

(* local variables: *)
(* mode: tuareg *)
(* end: *)
