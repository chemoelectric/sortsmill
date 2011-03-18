/*
  Copyright (c) 2011 Barry Schwartz

  Permission is hereby granted, free of charge, to any person
  obtaining a copy of this software and associated documentation files
  (the "Software"), to deal in the Software without restriction,
  including without limitation the rights to use, copy, modify, merge,
  publish, distribute, sublicense, and/or sell copies of the Software,
  and to permit persons to whom the Software is furnished to do so,
  subject to the following conditions:

  The above copyright notice and this permission notice shall be
  included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

*/

import sortsmill_glyph;

//-------------------------------------------------------------------------

struct Font {
    Glyph[] glyph_list;
    string version;
    string copyright_notice;
    string fontname;
    string familyname;
    string fullname;
    string weight;
    string sfnt_family;
    string sfnt_style;
    string preferred_family;
    string preferred_style;
    string wws_family;
    string wws_style;
    string license_url;
    string vendor_url;
    string designer_name;
    string designer_url;
    real design_size;

    int find_glyph(string glyphname) {
        int i = 0;
        while (i < glyph_list.length && glyph_list[i].name != glyphname)
            i += 1;
        return i;
    }

    Glyph get_glyph(string glyphname) {
        Glyph glyph;
        int i = find_glyph(glyphname);
        if (i < glyph_list.length)
            glyph = glyph_list[i];
        return glyph;
    }

    void set_glyph(Glyph glyph) {
        int i = find_glyph(glyph.name);
        glyph_list[i] = glyph;
    }

    void write_activeGlyph_update_code(string glyphname) {

        // For use in interactive FontForge sessions.

        write('import fontforge');
        Glyph glyph = get_glyph(glyphname);
        if (glyph.name != '') {
            write('glyph = fontforge.activeGlyph()');
            glyph.write_fontforge_code('contour', stdout);
        }
    }
};

//-------------------------------------------------------------------------
