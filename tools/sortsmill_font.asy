// Copyright (c) 2011 Barry Schwartz
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

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
    string sfnt_subfamily;
    string preferred_family;
    string preferred_subfamily;
    string wws_family;
    string wws_subfamily;
    string license_notice;
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
            write('my_glyph = fontforge.activeGlyph()');
            glyph.write_fontforge_code(glyphvar_name = 'my_glyph');
        }
    }

    void write_script_header_code() {
        write('#!/usr/bin/env python\n\n'
              'import fontforge\n'
              'import psMat');
    }

    void write_font_open_code(string fontvar_name = 'my_font') {
        write('\n' + fontvar_name + ' = fontforge.font()');
    }

    void write_font_names_code(string fontvar_name = 'my_font') {
        write();
        write(fontvar_name + '.version = \'' + version + '\'\n' +
              fontvar_name + '.fontname = \'' + fontname + '\'\n' +
              fontvar_name + '.familyname = \'' + familyname + '\'\n' +
              fontvar_name + '.fullname = \'' + fullname + '\'\n' +
              fontvar_name + '.weight = \'' + weight + '\'');

        if (copyright_notice != '')
            write(fontvar_name + '.copyright = \'' + copyright_notice + '\'');

        string lang = '\'English (US)\'';

        if (sfnt_family != '')
            write(fontvar_name + '.appendSFNTName(' + lang + ', \'Family\', \'' + sfnt_family + '\')');
        if (sfnt_subfamily != '')
            write(fontvar_name + '.appendSFNTName(' + lang + ', \'SubFamily\', \'' + sfnt_subfamily + '\')');

        if (preferred_family != '')
            write(fontvar_name + '.appendSFNTName(' + lang + ', \'Preferred Family\', \'' + preferred_family + '\')');
        if (preferred_subfamily != '')
            write(fontvar_name + '.appendSFNTName(' + lang + ', \'Preferred Styles\', \'' + preferred_subfamily + '\')');
    
        if (wws_family != '')
            write(fontvar_name + '.appendSFNTName(' + lang + ', \'WWS Family\', \'' + wws_family + '\')');
        if (wws_subfamily != '')
            write(fontvar_name + '.appendSFNTName(' + lang + ', \'WWS Subfamily\', \'' + wws_subfamily + '\')');

        if (license_notice != '')
            write(fontvar_name + '.appendSFNTName(' + lang + ', \'License\', \'' + license_notice + '\')');
        if (license_url != '')
            write(fontvar_name + '.appendSFNTName(' + lang + ', \'License URL\', \'' + license_url + '\')');
        if (vendor_url != '')
            write(fontvar_name + '.appendSFNTName(' + lang + ', \'Vendor URL\', \'' + vendor_url + '\')');
        if (designer_name != '')
            write(fontvar_name + '.appendSFNTName(' + lang + ', \'Designer\', \'' + designer_name + '\')');
        if (designer_url != '')
            write(fontvar_name + '.appendSFNTName(' + lang + ', \'Designer URL\', \'' + designer_url + '\')');
    }

    void write_size_feature_code(string fontvar_name = 'my_font') {
        // TODO: Support for size ranges.

        if (0 < design_size)
            write('\n' + fontvar_name + '.size_feature = (' + format('%.1f', design_size) + ',)');
    }

    void write_glyphs_code(string fontvar_name = 'my_font',
                           string glyphvar_name = 'my_glyph',
                           string contourvar_name = 'my_contour',
                           string unicode_function = '') {
        if (unicode_function == '') {
            write('\nfrom sortsmill.glyphbuild import preferred_unicode');
            unicode_function = 'preferred_unicode';
        }
        for (Glyph glyph : glyph_list) {
            string uni = (glyph.unicode != undefined) ?
                format('%d', glyph.unicode) :
                unicode_function + '(\'' + glyph.name + '\')';
            write();
            write(glyphvar_name + ' = ' + fontvar_name + '.createChar(' + uni + ', \'' + glyph.name + '\')');
            write(glyphvar_name + '.unicode = ' + uni);
            glyph.write_fontforge_code(glyphvar_name, contourvar_name);
        }
        write('\n' + fontvar_name + '.encoding = \'UnicodeBMP\'');
    }

    void write_script_code() {
        write_script_header_code();
        write_font_open_code();
        write_font_names_code();
        write_size_feature_code();
        write_glyphs_code();
        write();
    }
};

//-------------------------------------------------------------------------
