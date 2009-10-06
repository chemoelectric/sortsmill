# -*- coding: utf-8 -*-

"""
Copyright (c) 2009 Barry Schwartz

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
"""

import fontforge
import spacing_by_anchors

def build_glyphs(bitbucket, f):

    import cap_spacing

    figures = ['zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine']

    caps = [
        'A', 'Agrave', 'Aacute', 'Acircumflex', 'Atilde', 'Adieresis', 'Aring', 'Amacron', 'Abreve', 'Aogonek',
        'B',
        'C', 'Ccedilla', 'Cacute', 'Ccircumflex', 'Cdotaccent', 'Ccaron',
        'D', 'Dcaron', 'Dcroat',
        'E', 'Egrave', 'Eacute', 'Ecircumflex', 'Edieresis', 'Emacron', 'Ebreve', 'Edotaccent', 'Eogonek', 'Ecaron',
        'F',
        'G', 'Gcircumflex', 'Gbreve', 'Gdotaccent', 'Gcommaaccent',
        'H', 'Hcircumflex', 'Hbar',
        'I', 'Igrave', 'Iacute', 'Icircumflex', 'Idieresis', 'Itilde', 'Imacron', 'Ibreve', 'Iogonek',
        'J', 'Jcircumflex',
        'K', 'Kcommaaccent',
        'L', 'Lacute', 'Lcommaaccent', 'Lcaron', 'Ldot', 'Lslash',
        'M',
        'N', 'Ntilde', 'Nacute', 'Ncommaaccent', 'Ncaron',
        'O', 'Ograve', 'Oacute', 'Ocircumflex', 'Otilde', 'Odieresis', 'Oslash', 'Omacron', 'Obreve', 'Ohungarumlaut',
        'P',
        'Q',
        'R', 'Racute', 'Rcommaaccent', 'Rcaron',
        'S', 'Sacute', 'Scircumflex', 'Scedilla', 'uni0218', 'Scaron',
        'T', 'uni0162', 'uni021A', 'Tcaron', 'Tbar',
        'U', 'Ugrave', 'Uacute', 'Ucircumflex', 'Udieresis', 'Utilde', 'Umacron', 'Ubreve', 'Uring', 'Uhungarumlaut', 'Uogonek',
        'V',
        'W', 'Wcircumflex',
        'X',
        'Y', 'Yacute', 'Ydieresis', 'Ycircumflex',
        'Z', 'Zacute', 'Zdotaccent', 'Zcaron',
        'IJ', 'AE', 'OE', 'Eth', 'Thorn',
        'question', 'questiondown',
        'exclam', 'exclamdown',
        'ampersand'
        ]

    def base(letter):
        if letter == 'i':
            base = 'dotlessi'
        elif letter == 'j':
            base = 'uni0237'
        else:
            base = letter
        return base    

    if f.persistent == None:
        f.persistent = {}
    f.persistent['spacing_anchor_heights'] = { 'hi' : 684, # caps and ascenders
#                                               't'  : ???, # top diacritics
                                               'x'  : 376,  # ex-height
                                               'o'  : 195,  # like the letter o
                                               'bl' : 15,   # baseline
                                               'lo' : -244 } # descenders

    build_several_space_glyphs(f, emsize = 1000, spacesize = 185,
                               thinspacesize = 1000 / 6,
                               hairspacesize = 1000 / 10,
                               tabwidth = f['zero.lining'].width)
    propagate_hyphens(f)
#    propagate_hyphens(f, '.uppercase')
    build_spacing_marks(f, width = 2 * 195)

    make_glyph_reference('quotesingle', f['minute'])
    make_glyph_reference('quotedbl', f['second'])
    make_glyph_reference('asciitilde', f['uni2053']) # Swung dash.
    make_glyph_reference('i.TRK', f['i'])
    make_glyph_reference('Dcroat', f['Eth'])

    build_multigraph('ellipsis', [f['period'], f['period'], f['period']])

    for fig in figures + ['dollar']:
        make_glyph_reference(fig, f[fig + '.hanging'])

    make_glyph_reference('uni00B9', f['one.sup'])
    make_glyph_reference('uni00B2', f['two.sup'])
    make_glyph_reference('uni00B3', f['three.sup'])
    for extension in [('.numer', 244), ('.sub', -98), ('.sup', 293)]:
        for fig in figures:
            make_glyph_reference(fig + extension[0],
                                 f[fig + '.denom'],
                                 transformation = (1, 0, 0, 1, 0, extension[1]),
                                 copy_spacing_anchors = False)
    build_multigraph('onequarter', [f['one.numer'], f['fraction'], f['four.denom']])
    build_multigraph('onehalf', [f['one.numer'], f['fraction'], f['two.denom']])
    build_multigraph('threequarters', [f['three.numer'], f['fraction'], f['four.denom']])

    #--------------------------------------------------------------------------

    for letter in 'GKkLlNnRr':
        build_accented_glyph(letter + 'commaaccent', f[base(letter)], f['uni0326'])
    for letter in 'NR':
        build_accented_glyph(letter + 'commaaccent.001', f[base(letter) + '.001'], f['uni0326'])
    build_accented_glyph('uni0218', f['S'], f['uni0326'])
    build_accented_glyph('uni0219', f['s'], f['uni0326'])
    build_accented_glyph('uni021A', f['T'], f['uni0326'])
    build_accented_glyph('uni021A.001', f['T.001'], f['uni0326'])
    build_accented_glyph('uni021A.002', f['T.002'], f['uni0326'])
    build_accented_glyph('uni021B', f['t'], f['uni0326'])
    build_accented_glyph('gcommaaccent', f['g'], f['uni0312'])
    build_accented_glyph('gcommaaccent.001', f['g.001'], f['uni0312'])

    #--------------------------------------------------------------------------

    for letter in 'CcSs':
        build_accented_glyph(letter + 'cedilla', f[base(letter)], f['uni0327'])
    build_accented_glyph('uni0162', f['T'], f['uni0327'])
    build_accented_glyph('uni0162.001', f['T.001'], f['uni0327'])
    build_accented_glyph('uni0162.002', f['T.002'], f['uni0327'])
    build_accented_glyph('uni0163', f['t'], f['uni0327'])

    #--------------------------------------------------------------------------

    for letter in 'aeiou':
        build_accented_glyph(letter + 'grave', f[base(letter)], f['gravecomb'])
    for letter in 'AEIOU':
        build_accented_glyph(letter + 'grave', f[base(letter)], f['gravecomb.cap'])
    for letter in 'A':
        build_accented_glyph(letter + 'grave.001', f[base(letter) + '.001'], f['gravecomb.cap'])

    #--------------------------------------------------------------------------

    for letter in 'aceinorsuyz':
        build_accented_glyph(letter + 'acute', f[base(letter)], f['acutecomb'])
    for letter in 'ACEILNORSUYZ':
        build_accented_glyph(letter + 'acute', f[base(letter)], f['acutecomb.cap'])
    for letter in 'ANR':
        build_accented_glyph(letter + 'acute.001', f[base(letter) + '.001'], f['acutecomb.cap'])
    # build_accented_glyph('lacute', f['l'], f['acutecomb.cap']) <-- We are making this one by hand.

    #--------------------------------------------------------------------------

    for letter in 'ainou':
        build_accented_glyph(letter + 'tilde', f[base(letter)], f['tildecomb'])
    for letter in 'AINOU':
        build_accented_glyph(letter + 'tilde', f[base(letter)], f['tildecomb.cap'])
    for letter in 'AN':
        build_accented_glyph(letter + 'tilde.001', f[base(letter) + '.001'], f['tildecomb.cap'])

    #--------------------------------------------------------------------------

    for letter in 'aeouy':
        build_accented_glyph(letter + 'dieresis', f[base(letter)], f['uni0308'])
    for letter in 'AEIOUY':
        build_accented_glyph(letter + 'dieresis', f[base(letter)], f['uni0308.cap'])
    for letter in 'A':
        build_accented_glyph(letter + 'dieresis.001', f[base(letter) + '.001'], f['uni0308.cap'])
    for letter in 'i':
        build_accented_glyph(letter + 'dieresis', f[base(letter)], f['uni0308'])

    #--------------------------------------------------------------------------

    for letter in 'au':
        build_accented_glyph(letter + 'ring', f[base(letter)], f['uni030A'])
    for letter in 'AU':
        build_accented_glyph(letter + 'ring', f[base(letter)], f['uni030A.cap'])
    for letter in 'A':
        build_accented_glyph(letter + 'ring.001', f[base(letter) + '.001'], f['uni030A.cap'])

    #--------------------------------------------------------------------------

    for letter in 'acegijosuwy':
        build_accented_glyph(letter + 'circumflex', f[base(letter)], f['uni0302'])
    for letter in 'g':
        build_accented_glyph(letter + 'circumflex.001', f[base(letter) + '.001'], f['uni0302'])
    for letter in 'hACEGHIJOSUWY':
        build_accented_glyph(letter + 'circumflex', f[base(letter)], f['uni0302.cap'])
    for letter in 'A':
        build_accented_glyph(letter + 'circumflex.001', f[base(letter) + '.001'], f['uni0302.cap'])
    for letter in ['f_h', 'f_f_h']:
        build_accented_glyph(letter + 'circumflex', f[base(letter)], f['uni0302.cap'])

    #--------------------------------------------------------------------------

    for letter in 'aegiou':
        build_accented_glyph(letter + 'breve', f[base(letter)], f['uni0306'])
    for letter in 'g':
        build_accented_glyph(letter + 'breve.001', f[base(letter) + '.001'], f['uni0306'])
    for letter in 'AEGIOU':
        build_accented_glyph(letter + 'breve', f[base(letter)], f['uni0306.cap'])
    for letter in 'A':
        build_accented_glyph(letter + 'breve.001', f[base(letter) + '.001'], f['uni0306.cap'])

    #--------------------------------------------------------------------------

    for letter in 'cegz':
        build_accented_glyph(letter + 'dotaccent', f[base(letter)], f['uni0307'])
    for letter in 'g':
        build_accented_glyph(letter + 'dotaccent.001', f[base(letter) + '.001'], f['uni0307'])
    for letter in 'CEGIZ':
        build_accented_glyph(letter + 'dotaccent', f[base(letter)], f['uni0307.cap'])
    build_accented_glyph('i', f['dotlessi'], f['uni0307'])
    build_accented_glyph('iogonek', f['iogonek.dotless'], f['uni0307'])
    build_accented_glyph('j', f['uni0237'], f['uni0307'])

    #--------------------------------------------------------------------------

    for letter in 'cenrsz':
        build_accented_glyph(letter + 'caron', f[base(letter)], f['uni030C'])
    for letter in 'CDENRTSZ':
        build_accented_glyph(letter + 'caron', f[base(letter)], f['uni030C.cap'])
    for letter in 'NRT':
        build_accented_glyph(letter + 'caron.001', f[base(letter) + '.001'], f['uni030C.cap'])
    for letter in 'T':
        build_accented_glyph(letter + 'caron.002', f[base(letter) + '.002'], f['uni030C.cap'])
    for letter in 'dLlt':
        build_accented_glyph(letter + 'caron', f[base(letter)], f['uni0315'])

    #--------------------------------------------------------------------------

    for letter in 'aeiou':
        build_accented_glyph(letter + 'macron', f[base(letter)], f['uni0304'])
    for letter in 'AEIOU':
        build_accented_glyph(letter + 'macron', f[base(letter)], f['uni0304.cap'])
    for letter in 'A':
        build_accented_glyph(letter + 'macron.001', f[base(letter) + '.001'], f['uni0304.cap'])

    #--------------------------------------------------------------------------

    for letter in 'ou':
        build_accented_glyph(letter + 'hungarumlaut', f[base(letter)], f['uni030B'])
    for letter in 'OU':
        build_accented_glyph(letter + 'hungarumlaut', f[base(letter)], f['uni030B.cap'])

    #--------------------------------------------------------------------------

#    build_multigraph('quotedblleft', [f['quoteleft'], f['quoteleft']])
#    build_multigraph('quotedblright', [f['quoteright'], f['quoteright']])
#    build_multigraph('quotedblbase', [f['quotesinglbase'], f['quotesinglbase']])
#    build_multigraph('uni201F', [f['quotereversed'], f['quotereversed']])
#    build_multigraph('guillemotleft', [f['guilsinglleft'], f['guilsinglleft']])
#    build_multigraph('guillemotright', [f['guilsinglright'], f['guilsinglright']])
    build_multigraph('napostrophe', [f['quoteright'], f['n']])
    build_multigraph('IJ', [f['I'], f['J']])
    build_multigraph('ij', [f['i'], f['j']])
    build_multigraph('Ldot', [f['L'], f['periodcentered']])
    build_multigraph('ldot', [f['l'], f['periodcentered']])

    #--------------------------------------------------------------------------

    f.selection.all()
    spacing_by_anchors.space_selected_by_anchors(f)
    f.selection.none()

    rules = cap_spacing.cap_spacing(f, caps, 0.015)
    cpsp = open('LindenHill-Italic_cpsp.fea', 'w')
    print >> cpsp, 'feature cpsp {'
    print >> cpsp, rules,
    print >> cpsp, '} cpsp;'
    cpsp.close()

    generate_kerning_and_read_features(None, f)

    #--------------------------------------------------------------------------
