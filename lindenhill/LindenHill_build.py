# -*- coding: utf-8 -*-

"""
Copyright (c) 2010 Barry Schwartz

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
import string
from sortsmill import font_db
from sortsmill.glyphbuild import *
from sortsmill.spacing_by_anchors import *

def build_glyphs(bitbucket, f):

    from sortsmill import cap_spacing

    figures = ['zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine']

    def base(letter):
        if letter == 'i':
            base = 'dotlessi'
        elif letter == 'j':
            base = 'uni0237'
        else:
            base = letter
        return base    

    db = font_db.db_create(f)

    db['spacing_anchor_heights'] = { 'hi' : 684, # caps and ascenders
                                     'x'  : 376,  # ex-height
                                     'o'  : 195,  # like the letter o
                                     'bl' : 15,   # baseline
                                     'lo' : -244 } # descenders

    all_glyphs = set(f) - set(['.notdef'])
    (smallcaps, capssmall, uppercase, lowercase, fraction_bar, numerators, denominators, remaining) = \
        tuple(separate_strings(all_glyphs, [
                (lambda s: s[-3:] == '.sc'),
                (lambda s: s[-3:] == '.c2'),
                (lambda s: is_uppercase(s, last_name)),
                (lambda s: is_lowercase(s, last_name)),
                (lambda s: s == 'fraction'),
                (lambda s: s[-6:] == '.numer'),
                (lambda s: s[-6:] == '.denom'),
                ]))
    db["kerning_sets"] = [
        (remaining, uppercase | lowercase | smallcaps | capssmall | remaining),
        (uppercase, uppercase | lowercase | smallcaps | remaining),
        (smallcaps, uppercase | smallcaps | capssmall | remaining),
        (lowercase, uppercase | lowercase | remaining),
        (numerators, fraction_bar),
        (fraction_bar, denominators),
        ]
    db['kerning_rounding'] = '(lambda x: int(round(x/5.0)) * 5)'
#    db['kerning_rounding'] = '(lambda x: x if abs(x) < 10 else int(round(x/5.0))*5)'

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
#    make_glyph_reference('i.TRK', f['i']) <-- Handled below.
    make_glyph_reference('Dcroat', f['Eth'])
    make_glyph_reference('dcroat.sc', f['eth.sc'])
    make_glyph_reference('L.CAT', f['L'])
    make_glyph_reference('l.CAT', f['l'])
    make_glyph_reference('L.CAT.c2', f['L.c2'])
    make_glyph_reference('l.CAT.sc', f['l.sc'])

    build_multigraph('ellipsis', [f['period'], f['period'], f['period']])

    for fig in figures + ['dollar']:
        make_glyph_reference(fig, f[fig + '.hanging'])

    make_glyph_reference('uni00B9', f['one.sup'])
    make_glyph_reference('uni00B2', f['two.sup'])
    make_glyph_reference('uni00B3', f['three.sup'])
    for extension in [('.numer', 244), ('.sub', -180), ('.sup', 244)]:
        for fig in figures:
            make_glyph_reference(fig + extension[0],
                                 f[fig + '.denom'],
                                 transformation = (1, 0, 0, 1, 0, extension[1]),
                                 copy_spacing_anchors = (extension[0] == '.numer'))
    build_multigraph('onequarter', [f['one.numer'], f['fraction'], f['four.denom']])
    build_multigraph('onehalf', [f['one.numer'], f['fraction'], f['two.denom']])
    build_multigraph('threequarters', [f['three.numer'], f['fraction'], f['four.denom']])

    for g in f:
        if g[-3:] == '.sc' and g not in ['i.TRK.sc', 'l.CAT.sc']:
            if g == 'periodcentered.sc':
                make_glyph_reference(g[:-3] + '.c2', f[g])
            elif g == 'uni0163.sc':
                make_glyph_reference('uni0162.c2', f[g])
            elif g == 'uni0219.sc':
                make_glyph_reference('uni0218.c2', f[g])
            elif g == 'uni021B.sc':
                make_glyph_reference('uni021A.c2', f[g])
            elif g in ('ae.sc', 'oe.sc', 'ij.sc'):
                make_glyph_reference(g[:-3].upper() + '.c2', f[g])
            else:
                make_glyph_reference(g[:-3].capitalize() + '.c2', f[g])

    #--------------------------------------------------------------------------

    for letter in 'GKkLlNnRr':
                build_accented_glyph(letter + 'commaaccent', f[base(letter)], f['uni0326'])
    build_accented_glyph('gcommaaccent', f['g'], f['uni0312'])
    build_accented_glyph('uni0218', f['S'], f['uni0326'])
    build_accented_glyph('uni0219', f['s'], f['uni0326'])
    build_accented_glyph('uni021A', f['T'], f['uni0326'])
    build_accented_glyph('uni021B', f['t'], f['uni0326'])

    for letter in 'gklnr':
        build_accented_glyph(letter + 'commaaccent.sc', f[letter + '.sc'], f['uni0326'])
    build_accented_glyph('uni0219.sc', f['s.sc'], f['uni0326'])
    build_accented_glyph('uni021B.sc', f['t.sc'], f['uni0326'])

    #--------------------------------------------------------------------------

    for letter in 'CcSs':
        build_accented_glyph(letter + 'cedilla', f[base(letter)], f['uni0327'])
        remove_overlap(f[letter + 'cedilla'])
    build_accented_glyph('uni0162', f['T'], f['uni0327'])
    remove_overlap(f['uni0162'])
    build_accented_glyph('uni0163', f['t'], f['uni0327'])
    remove_overlap(f['uni0163'])

    for letter in 'cs':
        build_accented_glyph(letter + 'cedilla.sc', f[letter + '.sc'], f['uni0327'])
        remove_overlap(f[letter + 'cedilla.sc'])
    build_accented_glyph('uni0163.sc', f['t.sc'], f['uni0327'])
    remove_overlap(f['uni0163.sc'])

    #--------------------------------------------------------------------------

    for letter in 'aeiou':
        build_accented_glyph(letter + 'grave', f[base(letter)], f['gravecomb'])
    for letter in 'AEIOU':
        build_accented_glyph(letter + 'grave', f[base(letter)], f['gravecomb.cap'])
    for letter in 'aeiou':
        build_accented_glyph(letter + 'grave.sc', f[letter + '.sc'], f['gravecomb'])

    #--------------------------------------------------------------------------

    for letter in 'aceinorsuyz':
        build_accented_glyph(letter + 'acute', f[base(letter)], f['acutecomb'])
    for letter in 'ACEILNORSUYZ':
        build_accented_glyph(letter + 'acute', f[base(letter)], f['acutecomb.cap'])
    # build_accented_glyph('lacute', f['l'], f['acutecomb.cap']) <-- We are making this one by hand.
    for letter in 'aceilnorsuyz':
        build_accented_glyph(letter + 'acute.sc', f[letter + '.sc'], f['acutecomb'])

    #--------------------------------------------------------------------------

    for letter in 'ainou':
        build_accented_glyph(letter + 'tilde', f[base(letter)], f['tildecomb'])
    for letter in 'AINOU':
        build_accented_glyph(letter + 'tilde', f[base(letter)], f['tildecomb.cap'])
    for letter in 'ainou':
        build_accented_glyph(letter + 'tilde.sc', f[letter + '.sc'], f['tildecomb'])

    #--------------------------------------------------------------------------

    for letter in 'aeouy':
        build_accented_glyph(letter + 'dieresis', f[base(letter)], f['uni0308'])
    for letter in 'AEIOUY':
        build_accented_glyph(letter + 'dieresis', f[base(letter)], f['uni0308.cap'])
    for letter in 'aeiouy':
        build_accented_glyph(letter + 'dieresis.sc', f[letter + '.sc'], f['uni0308'])
    for letter in 'i':
        build_accented_glyph(letter + 'dieresis', f[base(letter)], f['uni0308.narrow'])

    #--------------------------------------------------------------------------

    for letter in 'au':
        build_accented_glyph(letter + 'ring', f[base(letter)], f['uni030A'])
    for letter in 'AU':
        build_accented_glyph(letter + 'ring', f[base(letter)], f['uni030A.cap'])
    for letter in 'au':
        build_accented_glyph(letter + 'ring.sc', f[letter + '.sc'], f['uni030A'])

    #--------------------------------------------------------------------------

    for letter in 'acegijosuwy':
        build_accented_glyph(letter + 'circumflex', f[base(letter)], f['uni0302'])
    for letter in 'hACEGHIJOSUWY':
        build_accented_glyph(letter + 'circumflex', f[base(letter)], f['uni0302.cap'])
    for letter in ['f_h', 'f_f_h']:
        build_accented_glyph(letter + 'circumflex', f[base(letter)], f['uni0302.cap'])
    for letter in 'aceghijosuwy':
        build_accented_glyph(letter + 'circumflex.sc', f[letter + '.sc'], f['uni0302'])

    #--------------------------------------------------------------------------

    for letter in 'aegiou':
        build_accented_glyph(letter + 'breve', f[base(letter)], f['uni0306'])
    for letter in 'AEGIOU':
        build_accented_glyph(letter + 'breve', f[base(letter)], f['uni0306.cap'])
    for letter in 'aegiou':
        build_accented_glyph(letter + 'breve.sc', f[letter + '.sc'], f['uni0306'])

    #--------------------------------------------------------------------------

    for letter in 'cegz':
        build_accented_glyph(letter + 'dotaccent', f[base(letter)], f['uni0307'])
    for letter in 'CEGIZ':
        build_accented_glyph(letter + 'dotaccent', f[base(letter)], f['uni0307.cap'])
    for letter in 'cegz':
        build_accented_glyph(letter + 'dotaccent.sc', f[letter + '.sc'], f['uni0307'])
    build_accented_glyph('i.TRK', f['dotlessi'], f['uni0307'])
    build_accented_glyph('i.TRK.sc', f['i.sc'], f['uni0307'])
    build_accented_glyph('j.TRK', f['uni0237'], f['uni0307'])
    build_accented_glyph('i', f['dotlessi'], f['uni0307.high'])
    build_accented_glyph('iogonek', f['iogonek.dotless'], f['uni0307.high'])
    build_accented_glyph('j', f['uni0237'], f['uni0307.high'])

    #--------------------------------------------------------------------------

    for letter in 'cenrsz':
        build_accented_glyph(letter + 'caron', f[base(letter)], f['uni030C'])
    for letter in 'CDENRTSZ':
        build_accented_glyph(letter + 'caron', f[base(letter)], f['uni030C.cap'])
    for letter in 'dLlt':
        build_accented_glyph(letter + 'caron', f[base(letter)], f['uni0315'])

    for letter in 'cdenrstz':
        build_accented_glyph(letter + 'caron.sc', f[letter + '.sc'], f['uni030C'])
    build_accented_glyph('lcaron.sc', f['l.sc'], f['uni0315'])

    #--------------------------------------------------------------------------

    for letter in 'aeiou':
        build_accented_glyph(letter + 'macron', f[base(letter)], f['uni0304'])
    for letter in 'AEIOU':
        build_accented_glyph(letter + 'macron', f[base(letter)], f['uni0304.cap'])
    for letter in 'aeiou':
        build_accented_glyph(letter + 'macron.sc', f[letter + '.sc'], f['uni0304'])

    #--------------------------------------------------------------------------

    for letter in 'ou':
        build_accented_glyph(letter + 'hungarumlaut', f[base(letter)], f['uni030B'])
    for letter in 'OU':
        build_accented_glyph(letter + 'hungarumlaut', f[base(letter)], f['uni030B.cap'])
    for letter in 'ou':
        build_accented_glyph(letter + 'hungarumlaut.sc', f[letter + '.sc'], f['uni030B'])

    #--------------------------------------------------------------------------

    build_multigraph('napostrophe', [f['quoteright'], f['n']])
    build_multigraph('IJ', [f['I'], f['J']])
    build_multigraph('ij', [f['i'], f['j']])
    build_multigraph('ij.sc', [f['i.sc'], f['j.sc']])
#    build_multigraph('Ldot', [f['L'], f['periodcentered']]) # Done by hand.
    build_multigraph('ldot', [f['l'], f['periodcentered']])
#    build_multigraph('ldot.sc', [f['l.sc'], f['periodcentered.sc']]) # Done by hand.

    #--------------------------------------------------------------------------

    f.selection.all()
    space_selected_by_anchors(f)
    f.selection.none()

    generate_kerning_and_read_features(None, f)

    #--------------------------------------------------------------------------

    font_db.db_close(f)

    #--------------------------------------------------------------------------
