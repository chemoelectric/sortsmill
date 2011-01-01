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

emsize = 1000
spacesize = 200

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

    db['spacing_anchor_heights'] = {
        'hi' : 650,             # caps and ascenders,
        'x'  : 380,             # ex-height
        'o'  : 200,             # like the letter o
        'bl' : 10,              # baseline
        'lo' : -260,            # descenders
        }

    all_glyphs = set(f) - set(['.notdef'])
    proportional_figures = ['zero.p', 'one.p', 'two.p', 'three.p', 'four.p', 'five.p', 'six.p', 'seven.p', 'eight.p', 'nine.p']
    (uppercase, lowercase, fraction_bar, numerators, denominators, remaining) = \
        tuple(separate_strings(all_glyphs, [
                (lambda s: is_uppercase(s, last_name)),
                (lambda s: is_lowercase(s, last_name) or s in proportional_figures),
                (lambda s: s == 'fraction'),
                (lambda s: s[-6:] == '.numer'),
                (lambda s: s[-6:] == '.denom'),
                ]))

    db["kerning_sets"] = [
        (remaining, uppercase | lowercase | remaining),
        (uppercase, uppercase | lowercase | remaining),
        (lowercase, uppercase | lowercase | remaining),
        (numerators, fraction_bar),
        (fraction_bar, denominators),
        ]

    build_several_space_glyphs(f, emsize = emsize, spacesize = spacesize,
                               thinspacesize = emsize / 6,
                               hairspacesize = emsize / 10,
                               tabwidth = f['zero'].width)

    propagate_hyphens(f)
    propagate_hyphens(f, '.u')
    build_spacing_marks(f, width = 2 * 200)

    make_glyph_reference('minute', f['quotesingle'])
    make_glyph_reference('second', f['quotedbl'])
    make_glyph_reference('asciitilde', f['uni2053']) # Swung dash.
    make_glyph_reference('i.TRK', f['i'])
    make_glyph_reference('L.CAT', f['L'])
    make_glyph_reference('l.CAT', f['l'])
    make_glyph_reference('Dcroat', f['Eth'])

    build_multigraph('ellipsis', [f['period'], f['period'], f['period']])

    for extension in [('.numer', 250), ('.sub', -150), ('.sup', 350)]:
        for fig in figures + ['comma', 'period', 'hyphen',
                              'parenleft', 'parenright',
                              'bracketleft', 'bracketright',
                              'dollar', 'cent',
                              ] + [string.ascii_lowercase[i] for i in range(0, 26)]:
            make_glyph_reference(fig + extension[0],
                                 f[fig + '.denom'],
                                 transformation = (1, 0, 0, 1, 0, extension[1]),
                                 copy_spacing_anchors = (extension[0] == '.numer'))

    make_glyph_reference('uni00B9', f['one.sup'])
    make_glyph_reference('uni00B2', f['two.sup'])
    make_glyph_reference('uni00B3', f['three.sup'])
    make_glyph_reference('ordfeminine', f['a.sup'])
    make_glyph_reference('ordmasculine', f['o.sup'])
    build_multigraph('onequarter', [f['one.numer'], f['fraction'], f['four.denom']], copy_spacing_anchors = False)
    build_multigraph('onehalf', [f['one.numer'], f['fraction'], f['two.denom']], copy_spacing_anchors = False)
    build_multigraph('threequarters', [f['three.numer'], f['fraction'], f['four.denom']], copy_spacing_anchors = False)

    #--------------------------------------------------------------------------

    for letter in 'GKkLlNnRr':
        build_accented_glyph(letter + 'commaaccent', f[base(letter)], f['uni0326'])
    build_accented_glyph('uni0218', f['S'], f['uni0326'])
    build_accented_glyph('uni0219', f['s'], f['uni0326'])
    build_accented_glyph('uni021A', f['T'], f['uni0326'])
    build_accented_glyph('uni021B', f['t'], f['uni0326'])
    build_accented_glyph('gcommaaccent', f['g'], f['uni0312'])

    #--------------------------------------------------------------------------

    for letter in 'CcSs':
        build_accented_glyph(letter + 'cedilla', f[base(letter)], f['uni0327'])
        remove_overlap(f[letter + 'cedilla'])
    build_accented_glyph('uni0162', f['T'], f['uni0327'])
    remove_overlap(f['uni0162'])
    build_accented_glyph('uni0163', f['t'], f['uni0327'])
    remove_overlap(f['uni0163'])

    #--------------------------------------------------------------------------

    for letter in 'aeiou':
        build_accented_glyph(letter + 'grave', f[base(letter)], f['gravecomb'])
    for letter in 'AEIOU':
        build_accented_glyph(letter + 'grave', f[base(letter)], f['gravecomb.cap'])

    #--------------------------------------------------------------------------

    for letter in 'aceinorsuyz':
        build_accented_glyph(letter + 'acute', f[base(letter)], f['acutecomb'])
    for letter in 'ACEILNORSUYZ':
        build_accented_glyph(letter + 'acute', f[base(letter)], f['acutecomb.cap'])
    build_accented_glyph('lacute', f['l'], f['acutecomb.cap'])

    #--------------------------------------------------------------------------

    for letter in 'ainou':
        build_accented_glyph(letter + 'tilde', f[base(letter)], f['tildecomb'])
    for letter in 'AINOU':
        build_accented_glyph(letter + 'tilde', f[base(letter)], f['tildecomb.cap'])

    #--------------------------------------------------------------------------

    for letter in 'aeouy':
        build_accented_glyph(letter + 'dieresis', f[base(letter)], f['uni0308'])
    for letter in 'AEIOUY':
        build_accented_glyph(letter + 'dieresis', f[base(letter)], f['uni0308.cap'])
    for letter in 'i':
        build_accented_glyph(letter + 'dieresis', f[base(letter)], f['uni0308.narrow'])

    #--------------------------------------------------------------------------

    for letter in 'au':
        build_accented_glyph(letter + 'ring', f[base(letter)], f['uni030A'])
    for letter in 'AU':
        build_accented_glyph(letter + 'ring', f[base(letter)], f['uni030A.cap'])

    #--------------------------------------------------------------------------

    for letter in 'acegijosuwy':
        build_accented_glyph(letter + 'circumflex', f[base(letter)], f['uni0302'])
    for letter in 'ACEGHIJOSUWYh':
        build_accented_glyph(letter + 'circumflex', f[base(letter)], f['uni0302.cap'])

    # A gift for Esperantists, although 'ffĥ' seems unlikely.
    for letter in ['f_h', 'f_f_h']:
        build_accented_glyph(letter + 'circumflex', f[base(letter)], f['uni0302.cap'])

    #--------------------------------------------------------------------------

    for letter in 'aegiou':
        build_accented_glyph(letter + 'breve', f[base(letter)], f['uni0306'])
    for letter in 'AEGIOU':
        build_accented_glyph(letter + 'breve', f[base(letter)], f['uni0306.cap'])

    #--------------------------------------------------------------------------

    for letter in 'cegz':
        build_accented_glyph(letter + 'dotaccent', f[base(letter)], f['uni0307'])
    for letter in 'CEGIZ':
        build_accented_glyph(letter + 'dotaccent', f[base(letter)], f['uni0307.cap'])

    # Extra dot accents for Old Irish.
    build_accented_glyph('uni1E02', f['B'], f['uni0307.cap'])
    build_accented_glyph('uni1E03', f['b'], f['uni0307.cap'])
    build_accented_glyph('uni1E0A', f['D'], f['uni0307.cap'])
    build_accented_glyph('uni1E0B', f['d'], f['uni0307.cap'])
    build_accented_glyph('uni1E1E', f['F'], f['uni0307.cap'])
    build_accented_glyph('uni1E1F', f['f'], f['uni0307.cap'])
    build_accented_glyph('uni1E22', f['H'], f['uni0307.cap'])
    build_accented_glyph('uni1E23', f['h'], f['uni0307.cap'])
    build_accented_glyph('uni1E40', f['M'], f['uni0307.cap'])
    build_accented_glyph('uni1E41', f['m'], f['uni0307'])
    build_accented_glyph('uni1E56', f['P'], f['uni0307.cap'])
    build_accented_glyph('uni1E57', f['p'], f['uni0307'])
    build_accented_glyph('uni1E60', f['S'], f['uni0307.cap'])
    build_accented_glyph('uni1E61', f['s'], f['uni0307'])
    build_accented_glyph('uni1E6A', f['T'], f['uni0307.cap'])
    build_accented_glyph('uni1E6B', f['t'], f['uni0307'])

    #--------------------------------------------------------------------------

    for letter in 'cenrsz':
        build_accented_glyph(letter + 'caron', f[base(letter)], f['uni030C'])
    for letter in 'CDENRTSZ':
        build_accented_glyph(letter + 'caron', f[base(letter)], f['uni030C.cap'])
    for letter in 'dLlt':
        build_accented_glyph(letter + 'caron', f[base(letter)], f['uni0315'])

    #--------------------------------------------------------------------------

    for letter in 'aeiou':
        build_accented_glyph(letter + 'macron', f[base(letter)], f['uni0304'])
    for letter in 'AEIOU':
        build_accented_glyph(letter + 'macron', f[base(letter)], f['uni0304.cap'])

    #--------------------------------------------------------------------------

    for letter in 'ou':
        build_accented_glyph(letter + 'hungarumlaut', f[base(letter)], f['uni030B'])
    for letter in 'OU':
        build_accented_glyph(letter + 'hungarumlaut', f[base(letter)], f['uni030B.cap'])

    #--------------------------------------------------------------------------

    build_multigraph('napostrophe', [f['quoteright'], f['n']])
    build_multigraph('ldot', [f['l'], f['periodcentered']])

    build_multigraph('IJ', [f['I'], f['J']])
    build_multigraph('ij', [f['i'], f['j']])

    #--------------------------------------------------------------------------

    f.selection.all()
    space_selected_by_anchors(f)
    f.selection.none()

    generate_kerning_and_read_features(None, f)

    #--------------------------------------------------------------------------

    font_db.db_close(f)

    #--------------------------------------------------------------------------
