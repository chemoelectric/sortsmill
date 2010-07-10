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
import glyphbuild
import math
import os
import psMat
import readfeatures
import sys

from . import font_db

###########################################################################
#
# Our principles of spacing and kerning:
#
#   1. Spacing is closely related to designing serifs.
#
#       1a. The designer’s control over spacing should
#           resemble control over the extents of serifs.
#
#       1b. Therefore we let the designer specify points
#           representing distances from the glyph.
#
#       1c. One point on the left and one on the right
#           will define the left and right side bearings,
#           respectively. (There is some arbitrariness
#           to the choice; kerning is more fundamental
#           than spacing.)
#
#   2. A certain part of the space around a glyph is
#      as if it were part of that glyph.
#
#       2a. Two glyphs may be brought together by kerning
#           only as much as it takes for their surrounding
#           spaces to touch.
#
#  An observation:
# 
#      Frederic Goudy's Kennerley Oldstyle fits tightly
#      with very little kerning; its spacing is its greatest
#      beauty. Perhaps, instead of viewing spacing by anchors
#      as the placement of "invisible serifs", one should view
#      spacing by anchors as marking off a space that, in
#      something like Kennerley, would be "claimed" by a part
#      of each letter. Notice, for instance, the bowing out of
#      the Kennerley v and the Kennerley w; in a more
#      conventional typeface, we may want to "claim" that
#      space with anchors indented inwards, rather than with
#      ink.
#
###########################################################################

#--------------------------------------------------------------------------

anchor_tolerance = 5

#--------------------------------------------------------------------------

class anchor_name:

    def __init__(self, name):
        self.full_name = name
        parts = self.full_name.split(";")
        modifiers = parts[:-1]

        self.spacing_name = parts[-1]

        self.is_spacing = False
        self.side = None
        self.is_kerning_only = False
        self.is_special_case = False
        if len(modifiers) != 0:
            self.is_spacing = modifiers[0] in "lr"
            if self.is_spacing:
                self.side = modifiers[0]
                self.is_kerning_only = ("k" in modifiers[1:])
                self.is_special_case = ("s" in modifiers[1:])

def left_signature(glyph, tolerance = anchor_tolerance):
    arbitrary_big_integer = sys.maxsize
    points = []
    x_min = arbitrary_big_integer
    for (name, kind, x, y) in glyph.anchorPoints:
        a_name = anchor_name(name)
        if a_name.side == "l":
            if a_name.is_special_case:
                points.append((x, y, a_name.spacing_name))
            else:
                points.append((x, y, None))
                if not a_name.is_kerning_only:
                    x_min = min(x_min, x)
    if x_min == arbitrary_big_integer:
        points = ()
    else:
        def key(p):
            (x, y, special_case_name) = p
            return (y, x, special_case_name)
        points.sort(key = key)
        for i in range(0, len(points)):
            points[i] = (points[i][0] - x_min, points[i][1], points[i][2])
        i = 1
        while i < len(points):
            if abs(points[i][1] - points[i - 1][1]) <= tolerance:
                if points[i][0] < points[i - 1][0]:
                    points = points[0:i - 1] + points[i:]
                else:
                    points = points[0:i] + points[i + 1:]
            i += 1
    return tuple(points)

def right_signature(glyph, tolerance = anchor_tolerance):
    arbitrary_big_integer = sys.maxsize
    points = []
    x_max = -arbitrary_big_integer
    for (name, kind, x, y) in glyph.anchorPoints:
        a_name = anchor_name(name)
        if a_name.side == "r":
            if a_name.is_special_case:
                points.append((x, y, a_name.spacing_name))
            else:
                points.append((x, y, None))
                if not a_name.is_kerning_only:
                    x_max = max(x_max, x)
    if x_max == -arbitrary_big_integer:
        points = ()
    else:
        def key(p):
            (x, y, special_case_name) = p
            return (y, x, special_case_name)
        points.sort(key = key)
        for i in range(0, len(points)):
            points[i] = (points[i][0] - x_max, points[i][1], points[i][2])
        i = 1
        while i < len(points):
            if abs(points[i][1] - points[i - 1][1]) <= tolerance:
                if points[i - 1][0] < points[i][0]:
                    points = points[0:i - 1] + points[i:]
                else:
                    points = points[0:i] + points[i + 1:]
            i += 1
    return tuple(points)

def group_glyphs_by_signature(font, side,
                              glyph_set = None,
                              include_marks = False,
                              signature_function = None,
                              tolerance = anchor_tolerance):
    assert side == "l" or side == "r"

    if glyph_set == None:
        glyph_set = set(font)

    if signature_function == None:
        if side == "l":
            signature_function = left_signature
        else:
            signature_function = right_signature

    groups = {}
    for glyph_name in set(glyph_set) - set([".notdef"]):
        glyph = font[glyph_name]
        if include_marks or not glyphbuild.is_mark(glyph_name):
            sig = signature_function(glyph, tolerance)
            if sig in groups:
                groups[sig].append(glyph_name)
            else:
                groups[sig] = [glyph_name]
    for sig in groups:
        groups[sig].sort()
    return groups

def name_of_group(group, side, subtable_ident):
    assert len(group) != 0
    assert side == "l" or side == "r"

    if len(group) == 1:
        prefix = "\\"
    else:
        prefix = "@" + side + str(subtable_ident) + "_"
    return prefix + group[0]

def print_kerning_class_definitions(file, groups, side, subtable_ident):
    assert side == "l" or side == "r"
    for sig in groups:
        group = groups[sig]
        if len(group) != 1:            
            file.write(name_of_group(group, side, subtable_ident) + " = [")
            for i in range(0, len(group) - 1):
                file.write("\\" + group[i] + " ")
                if (i + 1) % 6 == 0:
                    file.write("\n     ")
            file.write("\\" + group[-1] + "];\n")

def calculate_kerning(sig1, sig2, tolerance = anchor_tolerance,
                      rounding_function = round):

    def possible_kernings(sig1, sig2, tolerance = anchor_tolerance):
        possibles = []
        for p1 in sig1:
            for p2 in sig2:
                if p1[2] != None:
                    if p1[2] == p2[2]:
                        # A ‘special case’ anchor.
                        return [(p1, p2)]
                elif p2[2] == None and abs(p1[1] - p2[1]) <= tolerance:
                    possibles.append((p1, p2))
        return possibles

    possibles = possible_kernings(sig1, sig2, tolerance)
    kerning = None
    for (a, b) in possibles:
        k = int(rounding_function(a[0] - b[0]))
        if kerning == None or kerning < k:
            kerning = k
    return kerning

class kerning_object:

    def __init__(self, signature = None, bunch = None):
        assert signature != None or bunch != None
        assert signature == None or bunch == None
        self.signature = signature
        if bunch == None:
            self.bunch = None
        else:
            self.bunch = tuple(sorted(bunch))

    def __add__(self, obj):
        if self.bunch == None:
            new_obj = kerning_object(bunch = (self.signature,))
        else:
            new_obj = kerning_object(bunch = self.bunch)
        if obj.bunch == None:
            new_obj.bunch = list(new_obj.bunch) + [obj.signature]
        else:
            new_obj.bunch = list(new_obj.bunch) + list(obj.bunch)
        new_obj.bunch.sort()
        new_obj.bunch = tuple(new_obj.bunch)
        return new_obj

    def __eq__(self, obj):
        return self.signature == obj.signature and self.bunch == obj.bunch

    def __ne__(self, obj):
        return self.signature != obj.signature or self.bunch != obj.bunch

    def __cmp__(self, obj):
        result = 0
        if self.signature != None:
            if obj.signature != None:
                result = cmp(self.signature, obj.signature)
            else:
                result = -1
        elif obj.signature != None:
            result = 1
        else:
            result = cmp(self.signature, obj.signature)
        return result

    def __hash__(self):
        if self.signature != None:
            return hash(self.signature)
        else:
            return hash(self.bunch)

    def notation(self, groups, side, subtable_ident):
        if self.signature != None:
            n = name_of_group(groups[self.signature], side, subtable_ident)
        else:
            n = "["
            for i in range(0, len(self.bunch) - 1):
                n += name_of_group(groups[self.bunch[i]], side, subtable_ident) + " "
            n += name_of_group(groups[self.bunch[len(self.bunch) - 1]], side, subtable_ident) + "]"
        return n

    def graphite_notation(self, groups, side, subtable_ident):
        if self.signature != None:
            n = graphite_name_of_group(groups[self.signature], side, subtable_ident)
        else:
            n = "("
            for i in range(0, len(self.bunch) - 1):
                n += graphite_name_of_group(groups[self.bunch[i]], side, subtable_ident) + ", "
            n += graphite_name_of_group(groups[self.bunch[len(self.bunch) - 1]], side, subtable_ident) + ")"
        return n

    def is_singleton(self, groups, side):
        return self.signature != None and len(groups[self.signature]) == 1

def calculate_group_kernings(signatures1, signatures2, right_groups, left_groups,
                             cache = None, rounding_function = round,
                             tolerance = anchor_tolerance):
    kernings = []
    for sig1 in signatures1:
        for sig2 in signatures2:

            if cache != None:
                key = (sig1, sig2)
                try:
                    k = cache[key]
                except KeyError:
                    k = calculate_kerning(sig1, sig2, rounding_function = rounding_function)
                    cache[key] = k
            else:
                k = calculate_kerning(sig1, sig2, rounding_function = rounding_function)

            if k != None and k != 0:
                kernings.append((kerning_object(signature = sig1),
                                 kerning_object(signature = sig2), k))
    return kernings

def categorize_kernings(kernings, element_no):
    result = {}
    for k in kernings:
        try:
            result[k[element_no]].append(k)
        except KeyError:
            result[k[element_no]] = [k]
    return result

def join_groups(kernings):
    all_the_joined_kernings = []
    offsets = categorize_kernings(kernings, 2)
    for k in offsets:
        
        equal_obj2s = categorize_kernings(offsets[k], 1)
        semi_joined_kernings = []
        for obj2 in equal_obj2s:
            kerns = equal_obj2s[obj2]
            if len(kerns) == 1:
                semi_joined_kernings.append(kerns[0])
            else:
                joined_obj1 = kerns[0][0]
                for (o1, _, _) in kerns[1:]:
                    joined_obj1 += o1
                semi_joined_kernings.append((joined_obj1, obj2, k))
        
        equal_obj1s = categorize_kernings(semi_joined_kernings, 0)
        joined_kernings = []
        for obj1 in equal_obj1s:
            kerns = equal_obj1s[obj1]
            if len(kerns) == 1:
                joined_kernings.append(kerns[0])
            else:
                joined_obj2 = kerns[0][1]
                for (_, o2, _) in kerns[1:]:
                    joined_obj2 += o2
                joined_kernings.append((obj1, joined_obj2, k))

        all_the_joined_kernings += joined_kernings

    return all_the_joined_kernings

def calculate_kerning_subtables(font, db):

    tolerance = eval(db['spacing_anchor_tolerance'])
    rounding_function = eval(db['kerning_rounding'])

    subtables = []
    for (left_glyphs, right_glyphs) in db['kerning_sets']:

        # Note that right signatures are taken from "left_glyphs", and
        # left signatures are taken from "right_glyphs".
        right_groups = group_glyphs_by_signature(font, 'r', glyph_set = left_glyphs, tolerance = tolerance)
        left_groups = group_glyphs_by_signature(font, 'l', glyph_set = right_glyphs, tolerance = tolerance)

        right_signatures = right_groups.keys()
        left_signatures = left_groups.keys()
        
        kernings = calculate_group_kernings(right_signatures, left_signatures,
                                            right_groups, left_groups,
                                            cache = font.temporary['kern-cache'],
                                            rounding_function = rounding_function,
                                            tolerance = tolerance)
        kernings = join_groups(kernings)

        if kernings != []:
            subtables.append((right_groups, left_groups, kernings))

    return subtables

def print_kerning_feature_file(file, subtables):

    if subtables != []:

        file.write('feature kern {\n')

        for i in range(0, len(subtables)):
            (right_groups, left_groups, kernings) = subtables[i]
            print_kerning_class_definitions(file, right_groups, 'r', i + 1)
            file.write('\n')
            print_kerning_class_definitions(file, left_groups, 'l', i + 1)
            file.write('\n')

        for i in range(0, len(subtables)):
            if 0 < i:
                file.write('subtable;\n')
            (right_groups, left_groups, kernings) = subtables[i]
            for (obj1, obj2, k) in kernings:
                file.write('pos ')
                file.write(obj1.notation(right_groups, 'r', i + 1) + ' ')
                file.write(obj2.notation(left_groups, 'l', i + 1) + ' ')
                file.write(str(k) + ';\n')
            file.write('\n')
        file.write('} kern;\n')

def name_of_group(group, side, subtable_ident):
    assert len(group) != 0
    assert side == "l" or side == "r"

    if len(group) == 1:
        prefix = "\\"
    else:
        prefix = "@" + side + str(subtable_ident) + "_"
    return prefix + group[0]

def graphite_name_of_group(group, side, subtable_ident):
    assert len(group) != 0
    assert side == 'l' or side == 'r'

    if len(group) == 1:
        s = 'postscript("' + group[0] + '")'
    else:
        s = side + str(subtable_ident) + group[0].replace('.', '__')
    return s

def print_graphite_kerning_class_definitions(file, groups, side, subtable_ident):
    assert side == 'l' or side == 'r'
    for sig in groups:
        group = groups[sig]
        if len(group) != 1:
            file.write(graphite_name_of_group(group, side, subtable_ident) + ' = postscript(')
            for i in range(0, len(group) - 1):
                file.write('"' + group[i] + '", ')
                if (i + 1) % 6 == 0:
                    file.write('\n     ')
            file.write('"' + group[-1] + '")\n')

def print_kerning_graphite_file(file, subtables):

    if subtables != []:

        #
        # TODO: Clean out any unused class.
        #
        file.write('table(glyph)\n')
        for i in range(0, len(subtables)):
            (right_groups, left_groups, kernings) = subtables[i]
            print_graphite_kerning_class_definitions(file, right_groups, 'r', i + 1)
            file.write('\n')
            print_graphite_kerning_class_definitions(file, left_groups, 'l', i + 1)
            file.write('\n')
        file.write('endtable\n')

        file.write('\n')

        file.write('table(positioning)\n')
        for i in range(0, len(subtables)):
            (right_groups, left_groups, kernings) = subtables[i]
            for (obj1, obj2, k) in kernings:
                file.write(obj1.graphite_notation(right_groups, 'r', i + 1) + ' ')
                file.write(obj2.graphite_notation(left_groups, 'l', i + 1) + ' ')
                file.write('{ kern.x = ' + str(k) + 'm };\n')
        file.write('endtable\n')

def generate_kerning(font, feature_file_name, graphite_file_name):

    db = font_db.db_open(font, flag = 'c')

    font_db.db_init_binding(font, 'spacing_anchor_tolerance', str(anchor_tolerance))
    font_db.db_init_binding(font, 'kerning_rounding', 'round')
    font_db.db_init_binding(font, 'kerning_sets', [(set(font), set(font))])

    if font.temporary == None:
        font.temporary = {}
    if 'kern-cache' not in font.temporary or font.temporary['kern-cache'] == None:
        font.temporary['kern-cache'] = {}

    subtables = calculate_kerning_subtables(font, db)
    if feature_file_name:
        feature_file = open(feature_file_name, 'w')
        print_kerning_feature_file(feature_file, subtables)
        feature_file.close()
    if graphite_file_name:
        graphite_file = open(graphite_file_name, 'w')
        print_kerning_graphite_file(graphite_file, subtables)
        graphite_file.close()

    font_db.db_close(font)
    font_db.db_open(font)

#--------------------------------------------------------------------------

def copy_spacing_anchors(*glyphs):
    for glyph in glyphs:
        anchors = {}
        for (glyph_name, (_, _, _, _, dx, dy)) in glyph.references:
            referenced_glyph = glyph.font[glyph_name]
            points_list = list(referenced_glyph.anchorPoints)
            for (name, _, x, y) in points_list:
                a_name = anchor_name(name)
                if a_name.side == 'l':
                    if name not in anchors or x + dx < anchors[name][0]:
                        anchors[name] = (x + dx, y + dy)
                elif a_name.side == 'r':
                    if name not in anchors or anchors[name][0] < x + dx:
                        anchors[name] = (x + dx, y + dy)
        for name in anchors:
            (x, y) = anchors[name]
            glyphbuild.remove_anchor_point(glyph, name)
            glyph.anchorPoints += ((name, 'base', x, y),)

#--------------------------------------------------------------------------

def left_spacing(glyph):
    spacing = None
    least_x = None
    for a in glyph.anchorPoints:
        a_name = anchor_name(a[0])
        x = a[2]
        if (a_name.side == 'l'
            and not a_name.is_kerning_only
            and not a_name.is_special_case):
            if least_x == None or x < least_x:
                least_x = x
    return least_x

def right_spacing(glyph):
    spacing = None
    greatest_x = None
    for a in glyph.anchorPoints:
        a_name = anchor_name(a[0])
        x = a[2]
        if (a_name.side == 'r'
            and not a_name.is_kerning_only
            and not a_name.is_special_case):
            if greatest_x == None or greatest_x < x:
                greatest_x = x
    return greatest_x

#--------------------------------------------------------------------------

def create_missing_anchor_classes(font, spacing_names):
    lookup_name = 'spacing anchors'
    subtable_name = 'spacing anchors-1'

    if lookup_name not in font.gpos_lookups:
        font.addLookup(lookup_name, 'gpos_mark2base', (), ())
    if subtable_name not in font.getLookupSubtables(lookup_name):
        font.addLookupSubtable(lookup_name, subtable_name)

    for spc_name in spacing_names:
        for prefix in ('l;', 'r;', 'l;k;', 'r;k;'):
            full_name = prefix + spc_name
            if full_name not in font.getLookupSubtableAnchorClasses(subtable_name):
                font.addAnchorClass(subtable_name, full_name)

def spacing_anchor_heights_are_given(font):
    if os.path.exists(font_db.db_file_name(font)):
        db = font_db.db_open(font)
        result = 'spacing_anchor_heights' in db and db['spacing_anchor_heights'] != None
    else:
        result = False
    return result

def there_is_such_an_anchor(glyph, side, spacing_name):
    for a in glyph.anchorPoints:
        a_name = anchor_name(a[0])
        if a_name.side == side and a_name.spacing_name == spacing_name:
            return True
    return False

def populate_side_with_anchors(glyph, side, x):

    font = glyph.font
    db = font_db.db_open(font)
    heights = db['spacing_anchor_heights']

    create_missing_anchor_classes(glyph.font, heights.keys())

    if heights != None:
        for spacing_name in heights:
            if not there_is_such_an_anchor(glyph, side, spacing_name):
                h = heights[spacing_name]
                name = side + ';' + spacing_name
                glyph.anchorPoints += ((name, 'base', x, h),)

def populate_with_spacing_anchors(glyph):
    glyph.preserveLayerAsUndo()
    populate_side_with_anchors(glyph, 'l', 0)
    populate_side_with_anchors(glyph, 'r', glyph.width)

#--------------------------------------------------------------------------

def selected_spacing_anchors(glyph):
    selected_points = []
    for a in glyph.anchorPointsWithSel:
        if a[4]:
            a_name = anchor_name(a[0])
            if a_name.side and a_name.side in 'lr':
                selected_points.append(a)
    return selected_points

# TODO: Make this preserve modifiers other than 'k;'.
def set_spacing_anchors_read_only(glyph, kerning_only):
    glyph.preserveLayerAsUndo()
    anchor_points = []
    for a in glyph.anchorPointsWithSel:
        if a[4]:
            a_name = anchor_name(a[0])
            if a_name.side and a_name.side in 'lr':
                if kerning_only:
                    new_name = a_name.side + ';k;' + a_name.spacing_name
                else:
                    new_name = a_name.side + ';' + a_name.spacing_name
                anchor_points.append((new_name,) + a[1:])
            else:
                anchor_points.append(a)
        else:
            anchor_points.append(a)
    glyph.anchorPointsWithSel = anchor_points

def flip_spacing_anchors_left_right(glyph):
    glyph.preserveLayerAsUndo()
    anchor_points = []
    for a in glyph.anchorPointsWithSel:
        if a[4]:
            a_name = anchor_name(a[0])
            if a_name.side:
                if a_name.side in 'l':
                    new_name = 'r' + a_name.full_name[1:]
                    anchor_points.append((new_name,) + a[1:])
                elif a_name.side in 'r':
                    new_name = 'l' + a_name.full_name[1:]
                    anchor_points.append((new_name,) + a[1:])
                else:
                    anchor_points.append(a)
            else:
                anchor_points.append(a)
        else:
            anchor_points.append(a)
    glyph.anchorPointsWithSel = anchor_points

def spread_spacing_anchors(amount, glyph):
    glyph.preserveLayerAsUndo()
    none_selected = (selected_spacing_anchors(glyph) == [])
    anchor_points = []
    for a in glyph.anchorPointsWithSel:
        if none_selected or a[4]:
            a_name = anchor_name(a[0])
            if a_name.side:
                if a_name.side in 'l':
                    anchor_points.append(a[:2] + (a[2] - amount,) + a[3:])
                elif a_name.side in 'r':
                    anchor_points.append(a[:2] + (a[2] + amount,) + a[3:])
                else:
                    anchor_points.append(a)
            else:
                anchor_points.append(a)
        else:
            anchor_points.append(a)
    glyph.anchorPointsWithSel = anchor_points

def spread_spacing_anchors_of_selected_glyphs(amount, font):
    for glyph in font.selection.byGlyphs:
        spread_spacing_anchors(amount, glyph)

#--------------------------------------------------------------------------

def glyph_has_spacing_anchors(bitbucket, glyph = None):
    if glyph == None:
        glyph = bitbucket
    return left_spacing(glyph) != None and right_spacing(glyph) != None

def space_glyph_by_anchors(bitbucket, glyph = None):
    if glyph == None:
        glyph = bitbucket
    lspace = left_spacing(glyph)
    rspace = right_spacing(glyph)
    if lspace != None and rspace != None:
        for r in glyph.references:
            glyph.useRefsMetrics(r[0], False)
        glyph.transform((1, 0, 0, 1, -lspace, 0))
        glyph.width = rspace - lspace

        # Translate all references to this glyph, as well, but in the
        # opposite direction.
        if lspace != 0:
            f = glyph.font
            for glyph_name in f:
                g = f[glyph_name]
                references = list(g.references)
                refs_changed = False
                for i in range(0, len(references)):
                    if references[i][0] == glyph.glyphname:
                        references[i] = (references[i][0], psMat.compose(references[i][1], (1, 0, 0, 1, lspace, 0)))
                        refs_changed = True
                if refs_changed:
                    g.references = tuple(references)

def space_selected_by_anchors(font):
    for glyph in font.selection.byGlyphs:
        if not glyphbuild.is_mark(glyph.glyphname):
            space_glyph_by_anchors(None, glyph)

def generate_kerning_files(bitbucket, font):
    feature_file_name = font.fontname + '_generated_kerning.fea'
    graphite_file_name = font.fontname + '_generated_kerning.gdl'
    generate_kerning(font, feature_file_name, graphite_file_name)

def generate_kerning_and_read_features(bitbucket, font):
    generate_kerning_files(bitbucket, font)
    readfeatures.erase_and_read_features(None, font)

def something_is_selected(bitbucket, font = None):
    if font == None:
        font = bitbucket
    for g in font.selection.byGlyphs:
        return True
    return False

def clear_kern_cache(font):
    if kern_cache_has_contents(font):
        font.temporary['kern-cache'] = {}

def kern_cache_has_contents(font):
    return (font.temporary != None and
            'kern-cache' in font.temporary and
            font.temporary['kern-cache'] != None and
            font.temporary['kern-cache'] != {})

def clear_cached_data(font):
    clear_kern_cache(font)
    font_db.db_remove(font)

def cached_data_exists(font):
    return kern_cache_has_contents(font) or font_db.db_exists(font)

fontforge.registerMenuItem((lambda _, glyph: set_spacing_anchors_read_only(glyph, True)),
                           (lambda _, glyph: selected_spacing_anchors(glyph) != []),
                           None, 'Glyph', 'None',
                           'Make spacing anchors kerning-only')

fontforge.registerMenuItem((lambda _, glyph: set_spacing_anchors_read_only(glyph, False)),
                           (lambda _, glyph: selected_spacing_anchors(glyph) != []),
                           None, 'Glyph', 'None',
                           'Make spacing anchors non-kerning-only')

fontforge.registerMenuItem((lambda _, glyph: flip_spacing_anchors_left_right(glyph)),
                           (lambda _, glyph: selected_spacing_anchors(glyph) != []),
                           None, 'Glyph', 'None',
                           'Flip spacing anchors left-right')

fontforge.registerMenuItem(spread_spacing_anchors, glyph_has_spacing_anchors,
                           5, 'Glyph', 'None',
                           'Spread spacing anchors by 5')

fontforge.registerMenuItem(spread_spacing_anchors, glyph_has_spacing_anchors,
                           10, 'Glyph', 'None',
                           'Spread spacing anchors by 10')

fontforge.registerMenuItem(spread_spacing_anchors, glyph_has_spacing_anchors,
                           -5, 'Glyph', 'None',
                           'Spread spacing anchors by -5')

fontforge.registerMenuItem(spread_spacing_anchors, glyph_has_spacing_anchors,
                           -10, 'Glyph', 'None',
                           'Spread spacing anchors by -10')

fontforge.registerMenuItem(spread_spacing_anchors_of_selected_glyphs,
                           something_is_selected,
                           5, 'Font', 'None',
                           'Spread spacing anchors by 5')

fontforge.registerMenuItem(spread_spacing_anchors_of_selected_glyphs,
                           something_is_selected,
                           10, 'Font', 'None',
                           'Spread spacing anchors by 10')

fontforge.registerMenuItem(spread_spacing_anchors_of_selected_glyphs,
                           something_is_selected,
                           -5, 'Font', 'None',
                           'Spread spacing anchors by -5')

fontforge.registerMenuItem(spread_spacing_anchors_of_selected_glyphs,
                           something_is_selected,
                           -10, 'Font', 'None',
                           'Spread spacing anchors by -10')

fontforge.registerMenuItem(space_glyph_by_anchors,
                           glyph_has_spacing_anchors,
                           None, 'Glyph', 'None',
                           'Space by anchors')

fontforge.registerMenuItem((lambda _, glyph: populate_with_spacing_anchors(glyph)),
                           (lambda _, glyph: spacing_anchor_heights_are_given(glyph.font)),
                           None, 'Glyph', 'None',
                           'Populate with spacing anchors')

fontforge.registerMenuItem((lambda _, font: space_selected_by_anchors(font)),
                           something_is_selected, None, 'Font', 'None',
                           'Space selected glyphs by anchors')

fontforge.registerMenuItem(generate_kerning_files,
                           None, None, 'Font', 'None',
                           'Generate kerning files')

fontforge.registerMenuItem(generate_kerning_and_read_features,
                           readfeatures.feature_file_exists,
                           None, 'Font', 'None',
                           'Generate kerning and read features')

fontforge.registerMenuItem((lambda _, font: clear_cached_data(font)),
                           (lambda _, font: cached_data_exists(font)),
                           None, 'Font', 'None',
                           'Clear cached data')

#--------------------------------------------------------------------------
