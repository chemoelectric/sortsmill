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
import psMat
import glyphbuild
import readfeatures
import sys
import math

###########################################################################
#
# Our principles of spacing and kerning:
#
#   1. Spacing is closely related to designing serifs.
#
#       1a. The designer’s control over spacing should
#           resemble control over the extents of serifs.
#
#   2. A certain part of the space around a glyph is
#      as if it were part of that glyph.
#
#       2a. Two glyphs may be brought together by kerning
#           only as much as it takes for their surrounding
#           spaces to touch.
#
###########################################################################


anchor_tolerance = 5


###########################################################################


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


def spacing_anchors_are_slanted(glyph):
    if glyph.persistent == None:
        glyph.persistent = {}
    if "spacing_anchors_slanted" not in glyph.persistent:
        glyph.persistent["spacing_anchors_slanted"] = False
    return glyph.persistent["spacing_anchors_slanted"]


def unslant_points_list(points_list, slant_angle, sp_offset):
    shear_factor = math.tan(math.radians(slant_angle))
    for i in range(0, len(points_list)):
        (name, base, x, y) = points_list[i]
        a_name = anchor_name(name)
        if a_name.is_spacing:
            dx = round(y * shear_factor)
            x += dx - sp_offset
            points_list[i] = (name, base, x, y)


def slant_points_list(points_list, slant_angle, sp_offset):
    shear_factor = math.tan(math.radians(slant_angle))
    for i in range(0, len(points_list)):
        (name, base, x, y) = points_list[i]
        a_name = anchor_name(name)
        if a_name.is_spacing:
            dx = round(y * shear_factor)
            x += -dx + sp_offset
            points_list[i] = (name, base, x, y)


def unslant_spacing_anchors(glyph):
    if spacing_anchors_are_slanted(glyph):
        points_list = list(glyph.anchorPoints)
        unslant_points_list(points_list, glyph.font.italicangle, spacing_offset(glyph.font))
        glyph.anchorPoints = tuple(points_list)
    glyph.persistent["spacing_anchors_slanted"] = False


def slant_spacing_anchors(glyph):
    if not spacing_anchors_are_slanted(glyph):
        points_list = list(glyph.anchorPoints)
        slant_points_list(points_list, glyph.font.italicangle, spacing_offset(glyph.font))
        glyph.anchorPoints = tuple(points_list)
    glyph.persistent["spacing_anchors_slanted"] = True


def left_signature(glyph, tolerance = anchor_tolerance):
    if spacing_anchors_are_slanted(glyph):
        shear_factor = math.tan(math.radians(glyph.font.italicangle))
    else:
        shear_factor = 0
    points = []
    x_min = sys.maxint
    for (name, kind, x, y) in glyph.anchorPoints:
        dx = round(y * shear_factor)
        sheared_x = x + dx
        a_name = anchor_name(name)
        if a_name.side == "l":
            if a_name.is_special_case:
                points.append((sheared_x, y, a_name.spacing_name))
            else:
                points.append((sheared_x, y, None))
                if not a_name.is_kerning_only:
                    x_min = min(x_min, sheared_x)
    if x_min == sys.maxint:
        points = ()
    else:
        points.sort(key = lambda (x, y, special_case_name): (y, x, special_case_name))
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
    if spacing_anchors_are_slanted(glyph):
        shear_factor = math.tan(math.radians(glyph.font.italicangle))
    else:
        shear_factor = 0
    points = []
    x_max = -sys.maxint
    for (name, kind, x, y) in glyph.anchorPoints:
        dx = round(y * shear_factor)
        sheared_x = x + dx
        a_name = anchor_name(name)
        if a_name.side == "r":
            if a_name.is_special_case:
                points.append((sheared_x, y, a_name.spacing_name))
            else:
                points.append((sheared_x, y, None))
                if not a_name.is_kerning_only:
                    x_max = max(x_max, sheared_x)
    if x_max == -sys.maxint:
        points = ()
    else:
        points.sort(key = lambda (x, y, special_case_name): (y, x, special_case_name))
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


def group_glyphs_by_signature(font, side, signature_function = None, tolerance = anchor_tolerance):
    assert side == "l" or side == "r"

    if signature_function == None:
        if side == "l":
            signature_function = left_signature
        else:
            signature_function = right_signature

    groups = {}
    for glyph_name in font:
        glyph = font[glyph_name]
        if (not (side == "r" and variant_root(glyph_name) in ("Ldot", "ldot"))
            and not glyphbuild.is_genuine_mark(glyph_name)):
            sig = signature_function(glyph, tolerance)
            if sig in groups:
                groups[sig].append(glyph_name)
            else:
                groups[sig] = [glyph_name]
    for sig in groups:
        groups[sig].sort()
    return groups


def name_of_group(group, side):
    assert len(group) != 0
    assert side == "l" or side == "r"

    if len(group) == 1:
        prefix = "\\"
    else:
        prefix = "@" + side + "_"
    return prefix + group[0]


def print_kerning_class_definitions(file, groups, side):
    assert side == "l" or side == "r"
    for sig in groups:
        group = groups[sig]
        if len(group) != 1:            
            file.write(name_of_group(group, side) + " = [")
            for i in range(0, len(group) - 1):
                file.write("\\" + group[i] + " ")
                if (i + 1) % 6 == 0:
                    print >> file
                    print >> file, "    ",
            file.write("\\" + group[-1] + "];")
            print >> file


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

    def notation(self, groups, side):
        if self.signature != None:
            n = name_of_group(groups[self.signature], side)
        else:
            n = "["
            for i in range(0, len(self.bunch) - 1):
                n += name_of_group(groups[self.bunch[i]], side) + " "
            n += name_of_group(groups[self.bunch[len(self.bunch) - 1]], side) + "]"
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
                    k = calculate_kerning(sig1, sig2,
                                          rounding_function = rounding_function)
                    cache[key] = k
            else:
                k = calculate_kerning(sig1, sig2, rounding_function)

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


def print_kerning_feature_file(file, font):

    if font.persistent == None:
        font.persistent = {}

    if "spacing_anchor_tolerance" not in font.persistent:
        font.persistent["spacing_anchor_tolerance"] = str(anchor_tolerance)
    if "kerning_rounding_function" not in font.persistent:
        font.persistent["kerning_rounding_function"] = "round"

    tolerance = eval(font.persistent["spacing_anchor_tolerance"])
    rounding_function = eval(font.persistent["kerning_rounding_function"])

    right_groups = group_glyphs_by_signature(font, "r", tolerance = tolerance)
    left_groups = group_glyphs_by_signature(font, "l", tolerance = tolerance)

    right_signatures = right_groups.keys()
    left_signatures = left_groups.keys()

    print >> file, "lookup generated_kerning {"
    print >> file
    print_kerning_class_definitions(file, right_groups, "r")
    print >> file
    print_kerning_class_definitions(file, left_groups, "l")
    print >> file

    if font.temporary == None:
        font.temporary = {}
    if "kern-cache" not in font.temporary:
        font.temporary["kern-cache"] = {}

    kernings = calculate_group_kernings(right_signatures, left_signatures,
                                        right_groups, left_groups,
                                        cache = font.temporary["kern-cache"],
                                        rounding_function = rounding_function,
                                        tolerance = tolerance)
    kernings = join_groups(kernings)
    for (obj1, obj2, k) in kernings:
        print >> file, "pos",
        print >> file, obj1.notation(right_groups, "r"),
        print >> file, obj2.notation(left_groups, "l"),
        print >> file, str(k) + ";"

    print >> file
    print >> file, "} generated_kerning;"
    print >> file
    print >> file, "feature kern {"
    print >> file, "  lookup generated_kerning;"
    print >> file, "} kern;"


###########################################################################


def copy_spacing_anchors(*glyphs):
    for glyph in glyphs:
        slant_angle = glyph.font.italicangle
        sp_offset = spacing_offset(glyph.font)
        if glyph.persistent == None:
            glyph.persistent = {}
        glyph.persistent["spacing_anchors_slanted"] = True
        anchors = {}
        for (glyph_name, (_, _, _, _, dx, dy)) in glyph.references:
            referenced_glyph = glyph.font[glyph_name]
            points_list = list(referenced_glyph.anchorPoints)
            if not spacing_anchors_are_slanted(referenced_glyph):
                slant_points_list(points_list, slant_angle, sp_offset)
            for (name, _, x, y) in points_list:
                a_name = anchor_name(name)
                if a_name.side == "l":
                    if name not in anchors or x + dx < anchors[name][0]:
                        anchors[name] = (x + dx, y + dy)
                elif a_name.side == "r":
                    if name not in anchors or anchors[name][0] < x + dx:
                        anchors[name] = (x + dx, y + dy)
        for name in anchors:
            (x, y) = anchors[name]
            glyphbuild.remove_anchor_point(glyph, name)
            glyph.anchorPoints += ((name, "base", x, y),)


###########################################################################


def spacing_offset(font):
    if font.persistent == None:
        font.persistent = {}
    try:
        offset = font.persistent["spacing_offset"]
        if offset == None:
            offset = 0
    except KeyError:
        offset = 0
    return offset


def left_spacing(glyph):
    if spacing_anchors_are_slanted(glyph):
        shear_factor = math.tan(math.radians(glyph.font.italicangle))
    else:
        shear_factor = 0
    spacing = None
    sheared_least_x = None
    for a in glyph.anchorPoints:
        a_name = anchor_name(a[0])
        x = a[2]
        dx = round(a[3] * shear_factor)
        if (a_name.side == "l"
            and not a_name.is_kerning_only
            and not a_name.is_special_case):
            if sheared_least_x == None or x + dx < sheared_least_x:
                sheared_least_x = x + dx
    if sheared_least_x != None and spacing_anchors_are_slanted(glyph):
        sheared_least_x -= spacing_offset(glyph.font)
    return sheared_least_x


def right_spacing(glyph):
    if spacing_anchors_are_slanted(glyph):
        shear_factor = math.tan(math.radians(glyph.font.italicangle))
    else:
        shear_factor = 0
    spacing = None
    sheared_greatest_x = None
    for a in glyph.anchorPoints:
        a_name = anchor_name(a[0])
        x = a[2]
        dx = round(a[3] * shear_factor)
        if (a_name.side == "r"
            and not a_name.is_kerning_only
            and not a_name.is_special_case):
            if sheared_greatest_x == None or sheared_greatest_x < x + dx:
                sheared_greatest_x = x + dx
    if sheared_greatest_x != None and spacing_anchors_are_slanted(glyph):
        sheared_greatest_x -= spacing_offset(glyph.font)
    return sheared_greatest_x


###########################################################################


def create_missing_anchor_classes(font, spacing_names):
    lookup_name = "spacing anchors"
    subtable_name = "spacing anchors-1"

    if lookup_name not in font.gpos_lookups:
        font.addLookup(lookup_name, "gpos_mark2base", (), ())
    if subtable_name not in font.getLookupSubtables(lookup_name):
        font.addLookupSubtable(lookup_name, subtable_name)

    for spc_name in spacing_names:
        for prefix in ("l;", "r;", "l;k;", "r;k;"):
            full_name = prefix + spc_name
            if full_name not in font.getLookupSubtableAnchorClasses(subtable_name):
                font.addAnchorClass(subtable_name, full_name)


def spacing_anchor_heights_are_given(font):
    return (font.persistent != None
            and "spacing_anchor_heights" in font.persistent
            and font.persistent["spacing_anchor_heights"] != None)


def there_is_such_an_anchor(glyph, side, spacing_name):
    for a in glyph.anchorPoints:
        a_name = anchor_name(a[0])
        if a_name.side == side and a_name.spacing_name == spacing_name:
            return True
    return False


def populate_side_with_anchors(glyph, side, x_pos):

    font = glyph.font

    if font.persistent == None:
        font.persistent = {}
    heights = font.persistent["spacing_anchor_heights"]

    create_missing_anchor_classes(glyph.font, heights.keys())

    if heights != None:
        if spacing_anchors_are_slanted(glyph):
            shear_factor = math.tan(math.radians(glyph.font.italicangle))
        else:
            shear_factor = 0
        for spacing_name in heights:
            if not there_is_such_an_anchor(glyph, side, spacing_name):
                h = heights[spacing_name]
                dx = round(h * shear_factor)
                name = side + ";" + spacing_name
                if spacing_anchors_are_slanted(glyph):
                    x = x_pos - dx + spacing_offset(font)
                else:
                    x = x_pos - dx
                glyph.anchorPoints += ((name, "base", x, h),)


def populate_with_spacing_anchors(glyph):
    populate_side_with_anchors(glyph, "l", 0)
    populate_side_with_anchors(glyph, "r", glyph.width)


###########################################################################


def glyph_has_spacing_anchors(bitbucket, glyph):
    return left_spacing(glyph) != None and right_spacing(glyph) != None


def space_glyph_by_anchors(bitbucket, glyph):
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
        if not glyphbuild.is_genuine_mark(glyph.glyphname):
            space_glyph_by_anchors(None, glyph)


def generate_kerning_feature_file(suffix, font):
    if suffix == None:
        suffix = "_generated_kerning.fea"
    f = open(font.fontname + suffix, "w")
    print_kerning_feature_file(f, font)
    f.close()


def generate_kerning_and_read_features(suffix, font):
    generate_kerning_feature_file(suffix, font)
    readfeatures.erase_and_read_features(None, font)


def toggle_spacing_anchor_angle(glyph):
    if glyph.persistent == None:
        glyph.persistent = {}
    if spacing_anchors_are_slanted(glyph):
        unslant_spacing_anchors(glyph)
    else:
        slant_spacing_anchors(glyph)


def something_is_selected(bitbucket, font):
    for g in font.selection.byGlyphs:
        return True
    return False


fontforge.registerMenuItem(space_glyph_by_anchors,
                           glyph_has_spacing_anchors,
                           None, "Glyph", "None",
                           "Space by anchors")

fontforge.registerMenuItem((lambda _, glyph: populate_with_spacing_anchors(glyph)),
                           (lambda _, glyph: spacing_anchor_heights_are_given(glyph.font)),
                           None, "Glyph", "None",
                           "Populate with spacing anchors")

## This facility is confusing, error prone, and likely unnecessary.
## Instead of lining up signatures at an angle, better to imagine
## italics on rectangular bodies with chamfers and kerns, and line up
## signatures vertically
##
## More experience with slanted fonts is needed, however,
## before we remove this.
##
#fontforge.registerMenuItem((lambda _, glyph: toggle_spacing_anchor_angle(glyph)),
#                           (lambda _, glyph: glyph.font.italicangle != 0),
#                           None, "Glyph", "None",
#                           "Toggle spacing-anchor angle")


fontforge.registerMenuItem((lambda _, font: space_selected_by_anchors(font)),
                           something_is_selected, None, "Font", "None",
                           "Space selected glyphs by anchors")

fontforge.registerMenuItem(generate_kerning_feature_file,
                           None, None, "Font", "None",
                           "Generate kerning feature file")

fontforge.registerMenuItem(generate_kerning_and_read_features,
                           readfeatures.feature_file_exists, None,
                           "Font", "None",
                           "Generate kerning and read features")


###########################################################################
