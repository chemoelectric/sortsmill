// Intersection and overlap operations on lists of cyclic paths.
//
//
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

from sortsmill_orientation access is_in_interior, normalize_orientations;

real overlap_fuzz = 0.001;

//-------------------------------------------------------------------------

struct Intersection {
    path path1;
    real time1;
    path path2;
    real time2;

    bool is_null() {
        return path1 == nullpath || path2 == nullpath;
    }
};

Intersection null_Intersection;

struct Subpath {
    path path;
    Intersection first_point;
    Intersection last_point;

    void reverse() {
        this.path = reverse(path);
        Intersection temp = this.first_point;
        this.first_point = this.last_point;
        this.last_point = temp;
    }

    bool is_cyclic() {
        return first_point.is_null();
    }

    bool is_appendable(Subpath sp) {
        return this.last_point == sp.first_point || this.last_point == sp.last_point;
    }

    void append_if_possible(Subpath sp) {
        if (sp != this) {
            if (sp.first_point == this.last_point) {
                this.path = this.path & sp.path;
                this.last_point = sp.last_point;
            } else if (sp.last_point == this.last_point) {
                this.path = this.path & reverse(sp.path);
                this.last_point = sp.first_point;
            }
        }
        if (this.last_point == this.first_point) {
            this.path = this.path & cycle;
            this.first_point = null_Intersection;
            this.last_point = null_Intersection;
        }
    }
}

void write(Intersection xs)
{
    if (xs.is_null()) {
        write('<Intersection null>', none);
    } else {
        write('<Intersection ', none);
        write(point(xs.path1, xs.time1), none);
        write(format(', <path1>: %f', xs.time1), none);
        write(format(', <path2>: %f', xs.time2), none);
        write('>');
    }
}

void write(Intersection[] xsect)
{
    for (int i = 0; i < xsect.length; ++i) {
        write(format('%d: ', i), none);
        write(xsect[i]);
    }
}

Intersection[] find_intersections(path[] paths1, path[] paths2, real fuzz = overlap_fuzz)
{
    Intersection[] xsect;

    for (path p : paths1) {
        for (path q : paths2) {
            real[][] times_list = intersections(p, q, fuzz);
            for (real[] times : times_list) {
                Intersection xs;
                xs.path1 = p;
                xs.time1 = times[0];
                xs.path2 = q;
                xs.time2 = times[1];
                xsect.push(xs);
            }
        }
    }
    return xsect;
}

Intersection[] filter_by_path1(Intersection[] xsect, path p)
{
    Intersection[] new_xsect;
    for (Intersection xs : xsect)
        if (xs.path1 == p)
            new_xsect.push(xs);
    return new_xsect;
}

Intersection[] filter_by_path2(Intersection[] xsect, path p)
{
    Intersection[] new_xsect;
    for (Intersection xs : xsect)
        if (xs.path2 == p)
            new_xsect.push(xs);
    return new_xsect;
}

bool time1_less (Intersection xs1, Intersection xs2)
{
    return xs1.time1 < xs2.time1;
}

bool time2_less (Intersection xs1, Intersection xs2)
{
    return xs1.time2 < xs2.time2;
}

Intersection[] sort_by_time1(Intersection[] xsect)
{
    return sort(xsect, time1_less);
}

Intersection[] sort_by_time2(Intersection[] xsect)
{
    return sort(xsect, time2_less);
}

Subpath[] split_path_by_time1(path p, Intersection[] xsect)
{
    Subpath[] split;
    xsect = sort_by_time1(filter_by_path1(xsect, p));
    if (xsect.length == 0) {
        Subpath sp;
        sp.path = p;
        sp.first_point = null_Intersection;
        sp.last_point = null_Intersection;
        split.push(sp);
    } else {
        for (int i = 0; i + 1 < xsect.length; ++i) {
            Subpath sp;
            sp.path = subpath(p, xsect[i].time1, xsect[i + 1].time1);
            sp.first_point = xsect[i];
            sp.last_point = xsect[i + 1];
            split.push(sp);
        }
        Subpath sp;
        sp.path = subpath(p, xsect[xsect.length - 1].time1, xsect[0].time1 + size(p));
        sp.first_point = xsect[xsect.length - 1];
        sp.last_point = xsect[0];
        split.push(sp);
    }
    return split;
}

Subpath[] split_path_by_time2(path p, Intersection[] xsect)
{
    Subpath[] split;
    xsect = sort_by_time2(filter_by_path2(xsect, p));
    if (xsect.length == 0) {
        Subpath sp;
        sp.path = p;
        sp.first_point = null_Intersection;
        sp.last_point = null_Intersection;
        split.push(sp);
    } else {
        for (int i = 0; i + 1 < xsect.length; ++i) {
            Subpath sp;
            sp.path = subpath(p, xsect[i].time2, xsect[i + 1].time2);
            sp.first_point = xsect[i];
            sp.last_point = xsect[i + 1];
            split.push(sp);
        }
        Subpath sp;
        sp.path = subpath(p, xsect[xsect.length - 1].time2, xsect[0].time2 + size(p));
        sp.first_point = xsect[xsect.length - 1];
        sp.last_point = xsect[0];
        split.push(sp);
    }
    return split;
}

Subpath[] path1_subpaths(path[] paths, Intersection[] xsect)
{
    Subpath[] subpaths;
    for (path p : paths)
        subpaths.append(split_path_by_time1(p, xsect));
    return subpaths;
}

Subpath[] path2_subpaths(path[] paths, Intersection[] xsect)
{
    Subpath[] subpaths;
    for (path p : paths)
        subpaths.append(split_path_by_time2(p, xsect));
    return subpaths;
}

Subpath[] apply_punch_and_output_Subpath_list(path[] punch, path[] target)
{
    Subpath[] subpaths;
    Intersection[] xsect = find_intersections(punch, target);
    for (Subpath sp : path2_subpaths(target, xsect))
        if (!is_in_interior(midpoint(sp.path), punch))
            subpaths.push(sp);
     for (Subpath sp : path1_subpaths(punch, xsect))
         if (is_in_interior(midpoint(sp.path), target))
            subpaths.push(sp);
   return subpaths;
}

Subpath[] join_subpaths(Subpath[] subpaths)
{
    Subpath[] paths;
    int[] k = subpaths.keys;
    while (0 < k.length) {
        Subpath sp = subpaths[k[0]];
        if (sp.is_cyclic()) {
            k.delete(0);
        } else {
            while (!sp.is_cyclic()) {
                int i = 0;
                while (!sp.is_appendable(subpaths[k[i]]))
                    ++i;
                sp.append_if_possible(subpaths[k[i]]);
                k.delete(i);
            }
        }
        paths.push(sp);
    }
    return paths;
}

path[] apply_punch(path[] punch, path[] target)
// Remove parts of the target that are overlapped by parts of the punch.
{
    path[] paths;
    Subpath[] subpaths = apply_punch_and_output_Subpath_list(punch, target);
    subpaths = join_subpaths(subpaths);
    for (Subpath sp : subpaths)
        paths.push(sp.path);
    return normalize_orientations(paths);
}

//-------------------------------------------------------------------------
