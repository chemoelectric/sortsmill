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

//-------------------------------------------------------------------------

path chop(path p, path chop_shape)
// Be aware that this algorithm can produce a self-intersecting result
// path.
{
    real[][] xsect = intersections(p, chop_shape);
    path result = nullpath;
    int i = 0;
    while (i < xsect.length) {
        int i1 = (i + 1) % xsect.length;
        real t = xsect[i][0];
        real t1 = (i1 == 0 ? length(p) : 0) + xsect[i1][0];
        path q = subpath(p, t, t1);
        if (inside(chop_shape, midpoint(q))) {
            t = xsect[i][1];
            t1 = xsect[i1][1];
            result = result & subpath(chop_shape, t, t1);
        } else {
            result = result & q;
        }
        i += 1;
    }
    result = result & cycle;
    return result;
}  

path chop(path p, pair point, real angle)
// Chop along a straight line.
{
    real bignum = 1e6;
    path chop_shape = (bignum,0)--(bignum,bignum)--(-bignum,bignum)--(-bignum,0)--cycle;
    chop_shape = shift(point) * rotate(angle) * chop_shape;
    return chop(p, chop_shape);
}

//-------------------------------------------------------------------------

path reshape_subpath(path p, real start_time, real end_time, path new_subpath(path subpath))
{
    path p1 = new_subpath(subpath(p, start_time, end_time));
    path p2 = subpath(p, end_time, length(p) + start_time);
    path q = p1 & p2;
    return cyclic(p) ? q & cycle : q;
}

path reshape_subpath(path p, real start_time, real end_time, guide new_part = nullpath .. nullpath)
{
    path new_subpath(path q)
    {
        return point(q,0) {dir(q,0)} .. new_part .. {dir(q,length(q))} point(q,length(q));
    }

    return reshape_subpath(p, start_time, end_time, new_subpath);
}

real time_at_distance_along_arc(path p, real time, real distance)
{
    real here_length = arclength(subpath(p, 0, time));
    real there_length = here_length + distance;
    return arctime(p, there_length);
}

path reshape_arc(path p, real time, real distance_before, real distance_after, guide new_part = nullpath .. nullpath)
{
    real t1 = time_at_distance_along_arc(p, time, -distance_before);
    real t2 = time_at_distance_along_arc(p, time, distance_after);
    return reshape_subpath(p, t1, t2, new_part);
}

path reshape_arc(path p, real time, real distance, guide new_part = nullpath .. nullpath)
// Good for rounding or beveling corners, for instance.
{
    return reshape_arc(p, time, distance, distance, new_part);
}

//-------------------------------------------------------------------------

string fontforge_contour_code(path p, string contour_name)
{
  string s = '';
  s += contour_name + ' = fontforge.contour()\n';
  s += contour_name + '.closed = ' + (cyclic(p) ? 'True' : 'False') + '\n';
  for (int i = 0; i < length(p); i += 1)
    {
      s += contour_name + ' += fontforge.point(';
      s += format('%f, ', precontrol(p,i).x);
      s += format('%f, False)\n', precontrol(p,i).y);

      s += contour_name + ' += fontforge.point(';
      s += format('%f,', point(p,i).x);
      s += format('%f)\n', point(p,i).y);

      s += contour_name + ' += fontforge.point(';
      s += format('%f,', postcontrol(p,i).x);
      s += format('%f, False)\n', postcontrol(p,i).y);
    }
  return s;
}

//-------------------------------------------------------------------------
