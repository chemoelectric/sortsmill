/*
  Intersection and overlap operations on lists of cyclic paths.


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

from sortsmill_orientation access is_oriented, is_clockwise, make_clockwise, normalize_orientations;
from sortsmill_overlap access apply_punch as apply_punch_to_outlines;

//-------------------------------------------------------------------------

struct GlyphShape {
    path[] outlines;

    void operator init(path p) {
        assert(is_oriented(p));
        outlines = new path[] { make_clockwise(p) };
    }

    void operator init(pair corner1, pair corner2) {
        // Make a rectangular blank.
        path rect = corner1---(corner1.x,corner2.y)---corner2---(corner2.x,corner1.y)---cycle;
        operator init(rect);
    }

    void draw() {
        for (path p : outlines)
            draw(p);
    }
    
    void fill() {
        for (path p : outlines)
            if (is_clockwise(p))
                fill(p);
            else
                unfill(p);
    }
};

struct Glyph {
    GlyphShape shape;
    int lsb;
    int rsb;

    void operator init(path p) { shape = GlyphShape(p); }
    void operator init(pair corner1, pair corner2) { shape = GlyphShape(corner1, corner2); }

    void draw() { shape.draw(); }
    void fill() { shape.fill(); }

    void apply_punch(GlyphShape punch) {
        shape.outlines = apply_punch_to_outlines(punch.outlines, shape.outlines);
    }
};

GlyphShape operator cast(Glyph g)
{
    return g.shape;
}

//-------------------------------------------------------------------------
