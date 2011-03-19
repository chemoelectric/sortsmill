// Operations on the orientations (clockwise, counterclockwise) of
// cyclic paths.
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

//-------------------------------------------------------------------------

int orientation(path p)
// Returns a positive number for a counterclockwise path, a negative
// number for clockwise path.
{
    return windingnumber(p, inside(p));
}

bool is_oriented(path p)
{
    int d = orientation(p);
    // Is d == 0 possible?
    return cyclic(p) && d != undefined && d != 0;
}

bool is_clockwise(path p)
{
    int d = orientation(p);
    return cyclic(p) && d != undefined && d < 0;
}

bool is_counterclockwise(path p)
{
    int d = orientation(p);
    return cyclic(p) && d != undefined && 0 < d;
}

path make_clockwise(path p)
{
    return is_counterclockwise(p) ? reverse(p) : p;
}

path make_counterclockwise(path p)
{
    return is_clockwise(p) ? reverse(p) : p;
}

//-------------------------------------------------------------------------

struct PathTreeNode {
    path p;
    PathTreeNode[] subtree;
};

void write(PathTreeNode[] tree, int indentation = 0)
{
    for (int i = 0; i < tree.length; ++i) {
        for (int j = 0; j < indentation; ++j)
            write(' ', none);
        write(format('%d: ', i), none);
        write(tree[i].p);
        write(tree[i].subtree, indentation + 1);
    }
}

int[] find_nonzero_insideness(path p, PathTreeNode[] tree, int i = 0)
{
    int insideness = 0;
    int i = 0;
    while (insideness == 0 && i < tree.length) {
        insideness = inside(p, tree[i].p);
        if (insideness == 0)
            ++i;
    }
    return new int[] { i, insideness };
}

PathTreeNode[] path_tree_insert(PathTreeNode[] tree, path p)
{
    int[] index = find_nonzero_insideness(p, tree);
    int i = index[0];
    int insideness = index[1];
    if (insideness == 0) {
        PathTreeNode pt;
        pt.p = p;
        tree.push(pt);
    } else if (insideness == 1) {
        PathTreeNode pt;
        pt.p = p;
        pt.subtree = new PathTreeNode[] { tree[i] };
        tree[i] = pt;
        int j = i;
        while (insideness == 1 && j + 1 < tree.length) {
            index = find_nonzero_insideness(p, tree, j + 1);
            j = index[0];
            insideness = index[1];
            assert(insideness != -1); // The path list was not well formed for a glyph.
            if (insideness == 1) {
                tree[i].subtree.push(tree[j]);
                tree.delete(j);
            }
        }
    } else {
        tree[i].subtree = path_tree_insert(tree[i].subtree, p);
    }
    return tree;
}

PathTreeNode[] make_path_tree(path[] paths)
{
    PathTreeNode[] tree;
    for (path p : paths)
        tree = path_tree_insert(tree, p);
    return tree;
}

path[] walk_path_tree(PathTreeNode[] tree,
                      path[] paths = new path[],
                      int level = 0)
{
    for (PathTreeNode pt : tree) {
        if (level % 2 == 0)
            paths.push(make_clockwise(pt.p));
        else
            paths.push(make_counterclockwise(pt.p));
        paths = walk_path_tree(pt.subtree, paths, level + 1);
    }
    return paths;
}

//-------------------------------------------------------------------------

path[] normalize_orientations(path[] paths)
// Make outer paths clockwise and nested paths appropriately
// counterclockwise or clockwise.
{
    PathTreeNode[] tree = make_path_tree(paths);
    path[] paths = walk_path_tree(tree);
    return paths;
}

bool is_in_interior(pair point, path[] paths, pen fillrule = currentpen)
// Is the point within the would-be-filled interior of the oriented
// paths?
{
    int winding_no = 0;
    if (winding_no != undefined) {
        int w = windingnumber(paths, point);
        if (w == undefined)
            winding_no = undefined;
        else
            winding_no += w;
    }
    return (winding_no != undefined) ? interior(winding_no, fillrule) : false;
}

//-------------------------------------------------------------------------
