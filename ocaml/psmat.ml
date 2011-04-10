(*
  PostScript/PDF-style transformation matrices.
  FIXME: Move this to a FontForge-interface package.
*)

(*
  Copyright (c) 2011 Barry Schwartz

  Permission is hereby granted, free of charge, to any person
  obtaining a copy of this software and associated documentation
  files (the "Software"), to deal in the Software without
  restriction, including without limitation the rights to use,
  copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following
  conditions:

  The above copyright notice and this permission notice shall be
  included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
  OTHER DEALINGS IN THE SOFTWARE.
*)


module type S =
sig
  type t = float * float * float * float * float * float

  val ident : t
  val scale : float -> t
  val scalexy : float -> float -> t
  val translate : float * float -> t
  val rotate : float -> t
  val skew : float -> t
  val mul : t -> t -> t
  val compose : t list -> t
  val inv : t -> t
  val transform : float * float -> t -> float * float
end

type t = float * float * float * float * float * float

let pi = 4.0 *. atan(1.0)

let ident = (1.,0.,0.,1.,0.,0.)
let scale a = (a,0.,0.,a,0.,0.)
let scalexy a b = (a,0.,0.,b,0.,0.)
let translate (x,y) = (1.,0.,0.,1.,x,y)

let rotate degrees =
  let theta = degrees *. pi /. 180. in
  let s = sin theta in
  let c = cos theta in
  (c,s,-.s,c,0.,0.)

let skew degrees =
  let theta = degrees *. pi /. 180. in
  let m = tan theta in
  (1.,0.,m,1.,0.,0.)

let mul (a11,a12,a21,a22,a31,a32) (b11,b12,b21,b22,b31,b32) =
  ((a11 *. b11) +. (a12 *. b21), (a11 *. b12) +. (a12 *. b22),
   (a21 *. b11) +. (a22 *. b21), (a21 *. b12) +. (a22 *. b22),
   (a31 *. b11) +. (a32 *. b21) +. b31, (a31 *. b12) +. (a32 *. b22) +. b32)

let compose matrices =      (* Matrices are composed left to right. *)
  let rec inner result mats =
    match mats with
      | [] -> result
      | t :: remaining -> inner (mul result t) remaining
  in
  inner ident matrices

let inv (a11,a12,a21,a22,a31,a32) =
  let det = (a11 *. a22) -. (a12 *. a21) in
  if det = 0.0 then
    failwith "inverse of singular matrix"
  else
    let b11 = a22 /. det in
    let b12 = -.a12 /. det in
    let b21 = -.a21 /. det in
    let b22 = a11 /. det in
    (b11, b12,
     b21, b22,
     -.((a31 *. b11) +. (a32 *. b21)), -.((a31 *. b12) +. (a32 *. b22)))

let transform (x,y) (a11,a12,a21,a22,a31,a32) =
  ((x *. a11) +. (y *. a21) +. a31, (x *. a12) +. (y *. a22) +. a32)
