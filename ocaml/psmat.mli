(**
  PostScript/PDF-style transformation matrices.
*)

(* FIXME: Move this to a FontForge-interface package. *)

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

open Batteries

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
  val op : t -> (float * float -> float * float)
  val print : unit IO.output -> t -> unit
end

include S
