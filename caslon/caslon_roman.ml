(*
  Copyright (c) 2011 Barry Schwartz
 
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
*)

(*-----------------------------------------------------------------------*)

open Batteries
open OptParse
module StringMap = Map.StringMap

module Complex = Fontdesign.Extended_complex
open Fontdesign
module Cpx = Complex_point

module Param =
struct
  type parameter =
    [
    | `Int of unit -> int option
    | `Float of unit -> float option
    | `Complex of unit -> Cpx.t option
    | `Hash of unit -> int32 option
    | `String of unit -> string option
    | `Metacubic of unit -> Metacubic.t option
    | `Random_state of unit -> Random.State.t option
    ]

  type t = parameter StringMap.t

  let no_parameters = StringMap.empty

  let put_parameter params name func = StringMap.add name func params
  let put_int_func params name ifunc = put_parameter params name (`Int ifunc)
  let put_float_func params name ffunc = put_parameter params name (`Float ffunc)
  let put_complex_func params name cfunc = put_parameter params name (`Complex cfunc)
  let put_hash_func params name hfunc = put_parameter params name (`Hash hfunc)
  let put_string_func params name sfunc = put_parameter params name (`String sfunc)
  let put_metacubic_func params name mfunc = put_parameter params name (`Metacubic mfunc)
  let put_random_state_func params name rsfunc = put_parameter params name (`Random_state rsfunc)
  let put_int params name i = put_int_func params name (fun () -> Some i)
  let put_float params name v =  put_float_func params name (fun () -> Some v)
  let put_complex params name c =  put_complex_func params name (fun () -> Some c)
  let put_hash params name h = put_hash_func params name (fun () -> Some h)
  let put_string params name s = put_string_func params name (fun () -> Some s)
  let put_metacubic params name m = put_metacubic_func params name (fun () -> Some m)
  let put_random_state params name rs = put_random_state_func params name (fun () -> Some rs)

  let parameter params name = StringMap.find name params

  let int params ?default name =
    try
      match parameter params name with
        | `Int ifunc -> Option.get (ifunc ())
        | _ -> failwith "Param.int"
    with
      | Not_found | Option.No_value as e ->
        match default with
          | Some i -> i
          | None -> raise e

  let float params ?default name =
    try
      match parameter params name with
        | `Float ffunc -> Option.get (ffunc ())
        | _ -> failwith "Param.float"
    with
      | Not_found | Option.No_value as e ->
        match default with
          | Some v -> v
          | None -> raise e

  let complex params ?default name =
    try
      match parameter params name with
        | `Complex cfunc -> Option.get (cfunc ())
        | _ -> failwith "Param.complex"
    with
      | Not_found | Option.No_value as e ->
        match default with
          | Some c -> c
          | None -> raise e

  let hash params ?default name =
    try
      match parameter params name with
        | `Hash hfunc -> Option.get (hfunc ())
        | _ -> failwith "Param.hash"
    with
      | Not_found | Option.No_value as e ->
        match default with
          | Some h -> h
          | None -> raise e

  let string params ?default name =
    try
      match parameter params name with
        | `String sfunc -> Option.get (sfunc ())
        | _ -> failwith "Param.string"
    with
      | Not_found | Option.No_value as e ->
        match default with
          | Some s -> s
          | None -> raise e

  let metacubic params ?default name =
    try
      match parameter params name with
        | `Metacubic mfunc -> Option.get (mfunc ())
        | _ -> failwith "Param.metacubic"
    with
      | Not_found | Option.No_value as e ->
        match default with
          | Some m -> m
          | None -> raise e

  let random_state params ?default name =
    try
      match parameter params name with
        | `Random_state rsfunc -> Option.get (rsfunc ())
        | _ -> failwith "Param.random_state"
    with
      | Not_found | Option.No_value as e ->
        match default with
          | Some rs -> rs
          | None -> raise e

  module type Tools_module =
  sig
    val p_ref : t ref

    val null_func : unit -> 'a option
    val set_pi_func : string -> (unit -> int option) -> unit
    val set_pf_func : string -> (unit -> float option) -> unit
    val set_pc_func : string -> (unit -> Cpx.t option) -> unit
    val set_ph_func : string -> (unit -> int32 option) -> unit
    val set_ps_func : string -> (unit -> string option) -> unit
    val set_pm_func : string -> (unit -> Metacubic.t option) -> unit
    val set_prs_func : string -> (unit -> Random.State.t option) -> unit
    val set_pi : string -> int -> unit
    val set_pf : string -> float -> unit
    val set_pc : string -> Cpx.t -> unit
    val set_ph : string -> int32 -> unit
    val set_ps : string -> string -> unit
    val set_pm : string -> Metacubic.t -> unit
    val set_prs : string -> Random.State.t -> unit

    val pi : ?default:int -> string -> int
    val pf : string -> float
    val pc : string -> Cpx.t
    val ph : string -> int32
    val ps : string -> string
    val pm : string -> Metacubic.t
    val prs : string -> Random.State.t

    val initialize_state : string -> unit
    val initialize_hash : string -> unit
    val float_hash : string -> float -> float -> float

    val xrel : float -> float
    val yrel : float -> float
    val xpos : float -> float
    val ypos : float -> float
    val x'rel : float -> Complex.t
    val y'rel : float -> Complex.t
    val x'pos : float -> Complex.t
    val y'pos : float -> Complex.t
  end

  let make_tools ~glyph_name ?(p_ref = ref no_parameters) ?param () =
    begin
      match param with
        | Some p' -> p_ref := p'
        | None -> ()
    end;
    begin
      try
        ignore (random_state !p_ref "state")
      with Not_found | Option.No_value ->
        p_ref := put_random_state !p_ref "state" (Random.State.make [||])
    end;
    let xrel v = v *. float !p_ref "width" in
    let yrel v = v *. float !p_ref "height" in
    let xpos v = xrel v -. float !p_ref ~default:0. "left_overlap" in
    let ypos v = yrel v -. float !p_ref ~default:0. "bottom_overlap" in
    let x'rel = Cpx.x' -| xrel in
    let y'rel = Cpx.y' -| yrel in
    let x'pos = Cpx.x' -| xpos in
    let y'pos = Cpx.y' -| ypos in

    let module Tools =
        struct
            let p_ref = p_ref

            let null_func = (fun () -> None)
            let set_pi_func key ifunc = p_ref := put_int_func !p_ref key ifunc
            let set_pf_func key ffunc = p_ref := put_float_func !p_ref key ffunc
            let set_pc_func key cfunc = p_ref := put_complex_func !p_ref key cfunc
            let set_ph_func key hfunc = p_ref := put_hash_func !p_ref key hfunc
            let set_ps_func key sfunc = p_ref := put_string_func !p_ref key sfunc
            let set_pm_func key mfunc = p_ref := put_metacubic_func !p_ref key mfunc
            let set_prs_func key rsfunc = p_ref := put_random_state_func !p_ref key rsfunc
            let set_pi key i = p_ref := put_int !p_ref key i
            let set_pf key v = p_ref := put_float !p_ref key v
            let set_pc key c = p_ref := put_complex !p_ref key c
            let set_ph key h = p_ref := put_hash !p_ref key h
            let set_ps key s = p_ref := put_string !p_ref key s
            let set_pm key m = p_ref := put_metacubic !p_ref key m
            let set_prs key rs = p_ref := put_random_state !p_ref key rs

            let pi ?default key = int !p_ref ?default key
            let pf key = float !p_ref key
            let pc key = complex !p_ref key
            let ph key = hash !p_ref key
            let ps key = string !p_ref key
            let pm key = metacubic !p_ref key
            let prs key = random_state !p_ref key

            (* ??????????????????????????????????????????????????????????????????????????????? *)
            let initialize_state key =
              let state =
                Random.State.make
                  (Array.of_list
                     (int_of_float (pf "design_size") ::
                        pi "os2_weight" ::
                        List.map Char.code (String.explode glyph_name)))
              in
              set_prs key state
            (* ??????????????????????????????????????????????????????????????????????????????? *)

            let initialize_hash key =
              let data =
                Print.sprintf p"%F;%d;%s" (pf "design_size") (pi "os2_weight") glyph_name
              in
              let hash_val = fnv_32a data in
              set_ph key hash_val

            let float_hash data a b =
              fnv_32a_float_hash ~hash_val:(ph "hash") data a b

            let xrel = xrel
            let yrel = yrel
            let xpos = xpos
            let ypos = ypos
            let x'rel = x'rel
            let y'rel = y'rel
            let x'pos = x'pos
            let y'pos = y'pos
          end
      in
      (module Tools : Tools_module)
end

open Param

(*-----------------------------------------------------------------------*)

let glyph_table : (Param.t -> (float, Cubic.t) Glyph.t) StringMap.t ref =
  ref StringMap.empty

let add_glyph glyph_name glyph = glyph_table := Glyph.(StringMap.add glyph_name glyph !glyph_table)
let have_glyph name = StringMap.mem name !glyph_table
let get_glyph name = StringMap.find name !glyph_table
let enum_glyphs () = StringMap.enum !glyph_table
let get_resolve_glyph name param = (get_glyph name) param
let enum_resolve_glyphs params = map (fun (glyph_name, glyph) -> glyph (params ~glyph_name)) (enum_glyphs ())
;;

(*-----------------------------------------------------------------------*)

let huge = 1e10 ;;

let c_letters = Set.of_list ["c"] ;;
let e_letters = Set.of_list ["e"] ;;
let i_letters = Set.of_list ["dotlessi"; "i"] ;;
let o_letters = Set.of_list ["o"] ;;
let l_letters = Set.of_list ["l"] ;;
let n_letters = Set.of_list ["n"] ;;
let r_letters = Set.of_list ["r"] ;;
let t_letters = Set.of_list ["t"] ;;

type bend_kind = [ `With_extrema | `With_points_at_extrema | `Without_extrema ]

let bend_points ~corner_point ~radius ~in_dir ~out_dir =
  let theta = Complex.(arg (out_dir / in_dir)) in
  let tangent_distance = Complex.of_float (abs_float (radius *. tan (0.5 *. theta))) in
  let point1 = Complex.(corner_point - tangent_distance * in_dir) in
  let point2 = Complex.(corner_point + tangent_distance * out_dir) in
  (Cpx.round point1, Cpx.round point2)
  
let extremum_free_bend ~point1 ~point2 ~tension ~in_dir ~out_dir =
  let v12 = Complex.(point2 - point1) in
  if v12 = Complex.zero then
    Metacubic.point ~in_dir ~out_dir point1
  else
    let (inx, iny) = Complex.(in_dir.re, in_dir.im) in
    let (outx, outy) = Complex.(out_dir.re, out_dir.im) in
    let (vx, vy) = Complex.(v12.re, v12.im) in
    let dir1x = if inx *. vx < 0. then 0. else inx in
    let dir1y = if iny *. vy < 0. then 0. else iny in
    let dir2x = if outx *. vx < 0. then 0. else outx in
    let dir2y = if outy *. vy < 0. then 0. else outy in
    Metacubic.(Complex_point.(
      Vect.concat
        (point ~in_dir ~out_tension:tension ~out_dir:(x' dir1x + y' dir1y) point1)
        (point ~in_tension:tension ~in_dir:(x' dir2x + y' dir2y) ~out_dir point2)
    ))

let bend_with_possible_extrema ~point1 ~point2 ~tension ~in_dir ~out_dir =
  if Complex.(point2 - point1 = zero) then
    Metacubic.point ~in_dir ~out_dir point1
  else
    Metacubic.(Vect.concat
                 (point ~dir:in_dir ~out_tension:tension point1)
                 (point ~dir:out_dir ~in_tension:tension point2))

let bend_with_points_at_extrema ~point1 ~point2 ~tension ~in_dir ~out_dir =
  if Complex.(point2 - point1 = zero) then
    Metacubic.point ~in_dir ~out_dir point1
  else
    (* FIXME: Make the following leave out inflection points. *)
    let bend =
      Metacubic.(Vect.concat
                   (point ~dir:in_dir ~out_tension:tension point1)
                   (point ~dir:out_dir ~in_tension:tension point2))
    in
    let cubic = Metacubic.to_cubic bend in
    let (x_times, y_times) = Cubic.curve_extrema_and_inflections cubic in
    let times = Array.append x_times y_times in
    let times_count = Array.length times in
    if times_count = 0 then
      bend
    else
      begin
        Array.sort Float.compare times;
        let cubic' =
          fold
            Cubic.(fun result k -> result |> put (portion cubic times.(k) times.(k + 1)))
            (Cubic.portion cubic 0. times.(0))
            (1 --^ (times_count - 1))
        in
        Cubic.(cubic' |> put (Cubic.portion cubic times.(times_count - 1) 1.))
        |> Metacubic.of_cubic
        |> Metacubic.set_dirs
      end

let contour_bend ~kind ~point1 ~point2 ~tension ~in_dir ~out_dir =
  match kind with
    | `With_extrema -> bend_with_possible_extrema ~point1 ~point2 ~tension ~in_dir ~out_dir
    | `With_points_at_extrema -> bend_with_points_at_extrema ~point1 ~point2 ~tension ~in_dir ~out_dir
    | `Without_extrema -> extremum_free_bend ~point1 ~point2 ~tension ~in_dir ~out_dir

let make_extremum_free_bend ~corner_point ~radius ~tension ~in_dir ~out_dir =
  let (point1, point2) = bend_points ~corner_point ~radius ~in_dir ~out_dir in
  let bend = extremum_free_bend ~point1 ~point2 ~tension ~in_dir ~out_dir in
  bend

let make_bend_with_possible_extrema ~corner_point ~radius ~tension ~in_dir ~out_dir =
  let (point1, point2) = bend_points ~corner_point ~radius ~in_dir ~out_dir in
  let bend = bend_with_possible_extrema ~point1 ~point2 ~tension ~in_dir ~out_dir in
  bend

let make_bend_with_points_at_extrema ~corner_point ~radius ~tension ~in_dir ~out_dir =
  let (point1, point2) = bend_points ~corner_point ~radius ~in_dir ~out_dir in
  let bend = bend_with_points_at_extrema ~point1 ~point2 ~tension ~in_dir ~out_dir in
  bend

let make_contour_bend ~kind ~corner_point ~radius ~tension ~in_dir ~out_dir =
  match kind with
    | `With_extrema ->
      make_bend_with_possible_extrema ~corner_point ~radius ~tension ~in_dir ~out_dir
    | `With_points_at_extrema ->
      make_bend_with_points_at_extrema ~corner_point ~radius ~tension ~in_dir ~out_dir
    | `Without_extrema ->
      make_extremum_free_bend ~corner_point ~radius ~tension ~in_dir ~out_dir

let make_bend_for_contours
    ?(kind = `With_extrema)
    ?corner_point
    ?corner_time1 ?corner_time2
    ?(tension = (-1.))
    ~contour1 ~contour2 ~radius () =
  let (pt, t1, t2) =
    match (corner_point, corner_time1, corner_time2) with
      | (Some pt, None, None) ->
        let t1 = Cubic.time_at_nearest_point contour1 pt in
        let t2 = Cubic.time_at_nearest_point contour2 pt in
        (pt, t1, t2)
      | (None, Some t1, Some t2) ->
        let pt = Cubic.point_at contour1 t1 in
        (pt, t1, t2)
      | (None, Some t1, None) ->
        let pt = Cubic.point_at contour1 t1 in
        let t2 = Cubic.time_at_nearest_point contour2 pt in
        (pt, t1, t2)
      | (None, None, Some t2) ->
        let pt = Cubic.point_at contour2 t2 in
        let t1 = Cubic.time_at_nearest_point contour1 pt in
        (pt, t1, t2)
      | (None, None, None) | (Some _, Some _, _) | (Some _, _, Some _) ->
        invalid_arg "make_bend_for_contours"
  in
  let (in_dir,_) = Cubic.tangents_at contour1 t1 in
  let (_, out_dir) = Cubic.tangents_at contour2 t2 in
  let (point1, point2) = bend_points ~corner_point:pt ~radius ~in_dir ~out_dir in
  let time1 = Cubic.time_at_nearest_point contour1 point1 in
  let time2 = Cubic.time_at_nearest_point contour2 point2 in
  let point1 = Cubic.point_at contour1 time1 in
  let point2 = Cubic.point_at contour2 time2 in
  let (in_dir,_) = Cubic.tangents_at contour1 time1 in
  let (_, out_dir) = Cubic.tangents_at contour2 time2 in
  (contour_bend ~kind ~point1 ~point2 ~tension ~in_dir ~out_dir,
   time1, time2)

let round_off_corner 
    ?tol
    ?kind
    ?corner_point
    ?corner_time1 ?corner_time2
    ?tension
    ~contour ~radius () =
  let (bend, time1, time2) =
    make_bend_for_contours
      ?kind ?corner_point
      ?corner_time1 ?corner_time2 ?tension
      ~contour1:contour ~contour2:contour
      ~radius
      ()
  in
  let contour =
    if Cubic.is_closed ?tol contour then
      Cubic.splice_into_cycle ~time1 ~time2 contour (Metacubic.to_cubic bend)
    else
      Cubic.splice_into ~time1 ~time2 contour (Metacubic.to_cubic bend)
  in
  contour

let flat_cut_corners ~corner ~cut_length ~normal ~in_dir ~out_dir =
  let angle1 = Complex.(arg (neg normal / in_dir)) in
  let angle2 = Complex.(arg (out_dir / normal)) in
  let normal_length = cut_length /. (tan angle1 +. tan angle2) in
  let corner1 = Complex_point.(corner - x'(normal_length /. (cos angle1)) * in_dir) in
  let corner2 = Complex_point.(corner + x'(normal_length /. (cos angle2)) * out_dir) in
  (corner1, corner2)

let flat_cut_points ~corner1 ~corner2 ~radius1 ~radius2 ~in_dir ~out_dir =
  let cut_dir = Complex_point.(dir (corner2 - corner1)) in
  let (point1, point2) = bend_points ~corner_point:corner1 ~radius:radius1 ~in_dir ~out_dir:cut_dir in
  let (point3, point4) = bend_points ~corner_point:corner2 ~radius:radius2 ~in_dir:cut_dir ~out_dir in
  (point1, point2, point3, point4, cut_dir)

let flat_cut
    ?(bend_kind1 = `With_extrema) ?(bend_kind2 = `With_extrema)
    ?(tension = huge)
    ~point1 ~point2 ~point3 ~point4
    ~in_dir ~cut_dir ~out_dir () =
  if Complex_point.(inner (point3 - point2) cut_dir) <= 0. then
    let midpoint = Complex_point.(x' 0.5 * (point2 + point3)) in
    let bend1 = contour_bend ~tension:(-1.) ~kind:bend_kind1 ~point1 ~point2:midpoint ~in_dir ~out_dir:cut_dir in
    let bend2 = contour_bend ~tension:(-1.) ~kind:bend_kind2 ~point1:midpoint ~point2:point4 ~in_dir:cut_dir ~out_dir in
    Metacubic.(bend1 |> put bend2)
  else
    let bend1 = contour_bend ~tension:(-1.) ~kind:bend_kind1 ~point1 ~point2 ~in_dir ~out_dir:cut_dir in
    let bend2 = contour_bend ~tension:(-1.) ~kind:bend_kind2 ~point1:point3 ~point2:point4 ~in_dir:cut_dir ~out_dir in
    Metacubic.(bend1 |> dput ~tension bend2)

let make_flat_cut
    ?bend_kind1 ?bend_kind2 ?tension
    ~corner1 ~corner2
    ~radius1 ~radius2
    ~in_dir ~out_dir () =
  let (point1, point2, point3, point4, cut_dir) =
    flat_cut_points ~corner1 ~corner2 ~radius1 ~radius2 ~in_dir ~out_dir
  in
  flat_cut
    ?bend_kind1 ?bend_kind2 ?tension
    ~point1 ~point2 ~point3 ~point4
    ~in_dir ~cut_dir ~out_dir ()

let make_flat_cut_for_contours
    ?bend_kind1 ?bend_kind2
    ?corner_point1 ?corner_point2
    ?corner_time1 ?corner_time2
    ?(tension = huge)
    ~contour1 ~contour2
    ~radius1 ~radius2 () =
  let (corner1, t1) =
    match (corner_point1, corner_time1) with
      | (Some pt, None) -> (pt, Cubic.time_at_nearest_point contour1 pt)
      | (None, Some t) -> (Cubic.point_at contour1 t, t)
      | (None, None) | (Some _, Some _) -> invalid_arg "make_flat_cut_for_contours"
  in
  let (corner2, t2) =
    match (corner_point2, corner_time2) with
      | (Some pt, None) -> (pt, Cubic.time_at_nearest_point contour2 pt)
      | (None, Some t) -> (Cubic.point_at contour2 t, t)
      | (None, None) | (Some _, Some _) -> invalid_arg "make_flat_cut_for_contours"
  in
  let (in_dir,_) = Cubic.tangents_at contour1 t1 in
  let (_, out_dir) = Cubic.tangents_at contour2 t2 in
  let (point1, point2, point3, point4, cut_dir) =
    flat_cut_points ~corner1 ~corner2 ~radius1 ~radius2 ~in_dir ~out_dir
  in
  let time1 = Cubic.time_at_nearest_point contour1 point1 in
  let time2 = Cubic.time_at_nearest_point contour2 point4 in
  let point1 = Cubic.point_at contour1 time1 in
  let point4 = Cubic.point_at contour2 time2 in
  let (in_dir,_) = Cubic.tangents_at contour1 time1 in
  let (_, out_dir) = Cubic.tangents_at contour2 time2 in
  (flat_cut
     ?bend_kind1 ?bend_kind2 
     ~point1 ~point2 ~point3 ~point4
     ~tension ~in_dir ~cut_dir ~out_dir (),
   time1, time2)

let make_left_serif_end
    ~param
    ?height ?depth ?shear_angle
    ?bottom_radius ?top_radius
    ?(tension = huge)
    () =
  let p = param in
  let end_height =
    match height with
      | None -> Param.float p "serif_height" +. 4.
      | Some h -> h
  in
  let end_depth =
    match depth with
      | None -> 3.
      | Some h -> h
  in
  let angle =
    match shear_angle with
      | None -> Param.float p "serif_end_angle"
      | Some a -> a
  in
  let radius1 =
    match bottom_radius with
      | None -> Param.float p "corner_radius"
      | Some r -> r
  in
  let radius2 =
    match top_radius with
      | None -> Param.float p "corner_radius"
      | Some r -> r
  in
  let shear_offset = 0.5 *. end_height *. dtan angle in
  let corner1 = Complex_point.(x'(-.shear_offset)) in
  let corner2 = Complex_point.(x' shear_offset + y' end_height) in
  Metacubic.(
    make_flat_cut ~corner1 ~corner2 ~radius1 ~radius2 ~tension
      ~in_dir:Complex_point.leftward ~out_dir:Complex_point.rightward
      ~bend_kind1:`Without_extrema ~bend_kind2:`Without_extrema ()
    <+> Complex_point.y'(-.end_depth)
  )

let make_right_serif_end
    ~param
    ?height ?depth ?shear_angle
    ?bottom_radius ?top_radius
    ?(tension = huge)
    () =
  let p = param in
  let end_height =
    match height with
      | None -> Param.float p "serif_height" +. 4.
      | Some h -> h
  in
  let end_depth =
    match depth with
      | None -> 3.
      | Some h -> h
  in
  let angle =
    match shear_angle with
      | None -> Param.float p "serif_end_angle"
      | Some a -> a
  in
  let radius1 =
    match top_radius with
      | None -> Param.float p "corner_radius"
      | Some r -> r
  in
  let radius2 =
    match bottom_radius with
      | None -> Param.float p "corner_radius"
      | Some r -> r
  in
  let shear_offset = 0.5 *. end_height *. dtan angle in
  let corner1 = Complex_point.(x' shear_offset + y' end_height) in
  let corner2 = Complex_point.(x'(-.shear_offset)) in
  Metacubic.(
    make_flat_cut ~corner1 ~corner2 ~radius1 ~radius2 ~tension
      ~in_dir:Complex_point.rightward ~out_dir:Complex_point.leftward
      ~bend_kind1:`Without_extrema ~bend_kind2:`Without_extrema ()
    <+> Complex_point.y'(-.end_depth)
  )

let make_flag ~left_notch ~flag_corner ~top_corner =
  let lower_dir = Cpx.(dir (flag_corner - left_notch)) in
  let upper_dir = Cpx.(dir (top_corner - flag_corner)) in
  let contour =
    Metacubic.(
      Cpx.(point ~dir:lower_dir (left_notch + x' 30. * lower_dir))
      |> put Cpx.(point ~in_dir:lower_dir ~out_dir:(upper_dir / rot 3.) flag_corner)
      |> put Cpx.(point ~in_dir:(upper_dir * rot 3.) ~out_dir:downward top_corner)
      )
  in
  contour

let reshape_flag ~param ~contour ~left_notch ~flag_corner ~top_corner () =
  let p = param in
  let rand = random_state p "state" in

  let lower_dir = Cpx.(dir (flag_corner - left_notch)) in
  let upper_dir = Cpx.(dir (top_corner - flag_corner)) in

  let (corner_point1, corner_point2) =
    flat_cut_corners ~corner:flag_corner ~cut_length:15.
      ~normal:Cpx.(rot (Random.State.float rand 10. -. 5.))
      ~in_dir:lower_dir ~out_dir:Cpx.(upper_dir / rot 3.)
  in
  let (cut, time1, time2) =
    make_flat_cut_for_contours ~corner_point1 ~corner_point2
      ~contour1:contour ~contour2:contour
      ~radius1:(Param.float p "corner_radius" +. 2.)
      ~radius2:(Param.float p "corner_radius" +. 2.) ()
  in
  let contour = Cubic.splice_together ~time1 ~time2 contour contour (Metacubic.to_cubic cut) in

  let contour =
    round_off_corner ~contour ~corner_point:top_corner
      ~kind:`Without_extrema ~tension:2. ~radius:8. ()
  in

  contour

(*-----------------------------------------------------------------------*)

(* The letter "c" *)

let letter_c_contours glyph_name p =

  let tools = make_tools ~glyph_name ~param:p () in
  let module Tools = (val tools : Tools_module) in
  let open Tools in

      let contour =

        let outer =
          Metacubic.(
            set_dirs (
              point ~out_curl:1.5 (pc "tail1") (* tail *)
              |> dput Cpx.(point ~dir:leftward (x'pos 0.56 + y'pos 0.00)) (* bottom *)
              |> dput ~tension:0.95 Cpx.(point ~dir:upward (x'pos 0.0 + y'pos 0.50)) (* left *)
              |> dput Cpx.(point ~dir:rightward (x'pos 0.62 + y'pos 1.00)) (* top *)
              |> dput (point ~in_curl:1.5 (pc "head1")) (* head *)
            )   
          )
        in
        let across_head_dir = Cpx.(Metacubic.outgoing_dir outer / rot 90.) in
        let head2 = Cpx.(pc "head1" + (x'(pf "head_breadth")) * across_head_dir) in
        let top = Cpx.(x'pos 0.60 + y'pos 1.00 - y'(pf "top_breadth")) in
        let partway_pt = Cpx.(head2 + x' 0.5 * (top - head2) + x'(0.1 *. (re head2 -. re top)))  in

        let sketch_inner tail2 =
          Metacubic.(
            point head2
            |> dput ~tension:1000. (point partway_pt)
            |> dput ~tension:0.75 Cpx.(point ~dir:leftward top) (* inner top *)
            |> dput Cpx.(point ~dir:downward (x'pos 0.00 + x'(pf "left_breadth") + y'pos 0.52)) (* inner left *)
            |> dput Cpx.(point ~dir:rightward (x'pos 0.64 + y'pos 0.00 + y'(pf "bottom_breadth"))) (* inner bottom *)
            |> dput (point ~in_curl:1.5 tail2) (* tail *)
            |> to_cubic
          )
        in

        (* Roughly locate the sketched counter. *)
        let (_, tangent) = Cubic.tangents_at (Metacubic.to_cubic outer) 0. in
        let tail2 = Cpx.(pc "tail1" + x'(pf "tail_breadth") * tangent * rot (-100.)) in
        let inner = sketch_inner tail2 in

        (* Now use the rough sketch to get a better estimate of the
           crosscut vector. *)
        let (tangent2, _) = Cubic.tangents_at inner (float_of_int Int.(List.length inner - 1)) in
        let crosscut_dir = Cpx.(dir (tangent - tangent2) * rot (-90.)) in
        let tail2' = Cpx.(pc "tail1" + x'(pf "tail_breadth") * crosscut_dir) in
        let shear_vector = Cpx.(x'(pf "tail_breadth" *. dtan (pf "tail_end_angle")) * crosscut_dir * rot (-90.)) in
        let tail2 = Cpx.(tail2' + shear_vector) in
        let inner = sketch_inner tail2 in

        let outer' = Metacubic.to_cubic outer in
        let c = Cubic.(outer' |> dput ~tension:1.2 inner) in
        let (x_times, y_times) =
          Cubic.curve_extrema_and_inflections ~pos:Int.(Vect.length outer - 1) c
        in
        let y_time = y_times.(0) in
        let x_time = x_times.(0) /. y_time in
        let (c2,c3) = Cubic.subdivide c (float_of_int Int.(Vect.length outer - 1) +. y_time) in
        let (c1,c2) = Cubic.subdivide c2 (float_of_int Int.(Vect.length outer - 1) +. x_time) in
        let (tail_cut, inner_time, outer_time) =
          make_flat_cut_for_contours
            ~contour1:c3 ~contour2:c1
            ~corner_time1:(float_of_int Int.(List.length c3 - 1))
            ~corner_time2:0.
            ~radius1:(pf "corner_radius" +. 1.)
            ~radius2:(pf "corner_radius" +. 1.)
            ()
        in
        let (_,c1) = Cubic.subdivide c1 outer_time in
        let (c3,_) = Cubic.subdivide c3 inner_time in
        Cubic.(c1 |> put c2 |> put c3 |> put (Metacubic.to_cubic tail_cut))
      in
      [Cubic.round contour |> Cubic.simplify]
;;

(*.......................................................................*)

(* The letter "e" *)

let letter_e_contours glyph_name p =

  let tools = make_tools ~glyph_name ~param:p () in
  let module Tools = (val tools : Tools_module) in
  let open Tools in

      let crossbar_top = pf "crossbar_height" +. pf "crossbar_breadth" in
      let crossbar_top0 = Cpx.(x'pos 1.00 - x'(pf "right_breadth") + y' crossbar_top) in

      let outer_left =
        Metacubic.(
          point ~out_curl:1.0 (pc "tail1") (* outer tail *)
          |> dput Cpx.(point ~dir:leftward (x'pos 0.54 + y'pos 0.00)) (* bottom *)
          |> dput ~tension:0.9 Cpx.(point ~dir:upward (x'pos 0.00 + y'pos 0.51)) (* left *)
          |> dput Cpx.(point ~dir:rightward (x'pos 0.52 + y'pos 1.00)) (* top *)
        )
      in

      let sketch_counter tail2 =
        Metacubic.(
          Cpx.(point ~dir:upward (crossbar_top0 + y'(pf "crossbar_fillet_size")))
          |> dput Cpx.(point ~dir:leftward (x'pos 0.50 + y'pos 1.00 - y'(pf "top_breadth"))) (* eye top *)
          |> dput Cpx.(point ~dir:downward (x'pos 0.00 + x'(pf "left_breadth") + y'pos 0.52)) (* inner left *)
          |> dput Cpx.(point ~dir:rightward (x'pos 0.60 + y'pos 0.00 + y'(pf "bottom_breadth"))) (* inner bottom *)
          |> dput (point ~in_curl:1.0 tail2) (* inner tail *)
          |> to_cubic
        )
      in

      (* Roughly locate the sketched counter. *)
      let (_, tangent) = Cubic.tangents_at (Metacubic.to_cubic outer_left) 0. in
      let tail2 = Cpx.(pc "tail1" + x'(pf "tail_breadth") * tangent * rot (-95.)) in
      let counter = sketch_counter tail2 in

      (* Now use the rough sketch to get a better estimate of the
         crosscut vector. *)
      let (tangent2, _) = Cubic.tangents_at counter (float_of_int Int.(List.length counter - 1)) in
      let crosscut_dir = Cpx.(dir (tangent - tangent2) * rot (-90.)) in
      let tail2' = Cpx.(pc "tail1" + x'(pf "tail_breadth") * crosscut_dir) in
      let shear_vector = Cpx.(x'(pf "tail_breadth" *. dtan (pf "tail_end_angle")) * crosscut_dir * rot (-90.)) in
      let tail2 = Cpx.(tail2' + shear_vector) in
      let counter = sketch_counter tail2 in

      let crossbar_height_point =
        let time = (Cubic.curve_times_at_y counter ~pos:1 (pf "crossbar_height")).(0) in
        Cubic.curve_point_at counter ~pos:1 time
      in
      let (upper, lower) =
        let intersection_time =
          (Cubic.curve_times_at_y ~pos:1 counter (pf "crossbar_height" -. (pf "crossbar_fillet_size"))).(0) +. 1.
        in
        Cubic.subdivide counter intersection_time
      in
      let outer =
        Metacubic.(
          outer_left
          |> dput Cpx.(point ~dir:downward (x'pos 0.95 + y'(pf "crossbar_height") + y'rel 0.02)) (* right *)
          |> dput Cpx.(point ~dir:leftward (x'pos 0.85 + y'(pf "crossbar_height"))) (* crossbar right *)
          |> dput ~tension:huge
              Cpx.(point ~dir:leftward (crossbar_height_point + x'(pf "crossbar_fillet_size"))) (* crossbar left *)
          |> to_cubic
        )
      in
      let (tail_cut, lower_time, outer_time) =
        make_flat_cut_for_contours
          ~contour1:lower ~contour2:outer
          ~corner_time1:(float_of_int Int.(List.length lower - 1))
          ~corner_time2:0.
          ~radius1:(pf "corner_radius" +. 1.)
          ~radius2:(pf "corner_radius" +. 1.)
          ()
      in
      let lower' = fst (Cubic.subdivide lower lower_time) |> Metacubic.of_cubic |> Metacubic.set_dirs in
      let outer' = snd (Cubic.subdivide outer outer_time) |>
          Metacubic.of_cubic |> Metacubic.set_dirs ~out_dir:Cpx.leftward
      in
      let main_contour = Metacubic.(lower' |> put tail_cut |> put outer' |> close ~tension:(-1.) |> to_cubic) in
      let crossbar_top1 =
        let time = (Cubic.curve_times_at_y upper ~pos:1 crossbar_top).(0) in
        Cubic.curve_point_at upper ~pos:1 time
      in
      let eye_upper =
        let left_intersection_time =
          (Cubic.curve_times_at_y upper ~pos:1 (crossbar_top +. (pf "crossbar_fillet_size"))).(0) +. 1.
        in
        let (upper', _) = Cubic.subdivide upper left_intersection_time in
        upper'
      in
      let eye_contour =
        Metacubic.(
          Cpx.(point ~dir:rightward (crossbar_top1 + x'(pf "crossbar_fillet_size"))) (* crossbar top left *)
          |> dput ~tension:huge Cpx.(point ~dir:rightward
                                       (crossbar_top0 - x'(pf "crossbar_fillet_size"))) (* crossbar top right *)
          |> dput (set_dirs ~guess:false (of_cubic eye_upper))
          |> close
          |> to_cubic
        )
      in
      [Cubic.round main_contour |> Cubic.simplify;
       Cubic.round eye_contour |> Cubic.simplify]
;;

(*.......................................................................*)

(* Letters such as "dotlessi" and "l" *)

let contours_similar_to_letter_l glyph_name p =

  let tools = make_tools ~glyph_name ~param:p () in
  let module Tools = (val tools : Tools_module) in
  let open Tools in
      let left_pos = (-0.5) *. pf "stem_width" in
      let right_pos = 0.5 *. pf "stem_width" in

      let left_base = Cpx.(x' left_pos + y'(pf "serif_height")) in
      let right_base = Cpx.(x' right_pos + y'(pf "serif_height")) in

      let left_serif_end =
        Metacubic.(make_left_serif_end ~param:p () <+> Cpx.x'(left_pos -. pf "left_serif_width"))
      in
      let right_serif_end =
        Metacubic.(make_right_serif_end ~param:p () <+> Cpx.x'(right_pos +. pf "right_serif_width"))
      in

      let serif_to_top = pf "height" -. pf "serif_height" in

      let left_notch = Cpx.(left_base + y' serif_to_top + pc "left_notch_drop") in
      let top_corner = Cpx.(right_base + (x_shear (y' serif_to_top) (pf "right_side_shear"))) in
      let flag_corner = Cpx.(x'(re left_notch -. pf "flag_width") + y'(im top_corner -. pf "flag_drop")) in

      let lbrack = Metacubic.(pm "left_bracket" <+> left_base) in
      let left_side =
        Metacubic.(
          Cpx.(point ~dir:leftward zero)
          |> dput ~tensions:(1.,2.) left_serif_end
          |> dput ~tensions:(2.,1.) lbrack
        )
      in

      let rbrack = Metacubic.(pm "right_bracket" <+> right_base) in
      let right_side =
        Metacubic.(
          rbrack
          |> dput ~tensions:(1.,2.) right_serif_end
          |> dput ~tensions:(2.,1.) Cpx.(point ~dir:leftward zero)
        )
      in

      let flag = make_flag ~left_notch ~flag_corner ~top_corner in
      let flag_end = Metacubic.outgoing_point flag in
      let right_side_height = 0.7 *. (pf "height" -. Cpx.im (Metacubic.incoming_point rbrack)) in
      let contour = Metacubic.(
        left_side
        |> triput flag
        |> cput ~controls:Cpx.(flag_end - y' 20., incoming_point rbrack + y' right_side_height) right_side
        |> to_cubic
      )
      in
      let contour = reshape_flag ~param:p ~contour ~left_notch ~flag_corner ~top_corner () in
      [Cubic.round contour]
;;

(*.......................................................................*)

(* The letter "dotlessi" *)

let letter_dotlessi_contours glyph_name p =
  contours_similar_to_letter_l glyph_name p |> List.map Cubic.simplify

(*.......................................................................*)

(* The letter "i" *)

let letter_i_contours glyph_name p =
  let tools = make_tools ~glyph_name ~param:p () in
  let module Tools = (val tools : Tools_module) in
  let open Tools in
      let dot_contour = Cubic.(Metacubic.to_cubic (pm "dot") <+> pc "dot_point") in
      (letter_dotlessi_contours glyph_name p @ [dot_contour] |> List.map Cubic.simplify)
;;

(*.......................................................................*)

(* The letter "l" *)

let letter_l_contours glyph_name p =
  contours_similar_to_letter_l glyph_name p |> List.map Cubic.simplify

(*.......................................................................*)

(*
(* The letter "n" *)

let letter_n_contours glyph_name p =
  let tools =
    make_tools
      ~glyph_name
      ~height:(p.x_height +. p.curve_overshoot)
      ~overshoot:p.flag_overshoot
      ~param:p
      ()
  in
  let module Tools = (val tools : Tools_module) in

  Tools.(Complex_point.(
    let leftbrack= p.left_bracket glyph_name rand in
    let rightbrack = p.right_bracket glyph_name rand in
    
    let stem_width = p.stem_width glyph_name in
    let serif_height = p.serif_height glyph_name in

    let left_serif_width = 60. in
    let right_serif_width = 55. in
    let flag_width = 60. in
    let flag_drop = 70. in
    let left_notch_drop = 100. in
    let right_side_shear = 0. in

    let left_pos = (-0.5) *. stem_width in
    let right_pos = 0.5 *. stem_width in
    let left_base = x' left_pos + y' serif_height in
    let right_base = x' right_pos + y' serif_height in
    let left_serif_end =
      Metacubic.(make_left_serif_end ~param:p ~rand ~glyph_name ()
                 <+> x'(left_pos -. left_serif_width))
    in
    let right_serif_end =
      Metacubic.(make_right_serif_end ~param:p ~rand ~glyph_name ()
                 <+> x'(right_pos +. right_serif_width))
    in

    let overshot_height = height +. overshoot in
    let serif_to_top = overshot_height -. serif_height in

    let left_notch = left_base + y'(serif_to_top -. left_notch_drop) in
    let top_corner = right_base + (x_shear (y' serif_to_top) right_side_shear) - y' 5. in
    let flag_corner = x'(re left_notch -. flag_width) + y'(im top_corner -. flag_drop) in

    let lbrack = Metacubic.(leftbrack <+> left_base) in
    let left_side =
      Metacubic.(
        point ~dir:leftward zero
        |> dput ~tensions:(1.,2.) left_serif_end
        |> dput ~tensions:(2.,1.) lbrack
      )
    in

    let rbrack = Metacubic.(rightbrack <+> right_base) in
    let right_side =
      Metacubic.(
        rbrack
        |> dput ~tensions:(1.,2.) right_serif_end
        |> dput ~tensions:(2.,1.) (point ~dir:leftward zero)
      )
    in

    let flag = make_flag ~left_notch ~flag_corner ~top_corner in
    let flag_end = Metacubic.outgoing_point flag in
    let right_side_height = 0.7 *. (overshot_height -. im (Metacubic.incoming_point rbrack)) in
    let contour = Metacubic.(
      left_side
      |> triput flag
      |> cput ~controls:(flag_end - y' 20., incoming_point rbrack + y' right_side_height) right_side
      |> to_cubic
    )
    in
(*
    let contour = reshape_flag ~param:p ~contour ~left_notch ~flag_corner ~top_corner () in
*)
    [Cubic.round contour]
  ))
;;
*)

(*-----------------------------------------------------------------------*)

(* The letter "o" *)

let letter_o_contours glyph_name p =

  let tools = make_tools ~glyph_name ~param:p () in
  let module Tools = (val tools : Tools_module) in
  let open Tools in
      let outer_contour =
        Cubic.(Cpx.(
          make_up_node (x'pos 0.00 + y'pos 0.50) (* left *)
          |> dput ~tension:1. (make_right_node (x'pos 0.50 + y'pos 1.00)) (* top *)
          |> dput ~tension:1. (make_down_node (x'pos 1.00 + y'pos 0.50))  (* right *)
          |> dput ~tension:1. (make_left_node (x'pos 0.49 + y'pos 0.00))  (* bottom *)
          |> dclose ~tension:0.95
        ))
      in
      let inner_contour =
        Cubic.(Cpx.(
          make_down_node (x'pos 0.00 + x'(pf "left_breadth") + y'pos 0.52) (* left *)
          |> dput ~tension:0.98 (make_right_node (x'pos 0.49 + y'pos 0.00 + y'(pf "bottom_breadth"))) (* bottom *)
          |> dput ~tension:1. (make_up_node (x'pos 1.00 - x'(pf "right_breadth") + y'pos 0.48)) (* right *)
          |> dput ~tension:1. (make_left_node (x'pos 0.48 + y'pos 1.00 - y'(pf "top_breadth"))) (* top *)
          |> dclose ~tension:1.
        ))
      in
      [Cubic.round outer_contour;
       Cubic.round inner_contour]
;;

(*.......................................................................*)

(* The letter "r" *)

let letter_r_contours glyph_name p =
  let ilike_contours = contours_similar_to_letter_l glyph_name p in
  let tools = make_tools ~glyph_name ~param:p () in
  let module Tools = (val tools : Tools_module) in
  let open Tools in
      let offset = Complex.of_float (-0.5 *. pf "stem_width") in
      let stem = List.hd ilike_contours in
      let stem_offset = Cubic.(stem <+> offset) in

      let shoulder = pm "shoulder" in
      let arm_top_offset = Cubic.(nearest_point stem_offset (Metacubic.incoming_point shoulder)) in
      let shoulder = Vect.modify shoulder 0 (fun (i,_,o) -> (i, arm_top_offset, o)) in
      let arm_top = Complex.(arm_top_offset - offset) in

      let arm_end = pm "arm_end" in

      let arm_lower = pm "arm_lower" in
      let arm_bottom_offset = Cubic.(nearest_point stem_offset (Metacubic.outgoing_point arm_lower)) in
      let arm_lower = Vect.modify arm_lower (Vect.length arm_lower - 1) (fun (i,_,o) -> (i, arm_bottom_offset, o)) in

      let arm = Metacubic.(shoulder |> put arm_end |> put arm_lower |> to_cubic) in
      let contour = Cubic.(splice_into_cycle stem (arm <-> offset)) in
      let contour = round_off_corner ~contour ~corner_point:arm_top ~radius:3. () in
      [Cubic.round contour |> Cubic.simplify]
;;

(*-----------------------------------------------------------------------*)

(* The letter "t" *)

let letter_t_contours glyph_name p =

  let tools = make_tools ~glyph_name ~param:p () in
  let module Tools = (val tools : Tools_module) in
  let open Tools in
      let left_pos = Cpx.x'(-.0.5 *. pf "stem_width") in
      let right_pos = Cpx.x'(0.5 *. pf "stem_width") in
      let crossbar_height = pf "crossbar_height" in
      let crossbar_breadth = floor (0.85 *. pf "stem_width" /. (1. +. 0.7 *. pf "contrast") +. 0.5) in
      let bottom_breadth = 0.70 *. pf "stem_width" /. (1. +. pf "contrast") in
      let sheared_terminal_width = 70. in
      let sheared_terminal_height = crossbar_height -. crossbar_breadth -. 3. in
      let tail_breadth = 0.34 *. pf "stem_width" /. (1. +. pf "contrast") in
      let tail_cut_angle = 100. in

      let tail1 =
        Cpx.(x'(pf "width" -. 0.5 *. pf "stem_width" -. sheared_terminal_width) + y'pos 0.00 + y'(bottom_breadth +. 10.))
      in
      let tail2 = Cpx.(tail1 + x' tail_breadth * rot tail_cut_angle) in
      let lower_bowl_width = Cpx.(re (x'pos 1.00 - x' sheared_terminal_width)) in
      let upper_bowl_width = Cpx.(re tail2 -. re right_pos) in

      let top_corner = Cpx.(right_pos + x' 10. + y'pos 1.00) in
      let bowl_point = Cpx.(right_pos + x'(0.50 *. upper_bowl_width) + y'pos 0.00 + y' bottom_breadth) in
      let bottom_point = Cpx.(left_pos + x'(0.50 *. lower_bowl_width) + y'pos 0.00) in

      let left_corner = Cpx.(x' (re left_pos -. sheared_terminal_width) + y' sheared_terminal_height) in
      let sheared_terminal_dir = Cpx.(dir (top_corner - left_corner)) in
      let sheared_terminal_dip_dir = Cpx.(sheared_terminal_dir / rot 5.) in
      let sheared_terminal_rise_dir = Cpx.(sheared_terminal_dir * rot 5.) in

      let left_cut_normal = Cpx.(rot (Random.State.float (prs "state") 10. +. 5.)) in
      let (corner_point1, corner_point2) =
        flat_cut_corners ~corner:left_corner ~cut_length:15.
          ~normal:left_cut_normal ~in_dir:Cpx.leftward ~out_dir:sheared_terminal_dip_dir
      in
      let left_cut =
        make_flat_cut
          ~bend_kind2:`Without_extrema
          ~corner1:corner_point1 ~corner2:corner_point2
          ~radius1:(pf "corner_radius" +. 3.) ~radius2:(pf "corner_radius" +. 3.)
          ~in_dir:Cpx.leftward ~out_dir:sheared_terminal_dip_dir ()
      in

      let top_cut_normal = Cpx.(rot (Random.State.float (prs "state") 5. -. 103.)) in
      let (corner_point1, corner_point2) =
        flat_cut_corners ~corner:top_corner ~cut_length:14.
          ~normal:top_cut_normal
          ~in_dir:sheared_terminal_rise_dir ~out_dir:Cpx.downward
      in
      let top_cut =
        make_flat_cut
          ~corner1:corner_point1 ~corner2:corner_point2
          ~radius1:(pf "corner_radius" +. 2.) ~radius2:(pf "corner_radius" +. 2.)
          ~in_dir:sheared_terminal_rise_dir ~out_dir:Cpx.downward ()
      in

      let left_side =
        Metacubic.(
          point ~out_curl:1. tail1
          |> dput Cpx.(point ~dir:leftward bottom_point)
          |> dput ~tension:1.1 Cpx.(point ~dir:upward (left_pos - x' 5. + y'pos 0.15))
          |> dput Cpx.(point ~dir:upward (x' (re left_pos) + y' sheared_terminal_height - y' 60.))
          |> dput ~tension:huge Cpx.(point ~in_dir:upward ~out_dir:leftward
                                       (x' (re left_pos) + y' sheared_terminal_height))
          |> dput ~tension:huge left_cut
          |> put top_cut
          |> to_cubic
        )
      in

      let sketch_lower_right tail2 =
        Metacubic.(
          Cpx.(point ~dir:downward (right_pos + y'pos 0.25))
          |> dput Cpx.(point ~dir:rightward bowl_point)
          |> dput (point ~in_curl:1. tail2)
          |> to_cubic
        )
      in

      (* Roughly locate the sketched lower-right side. *)
      let (_, tangent) = Cubic.tangents_at left_side 0. in
      let tail2 = Cpx.(tail1 + x' tail_breadth * tangent * rot (-100.)) in
      let lower_right = sketch_lower_right tail2 in

      (* Now use the rough sketch to get a better estimate of the
         crosscut vector. *)
      let (tangent2, _) = Cubic.tangents_at lower_right (float_of_int Int.(List.length lower_right - 1)) in
      let crosscut_dir = Cpx.(dir (tangent - tangent2) * rot (-90.)) in
      let tail2' = Cpx.(tail1 + x' tail_breadth * crosscut_dir) in
      let shear_vector = Cpx.(x'(tail_breadth *. dtan (pf "tail_end_angle")) * crosscut_dir * rot (-90.)) in
      let tail2 = Cpx.(tail2' + shear_vector) in
      let lower_right = sketch_lower_right tail2 in

      let crossbar_point1 = Cpx.(x'(re right_pos) + x' 5. + y' crossbar_height) in
      let crossbar_point2 = Cpx.(x'(re right_pos) + y'(crossbar_height -. crossbar_breadth)) in

      let crossbar_end =
        make_flat_cut
          ~bend_kind1:`Without_extrema ~bend_kind2:`Without_extrema
          ~corner1:Cpx.(x'(re right_pos +. upper_bowl_width -. 8.) + y' crossbar_height)
          ~corner2:Cpx.(x'(re right_pos +. upper_bowl_width -. 18.) + y'(crossbar_height -. crossbar_breadth))
          ~radius1:6. ~radius2:6. ~tension:huge
          ~in_dir:Cpx.rightward ~out_dir:Cpx.leftward ()
      in

      let right_side =
        Metacubic.(
          point crossbar_point1
          |> dput ~tension:huge (crossbar_end)
          |> dput ~tension:huge Cpx.(point ~in_dir:leftward crossbar_point2)
          |> dput (set_dirs ~in_dir:Cpx.downward (of_cubic lower_right))
          |> to_cubic
        )
      in

      let (tail_cut, right_time, left_time) =
        make_flat_cut_for_contours
          ~contour1:right_side ~contour2:left_side
          ~corner_time1:(float_of_int Int.(List.length right_side - 1))
          ~corner_time2:0.
          ~radius1:(pf "corner_radius")
          ~radius2:(pf "corner_radius") ()
      in
      let right_side = fst (Cubic.subdivide right_side right_time) in
      let left_side = snd (Cubic.subdivide left_side left_time) in
      let cycle = Metacubic.(
        of_cubic left_side
        |> cput ~control:Cpx.(crossbar_point1 + y' 40.) (of_cubic right_side)
        |> put tail_cut
        |> to_cubic
      )
      in
      let cycle =
        round_off_corner 
          ~corner_point:Cpx.(x'(re left_pos) + y' sheared_terminal_height)
          ~contour:cycle ~radius:10. ()
      in
      let cycle = round_off_corner ~corner_point:crossbar_point1 ~contour:cycle ~radius:10. () in
      let cycle = round_off_corner ~corner_point:crossbar_point2 ~contour:cycle ~radius:10. () in
      [Cubic.round cycle |> Cubic.simplify]
;;

(*-----------------------------------------------------------------------*)

add_glyph "space"
  Glyph.(fun p -> {
    empty with
      name = "space";
      rsb = Some (Param.float p "space_width");
  })
;;

add_glyph "c"
  Glyph.(fun p -> {
    empty with
      name = "c";
      contours = letter_c_contours "c" p;
      lsb = Some 0.;
      rsb = Some 50.;
  })
;;

add_glyph "e"
  Glyph.(fun p -> {
    empty with
      name = "e";
      contours = letter_e_contours "e" p;
      lsb = Some 0.;
      rsb = Some 50.;
  })
;;

add_glyph "dotlessi"
  Glyph.(fun p -> {
    empty with
      name = "dotlessi";
      contours = letter_dotlessi_contours "dotlessi" p;
      lsb = Some 0.;
      rsb = Some 50.;
  })
;;

add_glyph "i"
  Glyph.(fun p -> {
    empty with
      name = "i";
      contours = letter_i_contours "i" p;
      lsb = Some 0.;
      rsb = Some 50.;
  })
;;

add_glyph "l"
  Glyph.(fun p -> {
    empty with
      name = "l";
      contours = letter_l_contours "l" p;
      lsb = Some 0.;
      rsb = Some 50.;
  })
;;

(*
add_glyph "n"
  Glyph.(fun p -> {
    empty with
      name = "n";
      contours = letter_n_contours "n" p;
      lsb = Some 0.;
      rsb = Some 50.;
  })
;;
*)

add_glyph "o"
  Glyph.(fun p -> {
    empty with
      name = "o";
      contours = letter_o_contours "o" p;
      lsb = Some 0.;
      rsb = Some 50.;
  })
;;

add_glyph "r"
  Glyph.(fun p -> {
    empty with
      name = "r";
      contours = letter_r_contours "r" p;
      lsb = Some 0.;
      rsb = Some 50.;
  })
;;

add_glyph "t"
  Glyph.(fun p -> {
    empty with
      name = "t";
      contours = letter_t_contours "t" p;
      lsb = Some 0.;
      rsb = Some 50.;
  })
;;

(*-----------------------------------------------------------------------*)

let run_command ?(outp = stdout) params =
  let tools = make_tools ~glyph_name:"" ~param:(params ~glyph_name:"") () in
  let module Tools = (val tools : Tools_module) in
  let open Tools in
      let glyph_opt = StdOpt.str_option () in
      let font_opt = StdOpt.str_option () in
      let font_flags_opt = StdOpt.str_option () in
      let sfd_opt = StdOpt.str_option () in
      let opt_parser = OptParser.make () in
      OptParser.add ~short_name:'g' ~long_name:"glyph" opt_parser glyph_opt;
      OptParser.add ~short_name:'f' ~long_name:"font" opt_parser font_opt;
      OptParser.add ~short_name:'l' ~long_name:"flags" opt_parser font_flags_opt;
      OptParser.add ~short_name:'s' ~long_name:"sfd" opt_parser sfd_opt;
      let _args = OptParser.parse_argv opt_parser in
      match Opt.opt glyph_opt with

        | None ->
          output_string outp "#!/usr/bin/env python\n";
          output_string outp "\n";
          output_string outp "import fontforge\n";
          output_string outp "import psMat\n";
          output_string outp "\n";
          output_string outp "my_font = fontforge.font()\n";
          output_string outp "\n";
          Print.printf p"my_font.version = '%s'\n" (ps "version");
          Print.printf p"my_font.fontname = '%s'\n" (ps "fontname");
          Print.printf p"my_font.familyname = '%s'\n" (ps "familyname");
          Print.printf p"my_font.fullname = '%s'\n" (ps "fullname");
          Print.printf p"my_font.weight = '%s'\n" (ps "weight");
          Print.printf p"my_font.appendSFNTName('English (US)', 'Family', '%s')\n" (ps "family");
          Print.printf p"my_font.appendSFNTName('English (US)', 'SubFamily', '%s')\n" (ps "subfamily");
          begin
            try
              Print.printf
                p"my_font.appendSFNTName('English (US)', 'Preferred Family', '%s')\n"
                (ps "preferred_family")
            with Not_found | Option.No_value -> ()
          end;
          begin
            try
              Print.printf
                p"my_font.appendSFNTName('English (US)', 'Preferred Styles', '%s')\n"
                (ps "preferred_subfamily")
            with Not_found | Option.No_value -> ()
          end;
          begin
            try
              Print.printf
                p"my_font.appendSFNTName('English (US)', 'WWS Family', '%s')\n"
                (ps "wws_family")
            with Not_found | Option.No_value -> ()
          end;
          begin
            try
              Print.printf
                p"my_font.appendSFNTName('English (US)', 'WWS Subfamily', '%s')\n"
                (ps "wws_subfamily")
            with Not_found | Option.No_value -> ()
          end;
          output_string outp "\n";
          Print.printf p"my_font.size_feature = (%F,)\n" (pf "design_size");
          output_string outp "\n";
          Print.printf p"my_font.os2_weight = %i\n" (pi "os2_weight");
          output_string outp "\n";
          output_string outp "def preferred_unicode(glyphname):\n";
          output_string outp "    if '_' in glyphname:\n";
          output_string outp "        uni = -1\n";
          output_string outp "    else:\n";
          output_string outp "        uni = fontforge.unicodeFromName(glyphname)\n";
          output_string outp "    return uni\n";
          output_string outp "\n";

          iter
            (Cubic_glyph.print_python_glyph_code outp)
            (enum_resolve_glyphs params);

          output_string outp "\n";
          output_string outp "my_font.encoding = 'UnicodeBMP'\n";
          output_string outp "\n";
          if Option.is_some (Opt.opt sfd_opt) then
            Print.printf p"my_font.save('%s')\n" (Option.get (Opt.opt sfd_opt));
          if Option.is_some (Opt.opt font_opt) then
            if Option.is_some (Opt.opt font_flags_opt) then
              Print.printf p"my_font.generate('%s', flags=[%s])\n"
                (Option.get (Opt.opt font_opt))
                (Option.get (Opt.opt font_flags_opt))
            else
              Print.printf p"my_font.generate('%s', flags=['opentype',])\n"
                (Option.get (Opt.opt font_opt))

        | Some glyph_name ->
          if have_glyph glyph_name then
            let glyph = get_resolve_glyph glyph_name (params ~glyph_name) in
            Cubic_glyph.print_python_glyph_update_module outp glyph

(*-----------------------------------------------------------------------*)
