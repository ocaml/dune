open Notty
open Lwd_utils

module Focus :
sig
  type var = int Lwd.var
  type handle
  val make : unit -> handle
  val request : handle -> unit
  val request_var : var -> unit
  val release : handle -> unit

  type status =
    | Empty
    | Handle of int * var
    | Conflict of int

  val empty : status
  (*val is_empty : status -> bool*)
  val status : handle -> status Lwd.t
  val has_focus : status -> bool
  val merge : status -> status -> status
end = struct

  type var = int Lwd.var

  type status =
    | Empty
    | Handle of int * var
    | Conflict of int

  type handle = var * status Lwd.t

  let make () =
    let v = Lwd.var 0 in
    (v, Lwd.map ~f:(fun i -> Handle (i, v)) (Lwd.get v))

  let empty : status = Empty

  let status (h : handle) : status Lwd.t = snd h

  let has_focus = function
    | Empty -> false
    | Handle (i, _) | Conflict i -> i > 0

  let clock = ref 0

  let request_var (v : var) =
    incr clock;
    Lwd.set v !clock

  let request (v, _ : handle) = request_var v
  let release (v, _ : handle) = incr clock; Lwd.set v 0

  let merge s1 s2 : status = match s1, s2 with
    | Empty, x | x, Empty -> x
    | _, Handle (0, _) -> s1
    | Handle (0, _), _ -> s2
    | Handle (i1, _), Handle (i2, _) when i1 = i2 -> s1
    | (Handle (i1, _) | Conflict i1), Conflict i2 when i1 < i2 -> s2
    | (Handle (i1, _) | Conflict i1), Handle (i2, _) when i1 < i2 ->
      Conflict i2
    | Conflict _, (Handle (_, _) | Conflict _) -> s1
    | Handle (i1, _), (Handle (_, _) | Conflict _) -> Conflict i1
end

module Gravity :
sig
  type direction = [
    | `Negative
    | `Neutral
    | `Positive
  ]
  val pp_direction : Format.formatter -> direction -> unit
  type t
  val pp : Format.formatter -> t -> unit
  val make : h:direction -> v:direction -> t
  val default : t
  val h : t -> direction
  val v : t -> direction

  type t2
  val pair : t -> t -> t2
  val p1 : t2 -> t
  val p2 : t2 -> t
end =
struct
  type direction = [ `Negative | `Neutral | `Positive ]
  type t = int
  type t2 = int

  let default = 0

  let pack = function
    | `Negative -> 0
    | `Neutral  -> 1
    | `Positive -> 2

  let unpack = function
    | 0 -> `Negative
    | 1 -> `Neutral
    | _ -> `Positive

  let make ~h ~v =
    (pack h lsl 2) lor pack v

  let h x = unpack (x lsr 2)
  let v x = unpack (x land 3)

  let pp_direction ppf dir =
    let text = match dir with
      | `Negative -> "`Negative"
      | `Neutral  -> "`Neutral"
      | `Positive -> "`Positive"
    in
    Format.pp_print_string ppf text

  let pp ppf g =
    Format.fprintf ppf "{ h = %a; v = %a }" pp_direction (h g) pp_direction (v g)

  let pair t1 t2 =
    (t1 lsl 4) lor t2

  let p1 t = (t lsr 4) land 15
  let p2 t = t land 15
end
type gravity = Gravity.t

module Interval : sig
  type t = private int
  val make : int -> int -> t
  val shift : t -> int -> t
  val fst : t -> int
  val snd : t -> int
  (*val size : t -> int*)
  val zero : t
end = struct
  type t = int

  let half = Sys.word_size lsr 1
  let mask = (1 lsl half) - 1

  let make x y =
    let size = y - x in
    (*assert (size >= 0);*)
    (x lsl half) lor (size land mask)

  let shift t d =
    t + d lsl half

  let fst t = t asr half
  let size t = t land mask
  let snd t = fst t + size t

  let zero = 0
end

module Ui =
struct
  type may_handle = [ `Unhandled | `Handled ]

  type mouse_handler = x:int -> y:int -> Unescape.button -> [
      | `Unhandled
      | `Handled
      | `Grab of (x:int -> y:int -> unit) * (x:int -> y:int -> unit)
    ]

  type semantic_key = [
    (* Clipboard *)
    | `Copy
    | `Paste
    (* Focus management *)
    | `Focus of [`Next | `Prev | `Left | `Right | `Up | `Down]
  ]

  type key = [
    | Unescape.special | `Uchar of Uchar.t | `ASCII of char | semantic_key
  ] * Unescape.mods

  type mouse = Unescape.mouse

  type event = [ `Key of key | `Mouse of mouse | `Paste of Unescape.paste ]

  type layout_spec = { w : int; h : int; sw : int; sh : int }

  let pp_layout_spec ppf { w; h; sw; sh } =
    Format.fprintf ppf "{ w = %d; h = %d; sw = %d; sh = %d }" w h sw sh

  type flags = int
  let flags_none = 0
  let flag_transient_sensor = 1
  let flag_permanent_sensor = 2

  type size_sensor = w:int -> h:int -> unit
  type frame_sensor = x:int -> y:int -> w:int -> h:int -> unit -> unit

  type t = {
    w : int; sw : int;
    h : int; sh : int;
    mutable desc : desc;
    focus : Focus.status;
    mutable flags : flags;
    mutable sensor_cache : (int * int * int * int) option;
    mutable cache : cache;
  }
  and cache = {
    vx : Interval.t; vy : Interval.t;
    image : image;
  }
  and desc =
    | Atom of image
    | Size_sensor of t * size_sensor
    | Transient_sensor of t * frame_sensor
    | Permanent_sensor of t * frame_sensor
    | Resize of t * Gravity.t2 * A.t
    | Mouse_handler of t * mouse_handler
    | Focus_area of t * (key -> may_handle)
    | Shift_area of t * int * int
    | Event_filter of t * ([`Key of key | `Mouse of mouse] -> may_handle)
    | X of t * t
    | Y of t * t
    | Z of t * t


  let layout_spec t : layout_spec =
    { w = t.w; h = t.h; sw = t.sw; sh = t.sh }
  let layout_width t = t.w
  let layout_stretch_width t = t.sw
  let layout_height t = t.h
  let layout_stretch_height t = t.sh

  let cache : cache =
    { vx = Interval.zero; vy = Interval.zero; image = I.empty }

  let empty : t =
    { w = 0; sw = 0; h = 0; sh = 0; flags = flags_none;
      focus = Focus.empty; desc = Atom I.empty;
      sensor_cache = None; cache }

  let atom img : t =
    { w = I.width img; sw = 0;
      h = I.height img; sh = 0;
      focus = Focus.empty; flags = flags_none;
      desc = Atom img;
      sensor_cache = None; cache; }

  let space_1_0 = atom (I.void 1 0)
  let space_0_1 = atom (I.void 0 1)
  let space_1_1 = atom (I.void 1 1)

  let space x y =
    match x, y with
    | 0, 0 -> empty
    | 1, 0 -> space_1_0
    | 0, 1 -> space_0_1
    | 1, 1 -> space_1_1
    | _ -> atom (I.void x y)

  let mouse_area f t : t =
    { t with desc = Mouse_handler (t, f) }

  let keyboard_area ?focus f t : t =
    let focus = match focus with
      | None -> t.focus
      | Some focus -> Focus.merge focus t.focus
    in
    { t with desc = Focus_area (t, f); focus }

  let shift_area x y t : t =
    { t with desc = Shift_area (t, x, y) }

  let size_sensor handler t : t =
    { t with desc = Size_sensor (t, handler) }

  let transient_sensor frame_sensor t =
    { t with desc = Transient_sensor (t, frame_sensor);
             flags = t.flags lor flag_transient_sensor }

  let permanent_sensor frame_sensor t =
    { t with desc = Permanent_sensor (t, frame_sensor);
             flags = t.flags lor flag_permanent_sensor }

  let prepare_gravity = function
    | None, None -> Gravity.(pair default default)
    | Some g, None | None, Some g -> Gravity.(pair g g)
    | Some pad, Some crop -> Gravity.(pair pad crop)

  let resize ?w ?h ?sw ?sh ?pad ?crop ?(bg=A.empty) t : t =
    let g = prepare_gravity (pad, crop) in
    match (w, t.w), (h, t.h), (sw, t.sw), (sh, t.sh) with
    | (Some w, _ | None, w), (Some h, _ | None, h),
      (Some sw, _ | None, sw), (Some sh, _ | None, sh) ->
      {t with w; h; sw; sh; desc = Resize (t, g, bg)}

  let resize_to ({w; h; sw; sh} : layout_spec) ?pad ?crop ?(bg=A.empty) t : t =
    let g = prepare_gravity (pad, crop) in
    {t with w; h; sw; sh; desc = Resize (t, g, bg)}

  let event_filter ?focus f t : t =
    let focus = match focus with
      | None -> t.focus
      | Some focus -> focus
    in
    { t with desc = Event_filter (t, f); focus }

  let join_x a b = {
    w = (a.w + b.w);   sw = (a.sw + b.sw);
    h = (maxi a.h b.h); sh = (maxi a.sh b.sh);
    flags = a.flags lor b.flags;
    focus = Focus.merge a.focus b.focus; desc = X (a, b);
    sensor_cache = None; cache
  }

  let join_y a b = {
    w = (maxi a.w b.w); sw = (maxi a.sw b.sw);
    h = (a.h + b.h);   sh = (a.sh + b.sh);
    flags = a.flags lor b.flags;
    focus = Focus.merge a.focus b.focus; desc = Y (a, b);
    sensor_cache = None; cache;
  }

  let join_z a b = {
    w = (maxi a.w b.w); sw = (maxi a.sw b.sw);
    h = (maxi a.h b.h); sh = (maxi a.sh b.sh);
    flags = a.flags lor b.flags;
    focus = Focus.merge a.focus b.focus; desc = Z (a, b);
    sensor_cache = None; cache;
  }

  let pack_x = (empty, join_x)
  let pack_y = (empty, join_y)
  let pack_z = (empty, join_z)

  let hcat xs = Lwd_utils.reduce pack_x xs
  let vcat xs = Lwd_utils.reduce pack_y xs
  let zcat xs = Lwd_utils.reduce pack_z xs

  let has_focus t = Focus.has_focus t.focus

  let rec pp ppf t =
    Format.fprintf ppf
      "@[<hov>{@ w = %d;@ h = %d;@ sw = %d;@ sh = %d;@ desc = @[%a@];@ }@]"
      t.w t.h t.sw t.sh pp_desc t.desc

  and pp_desc ppf = function
    | Atom _   -> Format.fprintf ppf "Atom _"
    | Size_sensor (desc, _) ->
      Format.fprintf ppf "Size_sensor (@[%a,@ _@])" pp desc
    | Transient_sensor (desc, _) ->
      Format.fprintf ppf "Transient_sensor (@[%a,@ _@])" pp desc
    | Permanent_sensor (desc, _) ->
      Format.fprintf ppf "Permanent_sensor (@[%a,@ _@])" pp desc
    | Resize (desc, gravity, _bg) ->
      Format.fprintf ppf "Resize (@[%a,@ %a,@ %a@])" pp desc
        Gravity.pp (Gravity.p1 gravity)
        Gravity.pp (Gravity.p2 gravity)
    | Mouse_handler (n, _) ->
      Format.fprintf ppf "Mouse_handler (@[%a,@ _@])" pp n
    | Focus_area (n, _) ->
      Format.fprintf ppf "Focus_area (@[%a,@ _@])" pp n
    | Shift_area (n, _, _) ->
      Format.fprintf ppf "Shift_area (@[%a,@ _@])" pp n
    | Event_filter (n, _) ->
      Format.fprintf ppf "Event_filter (@[%a,@ _@])" pp n
    | X (a, b) -> Format.fprintf ppf "X (@[%a,@ %a@])" pp a pp b
    | Y (a, b) -> Format.fprintf ppf "Y (@[%a,@ %a@])" pp a pp b
    | Z (a, b) -> Format.fprintf ppf "Z (@[%a,@ %a@])" pp a pp b

  let iter f ui = match ui.desc with
    | Atom _ -> ()
    | Size_sensor (u, _) | Transient_sensor (u, _) | Permanent_sensor (u, _)
    | Resize (u, _, _) | Mouse_handler (u, _)
    | Focus_area (u, _) | Shift_area (u, _, _) | Event_filter (u, _)
      -> f u
    | X (u1, u2) | Y (u1, u2) | Z (u1, u2) -> f u1; f u2
end
type ui = Ui.t

module Renderer =
struct
  open Ui

  type size = int * int

  type grab_function = (x:int -> y:int -> unit) * (x:int -> y:int -> unit)
  type t = {
    mutable size : size;
    mutable view : ui;
    mutable mouse_grab : grab_function option;
  }

  let make () = {
    mouse_grab = None;
    size = (0, 0);
    view = Ui.empty;
  }

  let size t = t.size

  let solve_focus ui i =
    let rec aux ui =
      match ui.focus with
      | Focus.Empty | Focus.Handle (0, _) -> ()
      | Focus.Handle (i', _) when i = i' -> ()
      | Focus.Handle (_, v) -> Lwd.set v 0
      | Focus.Conflict _ -> Ui.iter aux ui
    in
    aux ui

  let split ~a ~sa ~b ~sb total =
    let stretch = sa + sb in
    let flex = total - a - b in
    if stretch > 0 && flex > 0 then
      let ratio =
        if sa > sb then
          flex * sa / stretch
        else
          flex - flex * sb / stretch
      in
      (a + ratio, b + flex - ratio)
    else
      (a, b)

  let pack ~fixed ~stretch total g1 g2 =
    let flex = total - fixed in
    if stretch > 0 && flex > 0 then
      (0, total)
    else
      let gravity = if flex >= 0 then g1 else g2 in
      match gravity with
      | `Negative -> (0, fixed)
      | `Neutral  -> (flex / 2, fixed)
      | `Positive -> (flex, fixed)

  let has_transient_sensor flags = flags land flag_transient_sensor <> 0
  let has_permanent_sensor flags = flags land flag_permanent_sensor <> 0

  let rec update_sensors ox oy sw sh ui =
    if has_transient_sensor ui.flags || (
        has_permanent_sensor ui.flags &&
        match ui.sensor_cache with
        | None -> true
        | Some (ox', oy', sw', sh') ->
          not (ox = ox' && oy = oy' && sw = sw' && sh = sh')
      )
    then (
      ui.flags <- ui.flags land lnot flag_transient_sensor;
      if has_permanent_sensor ui.flags then
        ui.sensor_cache <- Some (ox, oy, sw, sh);
      match ui.desc with
      | Atom _ -> ()
      | Size_sensor (t, _) | Mouse_handler (t, _)
      | Focus_area (t, _) | Event_filter (t, _) ->
        update_sensors ox oy sw sh t
      | Transient_sensor (t, sensor) ->
        ui.desc <- t.desc;
        let sensor = sensor ~x:ox ~y:oy ~w:sw ~h:sh in
        update_sensors ox oy sw sh t;
        sensor ()
      | Permanent_sensor (t, sensor) ->
        let sensor = sensor ~x:ox ~y:oy ~w:sw ~h:sh in
        update_sensors ox oy sw sh t;
        sensor ()
      | Resize (t, g, _) ->
        let open Gravity in
        let dx, rw = pack ~fixed:t.w ~stretch:t.sw sw (h (p1 g)) (h (p2 g)) in
        let dy, rh = pack ~fixed:t.h ~stretch:t.sh sh (v (p1 g)) (v (p2 g)) in
        update_sensors (ox + dx) (oy + dy) rw rh t
      | Shift_area (t, sx, sy) ->
        update_sensors (ox - sx) (oy - sy) sw sh t
      | X (a, b) ->
        let aw, bw = split ~a:a.w ~sa:a.sw ~b:b.w ~sb:b.sw sw in
        update_sensors ox oy aw sh a;
        update_sensors (ox + aw) oy bw sh b
      | Y (a, b) ->
        let ah, bh = split ~a:a.h ~sa:a.sh ~b:b.h ~sb:b.sh sh in
        update_sensors ox oy sw ah a;
        update_sensors ox (oy + ah) sw bh b
      | Z (a, b) ->
        update_sensors ox oy sw sh a;
        update_sensors ox oy sw sh b
    )

  let update_focus ui =
    match ui.focus with
    | Focus.Empty | Focus.Handle _ -> ()
    | Focus.Conflict i -> solve_focus ui i

  let update t size ui =
    t.size <- size;
    t.view <- ui;
    update_sensors 0 0 (fst size) (snd size) ui;
    update_focus ui

  let dispatch_mouse st x y btn w h t =
    let handle ox oy f =
      match f ~x:(x - ox) ~y:(y - oy) btn with
      | `Unhandled -> false
      | `Handled -> true
      | `Grab f -> st.mouse_grab <- Some f; true
    in
    let rec aux ox oy sw sh t =
      match t.desc with
      | Atom _ -> false
      | X (a, b) ->
        let aw, bw = split ~a:a.w ~sa:a.sw ~b:b.w ~sb:b.sw sw in
        if x - ox < aw
        then aux ox oy aw sh a
        else aux (ox + aw) oy bw sh b
      | Y (a, b) ->
        let ah, bh = split ~a:a.h ~sa:a.sh ~b:b.h ~sb:b.sh sh in
        if y - oy < ah
        then aux ox oy sw ah a
        else aux ox (oy + ah) sw bh b
      | Z (a, b) ->
        aux ox oy sw sh b || aux ox oy sw sh a
      | Mouse_handler (t, f) ->
        let _offsetx, rw = pack ~fixed:t.w ~stretch:t.sw sw `Negative `Negative
        and _offsety, rh = pack ~fixed:t.h ~stretch:t.sh sh `Negative `Negative
        in
        assert (_offsetx = 0 && _offsety = 0);
        (x - ox >= 0 && x - ox <= rw && y - oy >= 0 && y - oy <= rh) &&
        (aux ox oy sw sh t || handle ox oy f)
      | Size_sensor (desc, _)
      | Transient_sensor (desc, _) | Permanent_sensor (desc, _)
      | Focus_area (desc, _) ->
        aux ox oy sw sh desc
      | Shift_area (desc, sx, sy) ->
        aux (ox - sx) (oy - sy) sw sh desc
      | Resize (t, g, _bg) ->
        let open Gravity in
        let dx, rw = pack ~fixed:t.w ~stretch:t.sw sw (h (p1 g)) (h (p2 g)) in
        let dy, rh = pack ~fixed:t.h ~stretch:t.sh sh (v (p1 g)) (v (p2 g)) in
        aux (ox + dx) (oy + dy) rw rh t
      | Event_filter (n, f) ->
        begin match f (`Mouse (`Press btn, (x, y), [])) with
          | `Handled -> true
          | `Unhandled -> aux ox oy sw sh n
        end
    in
    aux 0 0 w h t

  let release_grab st x y =
    match st.mouse_grab with
    | None -> ()
    | Some (_, release) ->
      st.mouse_grab <- None;
      release ~x ~y

  let dispatch_mouse t (event, (x, y), _mods) =
    if
      match event with
      | `Press btn ->
        release_grab t x y;
        let w, h = t.size in
        dispatch_mouse t x y btn w h t.view
      | `Drag ->
        begin match t.mouse_grab with
          | None -> false
          | Some (drag, _) -> drag ~x ~y; true
        end
      | `Release ->
        release_grab t x y; true
    then `Handled
    else `Unhandled

  let resize_canvas rw rh image =
    let w = I.width image in
    let h = I.height image in
    if w <> rw || h <> rh
    then I.pad ~r:(rw - w) ~b:(rh - h) image
    else image

  let resize_canvas2 ox oy rw rh image =
    let w = I.width image in
    let h = I.height image in
    I.pad ~l:ox ~t:oy ~r:(rw - w - ox) ~b:(rh - h - oy) image

  let same_size w h image =
    w = I.width image &&
    h = I.height image

  let rec render_node vx1 vy1 vx2 vy2 sw sh t : cache =
    if
      let cache = t.cache in
      vx1 >= Interval.fst cache.vx && vy1 >= Interval.fst cache.vy &&
      vx2 <= Interval.snd cache.vx && vy2 <= Interval.snd cache.vy &&
      same_size sw sh cache.image
    then t.cache
    else if vx2 < 0 || vy2 < 0 || sw < vx1 || sh < vy1 then
      let vx = Interval.make vx1 vx2 and vy = Interval.make vy1 vy2 in
      { vx; vy; image = I.void sw sh }
    else
      let cache = match t.desc with
        | Atom image ->
          { vx = Interval.make 0 sw;
            vy = Interval.make 0 sh;
            image = resize_canvas sw sh image }
        | Size_sensor (desc, handler) ->
          handler ~w:sw ~h:sh;
          render_node vx1 vy1 vx2 vy2 sw sh desc
        | Transient_sensor (desc, _) | Permanent_sensor (desc, _) ->
          render_node vx1 vy1 vx2 vy2 sw sh desc
        | Focus_area (desc, _) | Mouse_handler (desc, _) ->
          render_node vx1 vy1 vx2 vy2 sw sh desc
        | Shift_area (t', sx, sy) ->
          let cache = render_node
              (vx1 + sx) (vy1 + sy) (vx2 + sx) (vy2 + sy) (sx + sw) (sy + sh) t'
          in
          let vx = Interval.make vx1 vx2 and vy = Interval.make vy1 vy2 in
          let image = resize_canvas sw sh (I.crop ~l:sx ~t:sy cache.image) in
          { vx; vy; image }
        | X (a, b) ->
          let aw, bw = split ~a:a.w ~sa:a.sw ~b:b.w ~sb:b.sw sw in
          let ca = render_node vx1 vy1 vx2 vy2 aw sh a in
          let cb = render_node (vx1 - aw) vy1 (vx2 - aw) vy2 bw sh b in
          let vx = Interval.make
              (maxi (Interval.fst ca.vx) (Interval.fst cb.vx + aw))
              (mini (Interval.snd ca.vx) (Interval.snd cb.vx + aw))
          and vy = Interval.make
              (maxi (Interval.fst ca.vy) (Interval.fst cb.vy))
              (mini (Interval.snd ca.vy) (Interval.snd cb.vy))
          and image = resize_canvas sw sh (I.(<|>) ca.image cb.image) in
          { vx; vy; image }
        | Y (a, b) ->
          let ah, bh = split ~a:a.h ~sa:a.sh ~b:b.h ~sb:b.sh sh in
          let ca = render_node vx1 vy1 vx2 vy2 sw ah a in
          let cb = render_node vx1 (vy1 - ah) vx2 (vy2 - ah) sw bh b in
          let vx = Interval.make
              (maxi (Interval.fst ca.vx) (Interval.fst cb.vx))
              (mini (Interval.snd ca.vx) (Interval.snd cb.vx))
          and vy = Interval.make
              (maxi (Interval.fst ca.vy) (Interval.fst cb.vy + ah))
              (mini (Interval.snd ca.vy) (Interval.snd cb.vy + ah))
          and image = resize_canvas sw sh (I.(<->) ca.image cb.image) in
          { vx; vy; image }
        | Z (a, b) ->
          let ca = render_node vx1 vy1 vx2 vy2 sw sh a in
          let cb = render_node vx1 vy1 vx2 vy2 sw sh b in
          let vx = Interval.make
              (maxi (Interval.fst ca.vx) (Interval.fst cb.vx))
              (mini (Interval.snd ca.vx) (Interval.snd cb.vx))
          and vy = Interval.make
              (maxi (Interval.fst ca.vy) (Interval.fst cb.vy))
              (mini (Interval.snd ca.vy) (Interval.snd cb.vy))
          and image = resize_canvas sw sh (I.(</>) cb.image ca.image) in
          { vx; vy; image }
        | Resize (t, g, bg) ->
          let open Gravity in
          let dx, rw = pack ~fixed:t.w ~stretch:t.sw sw (h (p1 g)) (h (p2 g)) in
          let dy, rh = pack ~fixed:t.h ~stretch:t.sh sh (v (p1 g)) (v (p2 g)) in
          let c =
            render_node (vx1 - dx) (vy1 - dy) (vx2 - dx) (vy2 - dy) rw rh t
          in
          let image = resize_canvas2 dx dy sw sh c.image in
          let image =
            if bg != A.empty then
              I.(image </> char bg ' ' sw sh)
            else
              image
          in
          let vx = Interval.shift c.vx dx in
          let vy = Interval.shift c.vy dy in
          { vx; vy; image }
        | Event_filter (t, _f) ->
          render_node vx1 vy1 vx2 vy2 sw sh t
      in
      t.cache <- cache;
      cache

  let image {size = (w, h); view; _}  =
    (render_node 0 0 w h w h view).image

  let dispatch_raw_key st key =
    let rec iter (st: ui list) : [> `Unhandled] =
      match st with
      | [] -> `Unhandled
      | ui :: tl ->
        begin match ui.desc with
          | Atom _ -> iter tl
          | X (a, b) | Y (a, b) | Z (a, b) ->
            (* Try left/top most branch first *)
            let st' =
              if Focus.has_focus b.focus
              then b :: tl
              else a :: b :: tl
            in
            iter st'
          | Focus_area (t, f) ->
            begin match iter [t] with
              | `Handled -> `Handled
              | `Unhandled ->
                match f key with
                | `Handled -> `Handled
                | `Unhandled -> iter tl
            end
          | Mouse_handler (t, _) | Size_sensor (t, _)
          | Transient_sensor (t, _) | Permanent_sensor (t, _)
          | Shift_area (t, _, _) | Resize (t, _, _) ->
            iter (t :: tl)
          | Event_filter (t, f) ->
            begin match f (`Key key) with
              | `Unhandled -> iter (t :: tl)
              | `Handled -> `Handled
            end
        end
    in
    iter [st.view]

  exception Acquired_focus

  let grab_focus ui =
    let rec aux ui =
      match ui.focus with
      | Focus.Empty -> ()
      | Focus.Handle (_, v) -> Focus.request_var v; raise Acquired_focus
      | Focus.Conflict _ -> iter aux ui
    in
    try aux ui; false with Acquired_focus -> true

  let rec dispatch_focus t dir =
    match t.desc with
    | Atom _ -> false
    | Mouse_handler (t, _) | Size_sensor (t, _)
    | Transient_sensor (t, _) | Permanent_sensor (t, _)
    | Shift_area (t, _, _) | Resize (t, _, _) | Event_filter (t, _) ->
      dispatch_focus t dir
    | Focus_area (t', _) ->
      if Focus.has_focus t'.focus then
        dispatch_focus t' dir || grab_focus t
      else if Focus.has_focus t.focus then
        false
      else
        grab_focus t
    | X (a, b) ->
      begin if Focus.has_focus a.focus then
          dispatch_focus a dir ||
          (match dir with
           | `Next | `Right -> dispatch_focus b dir
           | _ -> false
          )
        else if Focus.has_focus b.focus then
          dispatch_focus b dir ||
          (match dir with
           | `Prev | `Left -> dispatch_focus a dir
           | _ -> false
          )
        else
          match dir with
          | `Prev | `Left | `Up -> dispatch_focus b dir || dispatch_focus a dir
          | `Next | `Down | `Right -> dispatch_focus a dir || dispatch_focus b dir
      end
    | Y (a, b) ->
      begin if Focus.has_focus a.focus then
          dispatch_focus a dir ||
          (match dir with
           | `Next | `Down -> dispatch_focus b dir
           | _ -> false
          )
        else if Focus.has_focus b.focus then
          dispatch_focus b dir ||
          (match dir with
           | `Prev | `Up -> dispatch_focus a dir
           | _ -> false
          )
        else match dir with
          | `Prev | `Up -> dispatch_focus b dir || dispatch_focus a dir
          | `Next | `Left | `Down | `Right -> dispatch_focus a dir || dispatch_focus b dir
      end
    | Z (a, b) ->
      if Focus.has_focus a.focus then
        dispatch_focus a dir
      else
        dispatch_focus b dir || dispatch_focus a dir

  let rec dispatch_key st key =
    match dispatch_raw_key st key, key with
    | `Handled, _ -> `Handled
    | `Unhandled, (`Arrow dir, [`Meta]) ->
      let dir : [`Down | `Left | `Right | `Up] :>
          [`Down | `Left | `Right | `Up | `Next | `Prev] = dir in
      dispatch_key st (`Focus dir, [`Meta])
    | `Unhandled, (`Tab, mods) ->
      let dir = if List.mem `Shift mods then `Prev else `Next in
      dispatch_key st (`Focus dir, mods)
    | `Unhandled, (`Focus dir, _) ->
      if dispatch_focus st.view dir then `Handled else `Unhandled
    | `Unhandled, _ -> `Unhandled

  let dispatch_event t = function
    | `Key key -> dispatch_key t key
    | `Mouse mouse -> dispatch_mouse t mouse
    | `Paste _ -> `Unhandled
end

module Ui_loop =
struct
  open Notty_unix

  (* FIXME Uses of [quick_sample] and [quick_release] should be replaced by
           [sample] and [release] with the appropriate release management. *)

  let step ?(process_event=true) ?(timeout=(-1.0)) ~renderer term root =
    let size = Term.size term in
    let image =
      let rec stabilize () =
        let tree = Lwd.quick_sample root in
        Renderer.update renderer size tree;
        let image = Renderer.image renderer in
        if Lwd.is_damaged root
        then stabilize ()
        else image
      in
      stabilize ()
    in
    Term.image term image;
    if process_event then
      let i, _ = Term.fds term in
      let has_event =
        let rec select () =
          match Unix.select [i] [] [i] timeout with
          | [], [], [] -> false
          | _ -> true
          | exception (Unix.Unix_error (Unix.EINTR, _, _)) -> select ()
        in
        select ()
      in
      if has_event then
        match Term.event term with
        | `End -> ()
        | `Resize _ -> ()
        | #Unescape.event as event ->
          let event = (event : Unescape.event :> Ui.event) in
          ignore (Renderer.dispatch_event renderer event : [`Handled | `Unhandled])

  let run_with_term term ?tick_period ?(tick=ignore) ~renderer quit t =
    let quit = Lwd.observe (Lwd.get quit) in
    let root = Lwd.observe t in
    let rec loop () =
      let quit = Lwd.quick_sample quit in
      if not quit then (
        step ~process_event:true ?timeout:tick_period ~renderer term root;
        tick ();
        loop ()
      )
    in
    loop ();
    ignore (Lwd.quick_release root);
    ignore (Lwd.quick_release quit)

  let run ?tick_period ?tick ?term ?(renderer=Renderer.make ())
          ?quit ?(quit_on_escape=true) ?(quit_on_ctrl_q=true) t =
    let quit = match quit with
      | Some quit -> quit
      | None -> Lwd.var false
    in
    let t = Lwd.map t ~f:(Ui.event_filter (function
        | `Key (`ASCII 'Q', [`Ctrl]) when quit_on_ctrl_q ->
          Lwd.set quit true; `Handled
        | `Key (`Escape, []) when quit_on_escape ->
          Lwd.set quit true; `Handled
        | _ -> `Unhandled
      ))
    in
    match term with
    | Some term -> run_with_term term ?tick_period ?tick ~renderer quit t
    | None ->
      let term = Term.create () in
      run_with_term term ?tick_period ?tick ~renderer quit t;
      Term.release term

end
