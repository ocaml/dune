open Import

module State = struct
  type t =
    { width : int
    ; height : int
    ; x : int
    ; y : int
    }

  let init = { width = 0; height = 0; x = 0; y = 0 }
end

let adjust_offset visible total off =
  let off = if off + visible > total then total - visible else off in
  let off = if off < 0 then 0 else off in
  off
;;

let decr_if x cond = if cond then x - 1 else x
let scrollbar_bg = Notty.A.gray 4
let scrollbar_fg = Notty.A.gray 7
let scrollbar_click_step = 3 (* Clicking scrolls one third of the screen *)
let scrollbar_wheel_step = 8 (* Wheel event scrolls 1/8th of the screen *)

let scroll visible offset ~set ~dir =
  let dir =
    match dir with
    | `Down -> 1
    | `Up -> -1
  in
  set (offset + (dir * max 1 (visible / scrollbar_wheel_step)))
;;

let hscrollbar visible total offset ~set =
  let prefix = offset * visible / total in
  let suffix = (total - offset - visible) * visible / total in
  let handle = visible - prefix - suffix in
  let render size color = Ui.atom Notty.(I.char (A.bg color) ' ' size 1) in
  let mouse_handler ~x ~y:_ = function
    | `Left ->
      if x < prefix
      then (
        set (offset - max 1 (visible / scrollbar_click_step));
        `Handled)
      else if x > prefix + handle
      then (
        set (offset + max 1 (visible / scrollbar_click_step));
        `Handled)
      else
        `Grab
          ( (fun ~x:x' ~y:_ -> set (offset + ((x' - x) * total / visible)))
          , fun ~x:_ ~y:_ -> () )
    | `Scroll dir ->
      scroll visible offset ~set ~dir;
      `Handled
    | _ -> `Unhandled
  in
  let ( ++ ) = Ui.join_x in
  Ui.mouse_area
    mouse_handler
    (render prefix scrollbar_bg
     ++ render handle scrollbar_fg
     ++ render suffix scrollbar_bg)
;;

let vscrollbar visible total offset ~set =
  let prefix = offset * visible / total in
  let suffix = (total - offset - visible) * visible / total in
  let handle = visible - prefix - suffix in
  let render size color = Ui.atom Notty.(I.char (A.bg color) ' ' 1 size) in
  let mouse_handler ~x:_ ~y = function
    | `Left ->
      if y < prefix
      then (
        set (offset - max 1 (visible / scrollbar_click_step));
        `Handled)
      else if y > prefix + handle
      then (
        set (offset + max 1 (visible / scrollbar_click_step));
        `Handled)
      else
        `Grab
          ( (fun ~x:_ ~y:y' -> set (offset + ((y' - y) * total / visible)))
          , fun ~x:_ ~y:_ -> () )
    | `Scroll dir ->
      scroll visible offset ~set ~dir;
      `Handled
    | _ -> `Unhandled
  in
  let ( ++ ) = Ui.join_y in
  Ui.mouse_area
    mouse_handler
    (render prefix scrollbar_bg
     ++ render handle scrollbar_fg
     ++ render suffix scrollbar_bg)
;;

type t =
  { ui : Ui.t
  ; vscroll : dir:[ `Down | `Up ] -> unit
  ; hscroll : dir:[ `Right | `Left ] -> unit
  }

let make (state_var : State.t Lwd.var) t =
  (* Keep track of size available for display *)
  let update_size ~w:width ~h:height =
    let state = Lwd.peek state_var in
    if state.width <> width || state.height <> height
    then Lwd.set state_var { state with width; height }
  in
  let measure_size body =
    Ui.size_sensor update_size (Ui.resize ~w:0 ~h:0 ~sw:1 ~sh:1 body)
  in
  (* Given body and state, composite scroll bars *)
  let compose_bars body (state : State.t) =
    let bw, bh = Ui.layout_width body, Ui.layout_height body in
    (* Logic to determine which scroll bar should be visible *)
    let hvisible = state.width < bw
    and vvisible = state.height < bh in
    let hvisible = hvisible || (vvisible && state.width = bw) in
    let vvisible = vvisible || (hvisible && state.height = bh) in
    (* Compute size and offsets based on visibility *)
    let state_w = decr_if state.width vvisible in
    let state_h = decr_if state.height hvisible in
    let state_x = adjust_offset state_w bw state.x in
    let state_y = adjust_offset state_h bh state.y in
    (* Composite visible scroll bars *)
    let crop b = Ui.resize ~sw:1 ~sh:1 ~w:0 ~h:0 (Ui.shift_area state_x state_y b) in
    let set_vscroll y =
      let state = Lwd.peek state_var in
      if state.y <> y then Lwd.set state_var { state with y }
    in
    let set_hscroll x =
      let state = Lwd.peek state_var in
      if state.x <> x then Lwd.set state_var { state with x }
    in
    let ( <-> ) = Ui.join_y
    and ( <|> ) = Ui.join_x in
    ( (match hvisible, vvisible with
       | false, false -> body
       | false, true -> crop body <|> vscrollbar state_h bh state_y ~set:set_vscroll
       | true, false -> crop body <-> hscrollbar state_w bw state_x ~set:set_hscroll
       | true, true ->
         crop body
         <|> vscrollbar state_h bh state_y ~set:set_vscroll
         <-> (hscrollbar state_w bw state_x ~set:set_hscroll <|> Ui.space 1 1))
    , scroll state_h state_y ~set:set_vscroll
    , scroll state_w state_x ~set:set_hscroll )
  in
  let open Lwd.O in
  let+ ui = t
  and+ size = Lwd.get state_var in
  (* Render final box *)
  let box, vscroll, hscroll = compose_bars ui size in
  let hscroll ~dir =
    hscroll
      ~dir:
        (match dir with
         | `Right -> `Down
         | `Left -> `Up)
  in
  { ui = measure_size box; vscroll; hscroll }
;;
