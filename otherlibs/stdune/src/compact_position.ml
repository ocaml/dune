module Position = struct
  (* We encode the position in three, 21 bit fields: [cnum][lnum][bol] *)
  type t = int

  let field_size = 21
  let field_mask = (1 lsl field_size) - 1
  let shift_bol = 0
  let shift_lnum = field_size
  let shift_cnum = 2 * field_size

  let small_enough =
    let max_size = 1 lsl field_size in
    let test int = int <= max_size in
    fun [@inline] { Lexing.pos_bol; pos_cnum; pos_lnum; pos_fname = _ } ->
      test pos_bol && test pos_cnum && test pos_lnum
  ;;

  let[@inline] of_position { Lexing.pos_bol; pos_cnum; pos_lnum; pos_fname = _ } =
    ((pos_bol land field_mask) lsl shift_bol)
    lor ((pos_lnum land field_mask) lsl shift_lnum)
    lor ((pos_cnum land field_mask) lsl shift_cnum)
  ;;

  let[@inline] bol t = (t lsr shift_bol) land field_mask
  let[@inline] lnum t = (t lsr shift_lnum) land field_mask
  let[@inline] cnum t = (t lsr shift_cnum) land field_mask

  let to_position t ~fname:pos_fname =
    let pos_bol = bol t in
    let pos_cnum = cnum t in
    let pos_lnum = lnum t in
    { Lexing.pos_bol; pos_cnum; pos_lnum; pos_fname }
  ;;
end

module Same_line_loc = struct
  (* we encode the location in four, 15 bit chunks
     [bol][lnum][start_cnum][stop_cnum]

     Note that this leaves us with 3 spare bits. We should probably use them to
     expand [bol] and [lnum] a little.

     CR-someday jtov: Instead of [stop_cnum], we can store [stop_cnum -
     start_cnum]. This should be smaller than [stop_cnum] and release more
     bits for other fields.
  *)
  type t = int

  let field_size = 15
  let field_mask = (1 lsl field_size) - 1
  let shift_bol = 0
  let shift_lnum = field_size
  let shift_start_cnum = 2 * field_size
  let shift_stop_cnum = 3 * field_size

  let create ~bol ~lnum ~start_cnum ~stop_cnum =
    ((bol land field_mask) lsl shift_bol)
    lor ((lnum land field_mask) lsl shift_lnum)
    lor ((start_cnum land field_mask) lsl shift_start_cnum)
    lor ((stop_cnum land field_mask) lsl shift_stop_cnum)
  ;;

  let[@inline] bol t = (t lsr shift_bol) land field_mask
  let[@inline] lnum t = (t lsr shift_lnum) land field_mask
  let[@inline] start_cnum t = (t lsr shift_start_cnum) land field_mask
  let[@inline] stop_cnum t = (t lsr shift_stop_cnum) land field_mask

  let set_start_to_stop t =
    let bol = bol t in
    let lnum = lnum t in
    let stop_cnum = stop_cnum t in
    (* this can be optimized more if necessary *)
    create ~bol ~lnum ~start_cnum:stop_cnum ~stop_cnum
  ;;

  let small_enough =
    let max_size = 1 lsl field_size in
    fun [@inline] int -> int <= max_size
  ;;

  let[@inline] to_loc t ~fname:pos_fname =
    let pos_lnum = lnum t in
    let pos_bol = bol t in
    let start = { Lexing.pos_fname; pos_lnum; pos_bol; pos_cnum = start_cnum t } in
    let stop = { start with pos_cnum = stop_cnum t } in
    { Lexbuf.Loc.start; stop }
  ;;

  let[@inline] start t ~fname:pos_fname =
    let pos_lnum = lnum t in
    let pos_bol = bol t in
    { Lexing.pos_fname; pos_lnum; pos_bol; pos_cnum = start_cnum t }
  ;;

  let[@inline] stop t ~fname:pos_fname =
    let pos_lnum = lnum t in
    let pos_bol = bol t in
    { Lexing.pos_fname; pos_lnum; pos_bol; pos_cnum = stop_cnum t }
  ;;
end

include Position

type of_loc =
  | Same_line of Same_line_loc.t
  | Loc of
      { start : t
      ; stop : t
      }
  | Loc_does_not_fit

let[@inline] try_loc { Lexbuf.Loc.start; stop } =
  if Position.small_enough start && Position.small_enough stop
  then (
    let start = Position.of_position start in
    let stop = Position.of_position stop in
    Loc { start; stop })
  else Loc_does_not_fit
;;

let[@inline] of_loc ({ Lexbuf.Loc.start; stop } as loc) =
  if start.pos_fname <> stop.pos_fname
  then Loc_does_not_fit
  else if start.pos_bol = stop.pos_bol && start.pos_lnum = stop.pos_lnum
  then (
    let bol = start.pos_bol in
    let lnum = start.pos_lnum in
    let start_cnum = start.pos_cnum in
    let stop_cnum = stop.pos_cnum in
    let test = Same_line_loc.small_enough in
    if test bol && test lnum && test start_cnum && test stop_cnum
    then Same_line (Same_line_loc.create ~bol ~lnum ~start_cnum ~stop_cnum)
    else try_loc loc)
  else try_loc loc
;;

let of_loc = if Sys.int_size = 63 then of_loc else fun _ -> Loc_does_not_fit

module For_tests = struct
  let small_enough = small_enough
end
