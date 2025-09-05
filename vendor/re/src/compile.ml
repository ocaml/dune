open Import

let rec iter n f v = if Int.equal n 0 then v else iter (n - 1) f (f v)

module Idx : sig
  type t [@@immediate]

  val unknown : t
  val make_break : Automata.Idx.t -> t
  val of_idx : Automata.Idx.t -> t
  val is_idx : t -> bool
  val is_break : t -> bool
  val idx : t -> int
  val break_idx : t -> int
end = struct
  type t = int

  let unknown = -2
  let break = -3
  let of_idx (x : Automata.Idx.t) = Automata.Idx.to_int x [@@inline always]
  let is_idx t = t >= 0 [@@inline always]
  let is_break x = x <= break [@@inline always]
  let idx t = t [@@inline always]
  let make_break (idx : Automata.Idx.t) = -5 - Automata.Idx.to_int idx [@@inline always]
  let break_idx t = (t + 5) * -1 [@@inline always]
end

type match_info =
  | Match of Group.t
  | Failed
  | Running of { no_match_starts_before : int }

type state_info =
  { idx : Idx.t
  ; (* Index of the current position in the position table.
       Not yet computed transitions point to a dummy state where
       [idx] is set to [unknown];
       If [idx] is set to [break] for states that either always
       succeed or always fail. *)
    mutable final : (Category.t * (Automata.Idx.t * Automata.Status.t)) list
  ; (* Mapping from the category of the next character to
       - the index where the next position should be saved
       - possibly, the list of marks (and the corresponding indices)
         corresponding to the best match *)
    desc : Automata.State.t (* Description of this state of the automata *)
  }

(* A state [t] is a pair composed of some information about the
   state [state_info] and a transition table [t array], indexed by
   color. For performance reason, to avoid an indirection, we manually
   unbox the transition table: we allocate a single array, with the
   state information at index 0, followed by the transitions. *)
module State : sig
  type t

  val make : ncol:int -> state_info -> t
  val make_break : state_info -> t
  val get_info : t -> state_info
  val follow_transition : t -> color:Cset.c -> t
  val set_transition : t -> color:Cset.c -> t -> unit
end = struct
  type t = Table of t array [@@unboxed]

  let get_info (Table st) : state_info = Obj.magic (Array.unsafe_get st 0)
  [@@inline always]
  ;;

  let set_info (Table st) (info : state_info) = st.(0) <- Obj.magic info

  let follow_transition (Table st) ~color = Array.unsafe_get st (1 + Cset.to_int color)
  [@@inline always]
  ;;

  let set_transition (Table st) ~color st' = st.(1 + Cset.to_int color) <- st'
  let dummy (info : state_info) = Table [| Obj.magic info |]
  let unknown_state = dummy { idx = Idx.unknown; final = []; desc = Automata.State.dummy }

  let make ~ncol state =
    let st = Table (Array.make (ncol + 1) unknown_state) in
    set_info st state;
    st
  ;;

  let make_break state = Table [| Obj.magic state |]
end

(* Automata (compiled regular expression) *)
type re =
  { initial : Automata.expr
  ; (* The whole regular expression *)
    mutable initial_states : (Category.t * State.t) list
  ; (* Initial states, indexed by initial category *)
    colors : Color_map.Table.t
  ; (* Color table *)
    color_repr : Color_map.Repr.t
  ; (* Table from colors to one character of this color *)
    ncolor : int
  ; (* Number of colors. *)
    lnl : Cset.c
  ; (* Color of the last newline. [Cset.null_char] if unnecessary *)
    tbl : Automata.Working_area.t
  ; (* Temporary table used to compute the first available index
       when computing a new state *)
    states : State.t Automata.State.Table.t
  ; (* States of the deterministic automata *)
    group_names : (string * int) list
  ; (* Named groups in the regular expression *)
    group_count : int (* Number of groups in the regular expression *)
  }

let pp_re ch re = Automata.pp ch re.initial
let group_count re = re.group_count
let group_names re = re.group_names

module Positions = struct
  (* Information used during matching *)
  type t =
    { mutable positions : int array
    (* Array of mark positions
       The mark are off by one for performance reasons *)
    }

  let empty = { positions = [||] }
  let length t = Array.length t.positions
  let set t idx pos = Array.unsafe_set t.positions idx pos

  let resize t =
    let len = Array.length t.positions in
    let pos = t.positions in
    t.positions <- Array.make (2 * len) 0;
    Array.blit pos 0 t.positions 0 len
  ;;

  let all t = t.positions
  let first t = t.positions.(0)

  let make ~groups re =
    if groups
    then
      { positions =
          (let n = Automata.Working_area.index_count re.tbl + 1 in
           if n <= 10 then [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |] else Array.make n 0)
      }
    else empty
  ;;
end

(****)

let category re ~color =
  if Cset.equal_c color Cset.null_char
  then Category.inexistant (* Special category for the last newline *)
  else if Cset.equal_c color re.lnl
  then Category.(lastnewline ++ newline ++ not_letter)
  else Category.from_char (Color_map.Repr.repr re.color_repr color)
;;

(****)

let find_state re desc =
  try Automata.State.Table.find re.states desc with
  | Not_found ->
    let st =
      let break_state =
        match Automata.State.status desc with
        | Running -> false
        | Failed | Match _ -> true
      in
      let st =
        { idx =
            (let idx = Automata.State.idx desc in
             if break_state then Idx.make_break idx else Idx.of_idx idx)
        ; final = []
        ; desc
        }
      in
      if break_state then State.make_break st else State.make ~ncol:re.ncolor st
    in
    Automata.State.Table.add re.states desc st;
    st
;;

(**** Match with marks ****)

let delta re positions cat ~color st =
  let desc = Automata.delta re.tbl cat color st.desc in
  let len = Positions.length positions in
  if len > 0 && Automata.State.idx desc |> Automata.Idx.to_int = len
  then Positions.resize positions;
  desc
;;

let validate re positions (s : string) ~pos st =
  let color = Color_map.Table.get re.colors s.[pos] in
  let st' =
    let desc' =
      let cat = category re ~color in
      delta re positions cat ~color (State.get_info st)
    in
    find_state re desc'
  in
  State.set_transition st ~color st'
;;

let next colors st s pos =
  State.follow_transition st ~color:(Color_map.Table.get colors (String.unsafe_get s pos))
;;

let rec loop re ~colors ~positions s ~pos ~last st0 st =
  if pos < last
  then (
    let st' = next colors st s pos in
    let idx = (State.get_info st').idx in
    if Idx.is_idx idx
    then (
      Positions.set positions (Idx.idx idx) pos;
      loop re ~colors ~positions s ~pos:(pos + 1) ~last st' st')
    else if Idx.is_break idx
    then (
      Positions.set positions (Idx.break_idx idx) pos;
      st')
    else (
      (* Unknown *)
      validate re positions s ~pos st0;
      loop re ~colors ~positions s ~pos ~last st0 st0))
  else st
;;

let rec loop_no_mark re ~colors s ~pos ~last st0 st =
  if pos < last
  then (
    let st' = next colors st s pos in
    let idx = (State.get_info st').idx in
    if Idx.is_idx idx
    then loop_no_mark re ~colors s ~pos:(pos + 1) ~last st' st'
    else if Idx.is_break idx
    then st'
    else (
      (* Unknown *)
      validate re Positions.empty s ~pos st0;
      loop_no_mark re ~colors s ~pos ~last st0 st0))
  else st
;;

let final re positions st cat =
  try List.assq cat st.final with
  | Not_found ->
    let st' = delta re positions cat ~color:Cset.null_char st in
    let res = Automata.State.idx st', Automata.State.status st' in
    st.final <- (cat, res) :: st.final;
    res
;;

let find_initial_state re cat =
  try List.assq cat re.initial_states with
  | Not_found ->
    let st = find_state re (Automata.State.create cat re.initial) in
    re.initial_states <- (cat, st) :: re.initial_states;
    st
;;

let get_color re (s : string) pos =
  if pos < 0
  then Cset.null_char
  else (
    let slen = String.length s in
    if pos >= slen
    then Cset.null_char
    else if pos = slen - 1
            && (not (Cset.equal_c re.lnl Cset.null_char))
            && Char.equal (String.unsafe_get s pos) '\n'
    then (* Special case for the last newline *)
      re.lnl
    else Color_map.Table.get re.colors (String.unsafe_get s pos))
;;

let rec handle_last_newline re positions ~pos st ~groups =
  let st' = State.follow_transition st ~color:re.lnl in
  let info = State.get_info st' in
  if Idx.is_idx info.idx
  then (
    if groups then Positions.set positions (Idx.idx info.idx) pos;
    st')
  else if Idx.is_break info.idx
  then (
    if groups then Positions.set positions (Idx.break_idx info.idx) pos;
    st')
  else (
    (* Unknown *)
    let color = re.lnl in
    let st' =
      let desc =
        let cat = category re ~color in
        let real_c = Color_map.Table.get re.colors '\n' in
        delta re positions cat ~color:real_c (State.get_info st)
      in
      find_state re desc
    in
    State.set_transition st ~color st';
    handle_last_newline re positions ~pos st ~groups)
;;

let rec scan_str re positions (s : string) initial_state ~last ~pos ~groups =
  if last = String.length s
     && (not (Cset.equal_c re.lnl Cset.null_char))
     && last > pos
     && Char.equal (String.get s (last - 1)) '\n'
  then (
    let last = last - 1 in
    let st = scan_str re positions ~pos s initial_state ~last ~groups in
    if Idx.is_break (State.get_info st).idx
    then st
    else handle_last_newline re positions ~pos:last st ~groups)
  else if groups
  then loop re ~colors:re.colors ~positions s ~pos ~last initial_state initial_state
  else loop_no_mark re ~colors:re.colors s ~pos ~last initial_state initial_state
;;

(* This function adds a final boundary check on the input.
   This is useful to indicate that the output failed because
   of insufficient input, or to verify that the output actually
   matches for regex that have boundary conditions with respect
   to the input string.
*)
let final_boundary_check re positions ~last ~slen s state_info ~groups =
  let idx, res =
    let final_cat =
      Category.(
        search_boundary
        ++ if last = slen then inexistant else category re ~color:(get_color re s last))
    in
    final re positions state_info final_cat
  in
  (match groups, res with
   | true, Match _ -> Positions.set positions (Automata.Idx.to_int idx) last
   | _ -> ());
  res
;;

let make_match_str re positions ~len ~groups ~partial s ~pos =
  let slen = String.length s in
  let last = if len = -1 then slen else pos + len in
  let st =
    let initial_state =
      let initial_cat =
        Category.(
          search_boundary
          ++ if pos = 0 then inexistant else category re ~color:(get_color re s (pos - 1)))
      in
      find_initial_state re initial_cat
    in
    scan_str re positions s initial_state ~pos ~last ~groups
  in
  let state_info = State.get_info st in
  if Idx.is_break state_info.idx || (partial && not groups)
  then Automata.State.status state_info.desc
  else if partial && groups
  then (
    match Automata.State.status state_info.desc with
    | (Match _ | Failed) as status -> status
    | Running ->
      (* This could be because it's still not fully matched, or it
         could be that because we need to run special end of input
         checks. *)
      (match final_boundary_check re positions ~last ~slen s state_info ~groups with
       | Match _ as status -> status
       | Failed | Running ->
         (* A failure here just means that we need more data, i.e.
            it's a partial match. *)
         Running))
  else final_boundary_check re positions ~last ~slen s state_info ~groups
;;

let match_str_no_bounds ~groups ~partial re s ~pos ~len =
  let positions = Positions.make ~groups re in
  match make_match_str re positions ~len ~groups ~partial s ~pos with
  | Match (marks, pmarks) ->
    Match
      (Group.create s marks pmarks ~gpos:(Positions.all positions) ~gcount:re.group_count)
  | Failed -> Failed
  | Running ->
    let no_match_starts_before = if groups then Positions.first positions else 0 in
    Running { no_match_starts_before }
;;

let match_str_p re s ~pos ~len =
  if pos < 0 || len < -1 || pos + len > String.length s
  then invalid_arg "Re.exec: out of bounds";
  match make_match_str re Positions.empty ~len ~groups:false ~partial:false s ~pos with
  | Match _ -> true
  | _ -> false
;;

let match_str ~groups ~partial re s ~pos ~len =
  if pos < 0 || len < -1 || pos + len > String.length s
  then invalid_arg "Re.exec: out of bounds";
  match_str_no_bounds ~groups ~partial re s ~pos ~len
;;

let mk_re ~initial ~colors ~color_repr ~ncolor ~lnl ~group_names ~group_count =
  { initial
  ; initial_states = []
  ; colors
  ; color_repr
  ; ncolor
  ; lnl
  ; tbl = Automata.Working_area.create ()
  ; states = Automata.State.Table.create 97
  ; group_names
  ; group_count
  }
;;

(**** Compilation ****)

module A = Automata

let enforce_kind ids kind kind' cr =
  match kind, kind' with
  | `First, `First -> cr
  | `First, k -> A.seq ids k cr (A.eps ids)
  | _ -> cr
;;

type context =
  { ids : A.Ids.t
  ; kind : A.Sem.t
  ; ign_group : bool
  ; greedy : A.Rep_kind.t
  ; pos : A.Mark.t ref
  ; names : (string * int) list ref
  ; cache : Cset.t Cset.CSetMap.t ref
  ; colors : Color_map.Table.t
  }

let trans_set cache (cm : Color_map.Table.t) s =
  match Cset.one_char s with
  | Some i -> Cset.csingle (Color_map.Table.get_char cm i)
  | None ->
    let v = Cset.hash s, s in
    (try Cset.CSetMap.find v !cache with
     | Not_found ->
       let l = Color_map.Table.translate_colors cm s in
       cache := Cset.CSetMap.add v l !cache;
       l)
;;

let make_repeater ids cr kind greedy =
  match greedy with
  | `Greedy -> fun rem -> A.alt ids [ A.seq ids kind (A.rename ids cr) rem; A.eps ids ]
  | `Non_greedy ->
    fun rem -> A.alt ids [ A.eps ids; A.seq ids kind (A.rename ids cr) rem ]
;;

(* XXX should probably compute a category mask *)
let rec translate
  ({ ids; kind; ign_group; greedy; pos; names; cache; colors } as ctx)
  (ast : Ast.no_case)
  =
  match ast with
  | Set s -> A.cst ids (trans_set cache colors s), kind
  | Sequence l -> trans_seq ctx l, kind
  | Ast (Alternative l) ->
    (match Ast.merge_sequences l with
     | [ r' ] ->
       let cr, kind' = translate ctx r' in
       enforce_kind ids kind kind' cr, kind
     | merged_sequences ->
       ( A.alt
           ids
           (List.map merged_sequences ~f:(fun r' ->
              let cr, kind' = translate ctx r' in
              enforce_kind ids kind kind' cr))
       , kind ))
  | Repeat (r', i, j) ->
    let cr, kind' = translate ctx r' in
    let rem =
      match j with
      | None -> A.rep ids greedy kind' cr
      | Some j ->
        let f = make_repeater ids cr kind' greedy in
        iter (j - i) f (A.eps ids)
    in
    iter i (fun rem -> A.seq ids kind' (A.rename ids cr) rem) rem, kind
  | Beg_of_line -> A.after ids Category.(inexistant ++ newline), kind
  | End_of_line -> A.before ids Category.(inexistant ++ newline), kind
  | Beg_of_word ->
    ( A.seq
        ids
        `First
        (A.after ids Category.(inexistant ++ not_letter))
        (A.before ids Category.letter)
    , kind )
  | End_of_word ->
    ( A.seq
        ids
        `First
        (A.after ids Category.letter)
        (A.before ids Category.(inexistant ++ not_letter))
    , kind )
  | Not_bound ->
    ( A.alt
        ids
        [ A.seq ids `First (A.after ids Category.letter) (A.before ids Category.letter)
        ; (let cat = Category.(inexistant ++ not_letter) in
           A.seq ids `First (A.after ids cat) (A.before ids cat))
        ]
    , kind )
  | Beg_of_str -> A.after ids Category.inexistant, kind
  | End_of_str -> A.before ids Category.inexistant, kind
  | Last_end_of_line -> A.before ids Category.(inexistant ++ lastnewline), kind
  | Start -> A.after ids Category.search_boundary, kind
  | Stop -> A.before ids Category.search_boundary, kind
  | Sem (kind', r') ->
    let cr, kind'' = translate { ctx with kind = kind' } r' in
    enforce_kind ids kind' kind'' cr, kind'
  | Sem_greedy (greedy', r') -> translate { ctx with greedy = greedy' } r'
  | Group (n, r') ->
    if ign_group
    then translate ctx r'
    else (
      let p = !pos in
      let () =
        match n with
        | Some name -> names := (name, A.Mark.group_count p) :: !names
        | None -> ()
      in
      pos := A.Mark.next2 !pos;
      let cr, kind' = translate ctx r' in
      ( A.seq ids `First (A.mark ids p) (A.seq ids `First cr (A.mark ids (A.Mark.next p)))
      , kind' ))
  | No_group r' -> translate { ctx with ign_group = true } r'
  | Nest r' ->
    let b = !pos in
    let cr, kind' = translate ctx r' in
    let e = A.Mark.prev !pos in
    if A.Mark.compare e b = -1
    then cr, kind'
    else A.seq ids `First (A.erase ids b e) cr, kind'
  | Pmark (i, r') ->
    let cr, kind' = translate ctx r' in
    A.seq ids `First (A.pmark ids i) cr, kind'

and trans_seq ({ ids; kind; _ } as ctx) = function
  | [] -> A.eps ids
  | [ r ] ->
    let cr', kind' = translate ctx r in
    enforce_kind ids kind kind' cr'
  | r :: rem ->
    let cr', kind' = translate ctx r in
    let cr'' = trans_seq ctx rem in
    if A.is_eps cr'' then cr' else if A.is_eps cr' then cr'' else A.seq ids kind' cr' cr''
;;

let compile_1 regexp =
  let regexp = Ast.handle_case false regexp in
  let color_map = Color_map.make () in
  let need_lnl = Ast.colorize color_map regexp in
  let colors, color_repr = Color_map.flatten color_map in
  let ncolor = Color_map.Repr.length color_repr in
  let lnl = if need_lnl then Cset.of_int ncolor else Cset.null_char in
  let ncolor = if need_lnl then ncolor + 1 else ncolor in
  let ctx =
    { ids = A.Ids.create ()
    ; kind = `First
    ; ign_group = false
    ; greedy = `Greedy
    ; pos = ref A.Mark.start
    ; names = ref []
    ; cache = ref Cset.CSetMap.empty
    ; colors
    }
  in
  let r, kind = translate ctx regexp in
  let r = enforce_kind ctx.ids `First kind r in
  (*Format.eprintf "<%d %d>@." !ids ncol;*)
  mk_re
    ~initial:r
    ~colors
    ~color_repr
    ~ncolor
    ~lnl
    ~group_names:(List.rev !(ctx.names))
    ~group_count:(A.Mark.group_count !(ctx.pos))
;;

let compile r =
  let open Ast.Export in
  compile_1 (if Ast.anchored r then group r else seq [ shortest (rep any); group r ])
;;
