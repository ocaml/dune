(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

module Re = Core

exception Parse_error

type enclosed =
  | Char of char
  | Range of char * char

type piece =
  | Exactly of char
  | Any_of of enclosed list
  | Any_but of enclosed list
  | One
  | Many
  | ManyMany

type t = piece list

let of_string ~double_asterisk s : t =
  let buf = Parse_buffer.create s in
  let eos () = Parse_buffer.eos buf in
  let read c = Parse_buffer.accept buf c in
  let char () =
    ignore (read '\\' : bool);
    if eos () then raise Parse_error;
    Parse_buffer.get buf
  in
  let enclosed () : enclosed list =
    let rec loop s =
      (* This returns the list in reverse order, but order isn't important
         anyway *)
      if s <> [] && read ']'
      then s
      else (
        let c = char () in
        if not (read '-')
        then loop (Char c :: s)
        else if read ']'
        then Char c :: Char '-' :: s
        else (
          let c' = char () in
          loop (Range (c, c') :: s)))
    in
    loop []
  in
  let piece acc =
    if double_asterisk && Parse_buffer.accept_s buf "/**"
    then ManyMany :: (if eos () then Exactly '/' :: acc else acc)
    else if read '*'
    then (if double_asterisk && read '*' then ManyMany else Many) :: acc
    else if read '?'
    then One :: acc
    else if not (read '[')
    then Exactly (char ()) :: acc
    else if read '^' || read '!'
    then Any_but (enclosed ()) :: acc
    else Any_of (enclosed ()) :: acc
  in
  let rec loop pieces = if eos () then List.rev pieces else loop (piece pieces) in
  loop []
;;

let mul l l' = List.flatten (List.map (fun s -> List.map (fun s' -> s ^ s') l') l)

let explode str =
  let l = String.length str in
  let rec expl inner s i acc beg =
    if i >= l
    then (
      if inner then raise Parse_error;
      mul beg [ String.sub str s (i - s) ], i)
    else (
      match str.[i] with
      | '\\' -> expl inner s (i + 2) acc beg
      | '{' ->
        let t, i' = expl true (i + 1) (i + 1) [] [ "" ] in
        expl inner i' i' acc (mul beg (mul [ String.sub str s (i - s) ] t))
      | ',' when inner ->
        expl inner (i + 1) (i + 1) (mul beg [ String.sub str s (i - s) ] @ acc) [ "" ]
      | '}' when inner -> mul beg [ String.sub str s (i - s) ] @ acc, i + 1
      | _ -> expl inner s (i + 1) acc beg)
  in
  List.rev (fst (expl false 0 0 [] [ "" ]))
;;

module State = struct
  type t =
    { re_pieces : Re.t list (* last piece at head of list. *)
    ; remaining : piece list (* last piece at tail of list. *)
    ; am_at_start_of_pattern : bool (* true at start of pattern *)
    ; am_at_start_of_component : bool
        (* true at start of pattern or immediately
           after '/' *)
    ; pathname : bool
    ; match_backslashes : bool
    ; period : bool
    }

  let create ~period ~pathname ~match_backslashes remaining =
    { re_pieces = []
    ; am_at_start_of_pattern = true
    ; am_at_start_of_component = true
    ; pathname
    ; match_backslashes
    ; period
    ; remaining
    }
  ;;

  let explicit_period t =
    t.period && (t.am_at_start_of_pattern || (t.am_at_start_of_component && t.pathname))
  ;;

  let explicit_slash t = t.pathname
  let slashes t = if t.match_backslashes then [ '/'; '\\' ] else [ '/' ]

  let append ?(am_at_start_of_component = false) t piece =
    { t with
      re_pieces = piece :: t.re_pieces
    ; am_at_start_of_pattern = false
    ; am_at_start_of_component
    }
  ;;

  let to_re t = Re.seq (List.rev t.re_pieces)

  let next t =
    match t.remaining with
    | [] -> None
    | piece :: remaining -> Some (piece, { t with remaining })
  ;;
end

let one ~explicit_slash ~slashes ~explicit_period =
  Re.compl
    (List.concat
       [ (if explicit_slash then List.map Re.char slashes else [])
       ; (if explicit_period then [ Re.char '.' ] else [])
       ])
;;

let enclosed enclosed =
  match enclosed with
  | Char c -> Re.char c
  | Range (low, high) -> Re.rg low high
;;

let enclosed_set ~explicit_slash ~slashes ~explicit_period kind set =
  let set = List.map enclosed set in
  let enclosure =
    match kind with
    | `Any_of -> Re.alt set
    | `Any_but -> Re.compl set
  in
  Re.inter [ enclosure; one ~explicit_slash ~slashes ~explicit_period ]
;;

let exactly state c =
  let slashes = State.slashes state in
  let am_at_start_of_component = List.mem c slashes in
  let chars = if am_at_start_of_component then slashes else [ c ] in
  State.append state (Re.alt (List.map Re.char chars)) ~am_at_start_of_component
;;

let many_many state =
  let explicit_period = state.State.period && state.State.pathname in
  let first_explicit_period = State.explicit_period state in
  let slashes = State.slashes state in
  let match_component ~explicit_period =
    Re.seq
      [ one ~explicit_slash:true ~slashes ~explicit_period
      ; Re.rep (one ~explicit_slash:true ~slashes ~explicit_period:false)
      ]
  in
  (* We must match components individually when [period] flag is set,
     making sure to not match ["foo/.bar"]. *)
  State.append
    state
    (Re.seq
       [ Re.opt (match_component ~explicit_period:first_explicit_period)
       ; Re.rep
           (Re.seq
              [ Re.alt (List.map Re.char slashes)
              ; Re.opt (match_component ~explicit_period)
              ])
       ])
;;

let many (state : State.t) =
  let explicit_slash = State.explicit_slash state in
  let explicit_period = State.explicit_period state in
  let slashes = State.slashes state in
  (* Whether we must explicitly match period depends on the surrounding
     characters, but slashes are easy to explicit match. This conditional
     splits out some simple cases. *)
  if not explicit_period
  then State.append state (Re.rep (one ~explicit_slash ~slashes ~explicit_period))
  else if not explicit_slash
  then
    (* In this state, we explicitly match periods only at the very beginning *)
    State.append
      state
      (Re.opt
         (Re.seq
            [ one ~explicit_slash:false ~slashes ~explicit_period
            ; Re.rep (one ~explicit_slash:false ~slashes ~explicit_period:false)
            ]))
  else (
    let not_empty =
      Re.seq
        [ one ~explicit_slash:true ~slashes ~explicit_period:true
        ; Re.rep (one ~explicit_slash:true ~slashes ~explicit_period:false)
        ]
    in
    (* [maybe_empty] is the default translation of Many, except in some special
       cases. *)
    let maybe_empty = Re.opt not_empty in
    let enclosed_set state kind set =
      State.append
        state
        (Re.alt
           [ enclosed_set kind set ~explicit_slash:true ~slashes ~explicit_period:true
           ; Re.seq
               [ not_empty
               ; (* Since [not_empty] matched, subsequent dots are not leading. *)
                 enclosed_set
                   kind
                   set
                   ~explicit_slash:true
                   ~slashes
                   ~explicit_period:false
               ]
           ])
    in
    let rec lookahead state =
      match State.next state with
      | None -> State.append state maybe_empty
      (* glob ** === glob * . *)
      | Some (Many, state) -> lookahead state
      | Some (Exactly c, state) ->
        let state = State.append state (if c = '.' then not_empty else maybe_empty) in
        exactly state c
      (* glob *? === glob ?* *)
      | Some (One, state) -> State.append state not_empty
      | Some (Any_of enclosed, state) -> enclosed_set state `Any_of enclosed
      | Some (Any_but enclosed, state) -> enclosed_set state `Any_but enclosed
      (* * then ** === ** *)
      | Some (ManyMany, state) -> many_many state
    in
    lookahead state)
;;

let piece state piece =
  let explicit_slash = State.explicit_slash state in
  let explicit_period = State.explicit_period state in
  let slashes = State.slashes state in
  match piece with
  | One -> State.append state (one ~explicit_slash ~slashes ~explicit_period)
  | Many -> many state
  | Any_of enclosed ->
    State.append
      state
      (enclosed_set `Any_of ~explicit_slash ~slashes ~explicit_period enclosed)
  | Any_but enclosed ->
    State.append
      state
      (enclosed_set `Any_but ~explicit_slash ~slashes ~explicit_period enclosed)
  | Exactly c -> exactly state c
  | ManyMany -> many_many state
;;

let glob ~pathname ~match_backslashes ~period glob =
  let rec loop state =
    match State.next state with
    | None -> State.to_re state
    | Some (p, state) -> loop (piece state p)
  in
  loop (State.create ~pathname ~match_backslashes ~period glob)
;;

let glob
  ?(anchored = false)
  ?(pathname = true)
  ?(match_backslashes = false)
  ?(period = true)
  ?(expand_braces = false)
  ?(double_asterisk = true)
  s
  =
  let to_re s =
    let re = glob ~pathname ~match_backslashes ~period (of_string ~double_asterisk s) in
    if anchored then Re.whole_string re else re
  in
  if expand_braces then Re.alt (List.map to_re (explode s)) else to_re s
;;

let glob' ?anchored period s = glob ?anchored ~period s
let globx ?anchored s = glob ?anchored ~expand_braces:true s
let globx' ?anchored period s = glob ?anchored ~expand_braces:true ~period s
