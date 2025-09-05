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

exception Parse_error = Parse_buffer.Parse_error
exception Not_supported

let acc_digits =
  let rec loop base digits acc i =
    match digits with
    | [] -> acc
    | d :: digits ->
      let acc = acc + (d * i) in
      let i = i * i in
      loop base digits acc i
  in
  fun ~base ~digits -> loop base digits 0 1
;;

let char_of_int x =
  match char_of_int x with
  | x -> x
  | exception _ -> raise Parse_error
;;

let parse multiline dollar_endonly dotall ungreedy s =
  let buf = Parse_buffer.create s in
  let accept = Parse_buffer.accept buf in
  let eos () = Parse_buffer.eos buf in
  let test c = Parse_buffer.test buf c in
  let unget () = Parse_buffer.unget buf in
  let get () = Parse_buffer.get buf in
  let greedy_mod r =
    let gr = accept '?' in
    let gr = if ungreedy then not gr else gr in
    if gr then Re.non_greedy r else Re.greedy r
  in
  let rec regexp () = regexp' (branch ())
  and regexp' left = if accept '|' then regexp' (Re.alt [ left; branch () ]) else left
  and branch () = branch' []
  and branch' left =
    if eos () || test '|' || test ')'
    then Re.seq (List.rev left)
    else branch' (piece () :: left)
  and in_brace ~f ~init =
    match accept '{' with
    | false -> None
    | true ->
      let rec loop acc =
        if accept '}'
        then acc
        else (
          let acc = f acc in
          loop acc)
      in
      Some (loop init)
  and piece () =
    let r = atom () in
    if accept '*'
    then greedy_mod (Re.rep r)
    else if accept '+'
    then greedy_mod (Re.rep1 r)
    else if accept '?'
    then greedy_mod (Re.opt r)
    else if accept '{'
    then (
      match Parse_buffer.integer buf with
      | Some i ->
        let j = if accept ',' then Parse_buffer.integer buf else Some i in
        if not (accept '}') then raise Parse_error;
        (match j with
         | Some j when j < i -> raise Parse_error
         | _ -> ());
        greedy_mod (Re.repn r i j)
      | None ->
        unget ();
        r)
    else r
  and atom () =
    if accept '.'
    then if dotall then Re.any else Re.notnl
    else if accept '('
    then
      if accept '?'
      then
        if accept ':'
        then (
          let r = regexp () in
          if not (accept ')') then raise Parse_error;
          r)
        else if accept '#'
        then comment ()
        else if accept '<'
        then (
          let name = name () in
          let r = regexp () in
          if not (accept ')') then raise Parse_error;
          Re.group ~name r)
        else raise Parse_error
      else (
        let r = regexp () in
        if not (accept ')') then raise Parse_error;
        Re.group r)
    else if accept '^'
    then if multiline then Re.bol else Re.bos
    else if accept '$'
    then if multiline then Re.eol else if dollar_endonly then Re.leol else Re.eos
    else if accept '['
    then if accept '^' then Re.compl (bracket []) else Re.alt (bracket [])
    else if accept '\\'
    then (
      (* XXX
         - Back-references
         - \cx (control-x), \ddd
      *)
      if eos () then raise Parse_error;
      match get () with
      | 'w' -> Re.alt [ Re.alnum; Re.char '_' ]
      | 'W' -> Re.compl [ Re.alnum; Re.char '_' ]
      | 's' -> Re.space
      | 'S' -> Re.compl [ Re.space ]
      | 'd' -> Re.digit
      | 'D' -> Re.compl [ Re.digit ]
      | 'b' -> Re.alt [ Re.bow; Re.eow ]
      | 'B' -> Re.not_boundary
      | 'A' -> Re.bos
      | 'Z' -> Re.leol
      | 'z' -> Re.eos
      | 'G' -> Re.start
      | 'e' -> Re.char '\x1b'
      | 'f' -> Re.char '\x0c'
      | 'n' -> Re.char '\n'
      | 'r' -> Re.char '\r'
      | 't' -> Re.char '\t'
      | 'Q' -> quote (Buffer.create 12)
      | 'E' -> raise Parse_error
      | 'x' ->
        let c1, c2 =
          match in_brace ~init:[] ~f:(fun acc -> hexdigit () :: acc) with
          | Some [ c1; c2 ] -> c1, c2
          | Some [ c2 ] -> 0, c2
          | Some _ -> raise Parse_error
          | None ->
            let c1 = hexdigit () in
            let c2 = hexdigit () in
            c1, c2
        in
        let code = (c1 * 16) + c2 in
        Re.char (char_of_int code)
      | 'o' ->
        (match
           in_brace ~init:[] ~f:(fun acc ->
             match maybe_octaldigit () with
             | None -> raise Parse_error
             | Some p -> p :: acc)
         with
         | None -> raise Parse_error
         | Some digits -> Re.char (char_of_int (acc_digits ~base:8 ~digits)))
      | 'a' .. 'z' | 'A' .. 'Z' -> raise Parse_error
      | '0' .. '7' as n1 ->
        let n2 = maybe_octaldigit () in
        let n3 = maybe_octaldigit () in
        (match n2, n3 with
         | Some n2, Some n3 ->
           let n1 = Char.code n1 - Char.code '0' in
           Re.char (char_of_int ((n1 * (8 * 8)) + (n2 * 8) + n3))
         | _, _ -> raise Not_supported)
      | '8' .. '9' -> raise Not_supported
      | c -> Re.char c)
    else (
      if eos () then raise Parse_error;
      match get () with
      | '*' | '+' | '?' | '{' | '\\' -> raise Parse_error
      | c -> Re.char c)
  and quote buf =
    if accept '\\'
    then (
      if eos () then raise Parse_error;
      match get () with
      | 'E' -> Re.str (Buffer.contents buf)
      | c ->
        Buffer.add_char buf '\\';
        Buffer.add_char buf c;
        quote buf)
    else (
      if eos () then raise Parse_error;
      Buffer.add_char buf (get ());
      quote buf)
  and hexdigit () =
    if eos () then raise Parse_error;
    match get () with
    | '0' .. '9' as d -> Char.code d - Char.code '0'
    | 'a' .. 'f' as d -> Char.code d - Char.code 'a' + 10
    | 'A' .. 'F' as d -> Char.code d - Char.code 'A' + 10
    | _ -> raise Parse_error
  and maybe_octaldigit () =
    if eos ()
    then None
    else (
      match get () with
      | '0' .. '7' as d -> Some (Char.code d - Char.code '0')
      | _ -> None)
  and name () =
    if eos ()
    then raise Parse_error
    else (
      match get () with
      | ('_' | 'a' .. 'z' | 'A' .. 'Z') as c ->
        let b = Buffer.create 32 in
        Buffer.add_char b c;
        name' b
      | _ -> raise Parse_error)
  and name' b =
    if eos ()
    then raise Parse_error
    else (
      match get () with
      | ('_' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as c ->
        Buffer.add_char b c;
        name' b
      | '>' -> Buffer.contents b
      | _ -> raise Parse_error)
  and bracket s =
    if s <> [] && accept ']'
    then s
    else (
      match char () with
      | `Set st -> bracket (st :: s)
      | `Char c ->
        if accept '-'
        then
          if accept ']'
          then Re.char c :: Re.char '-' :: s
          else
            bracket
              (match char () with
               | `Char c' -> Re.rg c c' :: s
               | `Set st' -> Re.char c :: Re.char '-' :: st' :: s)
        else bracket (Re.char c :: s))
  and char () =
    if eos () then raise Parse_error;
    let c = get () in
    if c = '['
    then (
      if accept '=' then raise Not_supported;
      match Posix_class.parse buf with
      | Some set -> `Set set
      | None ->
        if accept '.'
        then (
          if eos () then raise Parse_error;
          let c = get () in
          if not (accept '.') then raise Not_supported;
          if not (accept ']') then raise Parse_error;
          `Char c)
        else `Char c)
    else if c = '\\'
    then (
      if eos () then raise Parse_error;
      let c = get () in
      (* XXX
         \127, ...
      *)
      match c with
      | 'b' -> `Char '\008'
      | 'n' -> `Char '\n' (*XXX*)
      | 'r' -> `Char '\r' (*XXX*)
      | 't' -> `Char '\t' (*XXX*)
      | 'w' -> `Set (Re.alt [ Re.alnum; Re.char '_' ])
      | 'W' -> `Set (Re.compl [ Re.alnum; Re.char '_' ])
      | 's' -> `Set Re.space
      | 'S' -> `Set (Re.compl [ Re.space ])
      | 'd' -> `Set Re.digit
      | 'D' -> `Set (Re.compl [ Re.digit ])
      | 'a' .. 'z' | 'A' .. 'Z' -> raise Parse_error
      | '0' .. '9' -> raise Not_supported
      | _ -> `Char c)
    else `Char c
  and comment () =
    if eos () then raise Parse_error;
    if accept ')'
    then Re.epsilon
    else (
      Parse_buffer.junk buf;
      comment ())
  in
  let res = regexp () in
  if not (eos ()) then raise Parse_error;
  res
;;

type opt =
  [ `Ungreedy
  | `Dotall
  | `Dollar_endonly
  | `Multiline
  | `Anchored
  | `Caseless
  ]

let re ?(opts = []) s =
  let r =
    parse
      (List.memq `Multiline opts)
      (List.memq `Dollar_endonly opts)
      (List.memq `Dotall opts)
      (List.memq `Ungreedy opts)
      s
  in
  let r = if List.memq `Anchored opts then Re.seq [ Re.start; r ] else r in
  let r = if List.memq `Caseless opts then Re.no_case r else r in
  r
;;

let compile = Re.compile
let compile_pat ?(opts = []) s = compile (re ~opts s)
