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

(*
   What we could (should?) do:
   - a* ==> longest ((shortest (no_group a)* ), a | ())  (!!!)
   - abc understood as (ab)c
   - "((a?)|b)" against "ab" should not bind the first subpattern to anything

   Note that it should be possible to handle "(((ab)c)d)e" efficiently
*)
module Re = Core

exception Parse_error = Parse_buffer.Parse_error
exception Not_supported

let parse newline s =
  let buf = Parse_buffer.create s in
  let accept = Parse_buffer.accept buf in
  let eos () = Parse_buffer.eos buf in
  let test c = Parse_buffer.test buf c in
  let unget () = Parse_buffer.unget buf in
  let get () = Parse_buffer.get buf in
  let rec regexp () = regexp' (branch ())
  and regexp' left = if accept '|' then regexp' (Re.alt [ left; branch () ]) else left
  and branch () = branch' []
  and branch' left =
    if eos () || test '|' || test ')'
    then Re.seq (List.rev left)
    else branch' (piece () :: left)
  and piece () =
    let r = atom () in
    if accept '*'
    then Re.rep (Re.nest r)
    else if accept '+'
    then Re.rep1 (Re.nest r)
    else if accept '?'
    then Re.opt r
    else if accept '{'
    then (
      match Parse_buffer.integer buf with
      | Some i ->
        let j = if accept ',' then Parse_buffer.integer buf else Some i in
        if not (accept '}') then raise Parse_error;
        (match j with
         | Some j when j < i -> raise Parse_error
         | _ -> ());
        Re.repn (Re.nest r) i j
      | None ->
        unget ();
        r)
    else r
  and atom () =
    if accept '.'
    then if newline then Re.notnl else Re.any
    else if accept '('
    then (
      let r = regexp () in
      if not (accept ')') then raise Parse_error;
      Re.group r)
    else if accept '^'
    then if newline then Re.bol else Re.bos
    else if accept '$'
    then if newline then Re.eol else Re.eos
    else if accept '['
    then
      if accept '^'
      then Re.diff (Re.compl (bracket [])) (Re.char '\n')
      else Re.alt (bracket [])
    else if accept '\\'
    then (
      if eos () then raise Parse_error;
      match get () with
      | ('|' | '(' | ')' | '*' | '+' | '?' | '[' | '.' | '^' | '$' | '{' | '\\') as c ->
        Re.char c
      | _ -> raise Parse_error)
    else (
      if eos () then raise Parse_error;
      match get () with
      | '*' | '+' | '?' | '{' | '\\' -> raise Parse_error
      | c -> Re.char c)
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
    else `Char c
  in
  let res = regexp () in
  if not (eos ()) then raise Parse_error;
  res
;;

type opt =
  [ `ICase
  | `NoSub
  | `Newline
  ]

let re ?(opts = []) s =
  let r = parse (List.memq `Newline opts) s in
  let r = if List.memq `ICase opts then Re.no_case r else r in
  let r = if List.memq `NoSub opts then Re.no_group r else r in
  r
;;

let compile re = Re.compile (Re.longest re)
let compile_pat ?(opts = []) s = compile (re ~opts s)
