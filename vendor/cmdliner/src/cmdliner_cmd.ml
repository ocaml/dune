(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cmdliner programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(* Commands *)

(* Command info *)

type info = Cmdliner_info.Cmd.t
let info = Cmdliner_info.Cmd.v

type 'a t =
| Cmd of info * 'a Cmdliner_term.parser
| Group of info * ('a Cmdliner_term.parser option * 'a t list)

let get_info = function Cmd (i, _) | Group (i, _) -> i
let children_infos = function
| Cmd _ -> [] | Group (_, (_, cs)) -> List.map get_info cs

let v i (args, p) = Cmd (Cmdliner_info.Cmd.add_args i args, p)
let group ?default i cmds =
  let args, parser = match default with
  | None -> None, None | Some (args, p) -> Some args, Some p
  in
  let children = List.map get_info cmds in
  let i = Cmdliner_info.Cmd.with_children i ~args ~children in
  Group (i, (parser, cmds))

let name c = Cmdliner_info.Cmd.name (get_info c)

(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cmdliner programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
