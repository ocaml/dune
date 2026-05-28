(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Commands *)

type info = Cmdliner_def.Cmd_info.t
let info = Cmdliner_def.Cmd_info.make

type 'a t =
| Cmd of info * 'a Cmdliner_term.parser
| Group of info * ('a Cmdliner_term.parser option * 'a t list)

let make info t =
  let info = Cmdliner_def.Cmd_info.add_args info (Cmdliner_term.argset t) in
  Cmd (info, Cmdliner_term.parser t)

let v = make

let get_info = function Cmd (info, _) | Group (info, _) -> info
let get_children_infos = function
| Cmd _ -> [] | Group (_, (_, cs)) -> List.map get_info cs

let group ?default info cmds =
  let args, parser = match default with
  | None -> None, None
  | Some t -> Some (Cmdliner_term.argset t), Some (Cmdliner_term.parser t)
  in
  let children = List.map get_info cmds in
  let info = Cmdliner_def.Cmd_info.with_children info ~args ~children in
  Group (info, (parser, cmds))

let name c = Cmdliner_def.Cmd_info.name (get_info c)

let name_trie cmds =
  let add acc cmd =
    let info = get_info cmd in
    let name = Cmdliner_def.Cmd_info.name info in
    match Cmdliner_trie.add acc name cmd with
    | `New t -> t
    | `Replaced (cmd', _) ->
        let info' = get_info cmd' and kind = "command" in
        invalid_arg @@
        Cmdliner_base.err_multi_def ~kind name
          Cmdliner_def.Cmd_info.doc info info'
  in
  List.fold_left add Cmdliner_trie.empty cmds

let list_names cmds =
  let cmd_name c = Cmdliner_def.Cmd_info.name (get_info c) in
  List.sort String.compare (List.rev_map cmd_name cmds)
