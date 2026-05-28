(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Commands and their information. *)

type info = Cmdliner_def.Cmd_info.t

val info :
  ?deprecated:string -> ?man_xrefs:Cmdliner_manpage.xref list ->
  ?man:Cmdliner_manpage.block list -> ?envs:Cmdliner_def.Env.info list ->
  ?exits:Cmdliner_def.Exit.info list -> ?sdocs:string -> ?docs:string ->
  ?doc:string -> ?version:string -> string -> info

type 'a t =
| Cmd of info * 'a Cmdliner_term.parser
| Group of info * ('a Cmdliner_term.parser option * 'a t list)

val make : info -> 'a Cmdliner_term.t -> 'a t
val v : info -> 'a Cmdliner_term.t -> 'a t
val group : ?default:'a Cmdliner_term.t -> info -> 'a t list -> 'a t
val name : 'a t -> string
val name_trie : 'a t list -> 'a t Cmdliner_trie.t
val list_names : 'a t list -> string list
val get_info : 'a t -> info
val get_children_infos : 'a t -> info list
