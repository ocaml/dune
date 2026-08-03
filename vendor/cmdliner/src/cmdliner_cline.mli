(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Command lines. *)

val is_opt : string -> bool
val has_complete_prefix : string -> bool
val get_token_to_complete : string -> string

(** {1:cli Command lines} *)

val create :
  ?peek_opts:bool -> legacy_prefixes:bool -> for_completion:bool ->
  Cmdliner_def.Arg_info.Set.t -> string list ->
  [ `Ok of Cmdliner_def.Cline.t
  | `Complete of Cmdliner_def.Complete.t * Cmdliner_def.Cline.t
  | `Error of string * Cmdliner_def.Cline.t ]
