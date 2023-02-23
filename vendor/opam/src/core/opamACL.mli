(**************************************************************************)
(*                                                                        *)
(*    Copyright 2020 David Allsopp Ltd.                                   *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** C auxiliary function used for POSIX 1003.1e DRAFT 17 permission checking. *)

val get_acl_executable_info : string -> int -> int list option
  (** If compiled without libacl support, this function always returns None
      When opam is built with libacl support,
      [get_acl_executable_info file owner] takes a filename and the uid of the
      owner of that file (this is passed since the caller will have already
      called {!Unix.stat}). The function returns [Some []] if the process can
      execute [file] or [Some gids] if the process can execute [file] if one
      of its groups matches [gids]. If the process cannot under any
      circumstances execute [file] (or if an unexpected error occurred), then
      [None] is returned. *)
