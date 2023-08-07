(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018-2020 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module String : sig
  (* NOTE: OCaml >= 4.13 *)
  val exists: (char -> bool) -> string -> bool
end

module Either : sig
  (* NOTE: OCaml >= 4.12 *)
  type ('a, 'b) t =
    | Left of 'a
    | Right of 'b
end

module Lazy : sig
  (* NOTE: OCaml >= 4.13 *)
  val map : ('a -> 'b) -> 'a Lazy.t -> 'b Lazy.t
end

module Unix : sig
  (* `realpath` for OCaml >= 4.13.0,
     implementation with double chdir otherwise *)
  val realpath: string -> string
end

module Filename: sig
  (** NOTE: OCaml >= 4.10 *)

  val quote_command :
    string -> ?stdin:string -> ?stdout:string -> ?stderr:string
    -> string list -> string
end
