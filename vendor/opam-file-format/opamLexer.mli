(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2018 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** opam format lexer *)

open OpamParserTypes

exception Error of string
(** Raised on any lexing error with a description of the fault. Note that
    [Failure "lexing: empty token"] is never raised by the lexer. *)

val token: Lexing.lexbuf -> OpamBaseParser.token

(** [OpamLexer] transitional module with full position types *)
module FullPos : sig

  open OpamParserTypes.FullPos

  val relop: string -> relop_kind
  (** Inverse of {!OpamPrinter.relop} *)

  val logop: string -> logop_kind
  (** Inverse of {!OpamPrinter.logop} *)

  val pfxop: string -> pfxop_kind
  (** Inverse of {!OpamPrinter.pfxop} *)

  val env_update_op: string -> env_update_op_kind
  (** Inverse of {!OpamPrinter.env_update_op} *)

end

val relop: string -> relop
[@@ocaml.deprecated "Use OpamLexer.FullPos.relop instead."]

val logop: string -> logop
[@@ocaml.deprecated "Use OpamLexer.FullPos.logop instead."]

val pfxop: string -> pfxop
[@@ocaml.deprecated "Use OpamLexer.FullPos.pfxop instead."]

val env_update_op: string -> env_update_op
[@@ocaml.deprecated "Use OpamLexer.FullPos.env_update_op instead."]
