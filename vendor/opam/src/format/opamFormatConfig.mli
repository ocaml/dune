(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015-2016 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module E : sig
  type OpamStd.Config.E.t +=
    | ALLPARENS of bool option
    | SKIPVERSIONCHECKS of bool option
    | STRICT of bool option
end

(** Configuration options for the format lib (record, global reference and
    setter) *)

type t = private {
  strict : bool;
  (** Fail early with errors in OPAM files *)
  skip_version_checks : bool;
  (** Ignore mismatching OPAM versions in files *)
  all_parens : bool;
  (** Affects the OPAM format printer; for backwards-compatibility *)
}

type 'a options_fun =
  ?strict:bool ->
  ?skip_version_checks:bool ->
  ?all_parens:bool ->
  'a

include OpamStd.Config.Sig
  with type t := t
   and type 'a options_fun := 'a options_fun
