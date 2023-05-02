(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015-2016 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module E = struct

  type OpamStd.Config.E.t +=
    | ALLPARENS of bool option
    | SKIPVERSIONCHECKS of bool option
    | STRICT of bool option

  open OpamStd.Config.E
  let allparens = value (function ALLPARENS b -> b | _ -> None)
  let skipversionchecks = value (function SKIPVERSIONCHECKS b -> b | _ -> None)
  let strict = value (function STRICT b -> b | _ -> None)

end

type t = {
  strict: bool;
  skip_version_checks: bool;
  all_parens: bool;
}

type 'a options_fun =
  ?strict:bool ->
  ?skip_version_checks:bool ->
  ?all_parens:bool ->
  'a

let default = {
  strict = false;
  skip_version_checks = false;
  all_parens = false;
}

let setk k t
    ?strict
    ?skip_version_checks
    ?all_parens
  =
  let (+) x opt = match opt with Some x -> x | None -> x in
  k {
    strict = t.strict + strict;
    skip_version_checks = t.skip_version_checks + skip_version_checks;
    all_parens = t.all_parens + all_parens;
  }

let set t = setk (fun x () -> x) t

(* Global configuration reference *)

let r = ref default

let update ?noop:_ = setk (fun cfg () -> r := cfg) !r

let initk k =
  setk (setk (fun c -> r := c; k)) !r
    ?strict:(E.strict ())
    ?skip_version_checks:(E.skipversionchecks ())
    ?all_parens:(E.allparens ())

let init ?noop:_ = initk (fun () -> ())
