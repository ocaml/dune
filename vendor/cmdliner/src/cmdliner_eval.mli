(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cmdliner programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Command evaluation *)

(** {1:eval Evaluating commands} *)

type 'a eval_ok = [ `Ok of 'a | `Version | `Help ]
type eval_error = [ `Parse | `Term | `Exn ]

val eval_value :
  ?help:Format.formatter -> ?err:Format.formatter -> ?catch:bool ->
  ?env:(string -> string option) -> ?argv:string array -> 'a Cmdliner_cmd.t ->
  ('a eval_ok, eval_error) result

val eval_peek_opts :
  ?version_opt:bool -> ?env:(string -> string option) ->
  ?argv:string array -> 'a Cmdliner_term.t ->
  'a option * ('a eval_ok, eval_error) result

val eval :
  ?help:Format.formatter -> ?err:Format.formatter -> ?catch:bool ->
  ?env:(string -> string option) -> ?argv:string array ->
  ?term_err:int -> unit Cmdliner_cmd.t -> Cmdliner_info.Exit.code

val eval' :
  ?help:Format.formatter -> ?err:Format.formatter -> ?catch:bool ->
  ?env:(string -> string option) -> ?argv:string array ->
  ?term_err:int -> int Cmdliner_cmd.t -> Cmdliner_info.Exit.code

val eval_result :
  ?help:Format.formatter -> ?err:Format.formatter -> ?catch:bool ->
  ?env:(string -> string option) -> ?argv:string array ->
  ?term_err:Cmdliner_info.Exit.code -> (unit, string) result Cmdliner_cmd.t ->
  Cmdliner_info.Exit.code

val eval_result' :
  ?help:Format.formatter -> ?err:Format.formatter -> ?catch:bool ->
  ?env:(string -> string option) -> ?argv:string array ->
  ?term_err:Cmdliner_info.Exit.code ->
  (Cmdliner_info.Exit.code, string) result Cmdliner_cmd.t ->
  Cmdliner_info.Exit.code

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
