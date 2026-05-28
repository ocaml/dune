(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

val pp_man :
  env:(string -> string option) ->
  errs:Format.formatter -> Cmdliner_manpage.format -> Format.formatter ->
  Cmdliner_def.Eval.t -> unit

val styled_usage_synopsis :
  errs:Format.formatter -> Cmdliner_def.Eval.t -> string
