(*---------------------------------------------------------------------------
   Copyright (c) 2025 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

val output :
  out_ppf:Format.formatter -> err_ppf:Format.formatter ->
  Cmdliner_def.Eval.t -> Cmdliner_def.Complete.t -> Cmdliner_def.Cline.t ->
  unit
