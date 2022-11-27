include Stdune
module Digest = Dune_digest
module Console = Dune_console
module Metrics = Dune_metrics
module Log = Dune_util.Log
module Re = Dune_re
module Stringlike = Dune_util.Stringlike
module Stringlike_intf = Dune_util.Stringlike_intf
module Persistent = Dune_util.Persistent
module Value = Dune_util.Value
module Ml_kind = Ocaml.Ml_kind
module Cm_kind = Ocaml.Cm_kind
module Mode = Ocaml.Mode
module Config = Dune_util.Config
module Predicate_lang = Dune_lang.Predicate_lang
module String_with_vars = Dune_lang.String_with_vars
module Pform = Dune_lang.Pform
module Glob = Dune_lang.Glob
module Outputs = Dune_lang.Action.Outputs
module Inputs = Dune_lang.Action.Inputs
module File_perm = Dune_lang.Action.File_perm
module Diff = Dune_lang.Action.Diff
include No_io

(* To make bug reports usable *)
let () = Printexc.record_backtrace true

let protect = Exn.protect

let protectx = Exn.protectx
