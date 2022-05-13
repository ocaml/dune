include Stdune
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
include No_io

(* To make bug reports usable *)
let () = Printexc.record_backtrace true

let protect = Exn.protect

let protectx = Exn.protectx
