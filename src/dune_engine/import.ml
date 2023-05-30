include Stdune
module Digest = Dune_digest
module Console = Dune_console
module Metrics = Dune_metrics
module Log = Dune_util.Log
module Stringlike = Dune_util.Stringlike
module Stringlike_intf = Dune_util.Stringlike_intf
module Persistent = Dune_util.Persistent
module Execution_env = Dune_util.Execution_env
module Glob = Dune_glob.V1
include No_io
include Dune_config

(* To make bug reports usable *)
let () = Printexc.record_backtrace true

let protect = Exn.protect

let protectx = Exn.protectx
