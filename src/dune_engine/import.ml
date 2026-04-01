include Stdune
module Digest = Dune_digest.Digest
module Compound_user_error = Dune_rpc.Private.Compound_user_error
module Stringlike = Dune_util.Stringlike
module Files_to_promote = Dune_rpc.Private.Files_to_promote
include Dune_scheduler
module Dune_rpc = Dune_rpc.Private

module type Stringlike = Dune_util.Stringlike

module Persistent = Dune_util.Persistent
module Glob = Dune_glob.V1
module Targets = Dune_targets
include No_io

(* To make bug reports usable *)
let () = Printexc.record_backtrace true
let protect = Exn.protect
let protectx = Exn.protectx
