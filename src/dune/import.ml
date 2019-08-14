open! Stdune
include Stdune
module Re = Dune_re

(* To make bug reports usable *)
let () = Printexc.record_backtrace true

let initial_cwd = Sys.getcwd ()

module Sys = struct
  include Sys

  let force_remove =
    if win32 then (
      fun fn ->
    try remove fn
    with Sys_error _ ->
      (* Try to remove the "read-only" attribute, then retry. *)
      (try Unix.chmod fn 0o666 with Unix.Unix_error _ -> ());
      remove fn
    ) else
      remove
end

let protect = Exn.protect

let protectx = Exn.protectx

type fail = { fail : 'a. unit -> 'a }

(* Disable file operations to force to use the IO module *)
let open_in = `Use_Io

let open_in_bin = `Use_Io

let open_in_gen = `Use_Io

let open_out = `Use_Io

let open_out_bin = `Use_Io

let open_out_gen = `Use_Io

(* We open this module at the top of module generating rules, to make sure they
  don't do Io manually *)
module No_io = struct
  module Io = struct end
end
