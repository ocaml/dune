include Stdune
module Log = Dune_util.Log
module Re = Dune_re
module Stringlike = Dune_util.Stringlike
module Stringlike_intf = Dune_util.Stringlike_intf
module Persistent = Dune_util.Persistent
module Value = Dune_util.Value
module Ml_kind = Dune_util.Ml_kind
module Dune_rpc = Dune_rpc_private
module Config = Dune_util.Config

module Path = struct
  include Path

  module Untracked = struct
    let exists = exists

    let is_directory = is_directory

    let is_directory_with_error = is_directory_with_error

    let readdir_unsorted = readdir_unsorted

    let readdir_unsorted_with_kinds = readdir_unsorted_with_kinds

    let stat = stat

    let stat_exn = stat_exn

    let lstat = lstat

    let lstat_exn = lstat_exn
  end

  (* Encourage using [Fs_memo] equivalents if possible. The untracked versions
     are still available in the [Path.Untracked] module. *)

  let exists = `Use_fs_memo_or_untracked_module_instead

  let is_directory = `Use_fs_memo_or_untracked_module_instead

  let is_directory_with_error = `Use_fs_memo_or_untracked_module_instead

  let stat = `Use_fs_memo_or_untracked_module_instead

  let stat_exn = `Use_fs_memo_or_untracked_module_instead

  let lstat = `Use_fs_memo_or_untracked_module_instead

  let lstat_exn = `Use_fs_memo_or_untracked_module_instead

  let readdir_unsorted = `Use_fs_memo_or_untracked_module_instead

  let readdir_unsorted_with_kinds = `Use_fs_memo_or_untracked_module_instead
end

module Io = struct
  include Io

  module Untracked = struct
    let with_lexbuf_from_file = with_lexbuf_from_file
  end

  (* Encourage using [Fs_memo] equivalents if possible. The untracked versions
     are still available in the [Io.Untracked] module. *)

  let with_lexbuf_from_file = `Use_fs_memo_or_untracked_module_instead
end

(* To make bug reports usable *)
let () = Printexc.record_backtrace true

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
