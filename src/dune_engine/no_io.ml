(* we don't [Import] here because this module is part of [Import] *)
open Stdune

module Path = struct
  include Path

  module Untracked = struct
    let exists = exists
    let is_directory = is_directory
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
  let is_file = `Use_fs_memo_or_untracked_module_instead
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
