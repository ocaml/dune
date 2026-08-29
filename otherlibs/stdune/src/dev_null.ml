let path = Path.of_filename_relative_to_initial_cwd Filename.null

let open_null flags =
  lazy (Fd.unsafe_of_unix_file_descr (Unix.openfile Filename.null flags 0o666))
;;

let in_ = open_null [ Unix.O_RDONLY; Unix.O_CLOEXEC ]
let out = open_null [ Unix.O_WRONLY; Unix.O_CLOEXEC ]
