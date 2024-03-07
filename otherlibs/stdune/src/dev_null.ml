let dev_null_fn = if Sys.win32 then "nul" else "/dev/null"
let path = Path.of_filename_relative_to_initial_cwd dev_null_fn
let open_null flags = lazy (Unix.openfile dev_null_fn flags 0o666)
let in_ = open_null [ Unix.O_RDONLY; Unix.O_CLOEXEC ]
let out = open_null [ Unix.O_WRONLY; Unix.O_CLOEXEC ]
