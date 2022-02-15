open Stdune

let dev_null_fn = if Sys.win32 then "nul" else "/dev/null"

let dev_null = Path.of_filename_relative_to_initial_cwd dev_null_fn

let open_null flags = lazy (Unix.openfile dev_null_fn flags 0o666)

let dev_null_in = open_null [ Unix.O_RDONLY; Unix.O_CLOEXEC ]

let dev_null_out = open_null [ Unix.O_WRONLY; Unix.O_CLOEXEC ]

let inside_emacs = Option.is_some (Env.get Env.initial "INSIDE_EMACS")

let inside_dune = Option.is_some (Env.get Env.initial "INSIDE_DUNE")

let inside_ci = Option.is_some (Env.get Env.initial "CI")
