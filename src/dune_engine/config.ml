open! Stdune
open! Import

let local_install_dir ~context =
  let context = Context_name.to_string context in
  Path.Build.relative Dpath.Build.install_dir context

let local_install_lib_root ~context =
  Path.Build.relative (local_install_dir ~context) "lib"

let local_install_bin_dir ~context =
  Path.Build.relative (local_install_dir ~context) "bin"

let local_install_man_dir ~context =
  Path.Build.relative (local_install_dir ~context) "bin"

let local_install_lib_dir ~context ~package =
  Path.Build.relative
    (local_install_lib_root ~context)
    (Package.Name.to_string package)

let dev_null_fn =
  if Sys.win32 then
    "nul"
  else
    "/dev/null"

let dev_null = Path.of_filename_relative_to_initial_cwd dev_null_fn

let open_null flags = lazy (Unix.openfile dev_null_fn flags 0o666)

let dev_null_in = open_null [ Unix.O_RDONLY ]

let dev_null_out = open_null [ Unix.O_WRONLY ]

let dune_keep_fname = ".dune-keep"

let inside_emacs = Option.is_some (Env.get Env.initial "INSIDE_EMACS")

let inside_dune = Option.is_some (Env.get Env.initial "INSIDE_DUNE")

let inside_ci = Option.is_some (Env.get Env.initial "CI")

let show_full_command_on_error () =
  !Clflags.always_show_command_line
  || (* We want to show command lines in the CI, but not when running inside
        dune. Otherwise tests would yield different result whether they are
        executed locally or in the CI. *)
  (inside_ci && not inside_dune)
