open! Import

let local_install_dir =
  let dir = Path.(relative root) "_build/install" in
  fun ~context -> Path.relative dir context

let local_install_bin_dir ~context =
  Path.relative (local_install_dir ~context) "bin"

let local_install_man_dir ~context =
  Path.relative (local_install_dir ~context) "bin"

let local_install_lib_dir ~context ~package =
  Path.relative
    (Path.relative (local_install_dir ~context) "lib")
    package

let dev_null = Path.of_string (if Sys.win32 then "nul" else "/dev/null")

let jbuilder_keep_fname = ".jbuilder-keep"

open Sexp.Of_sexp

module Display = struct
  type t =
    | Progress
    | Short
    | Verbose
    | Quiet

  let all =
      [ "progress" , Progress
      ; "verbose"  , Verbose
      ; "short"    , Short
      ; "quiet"    , Quiet
      ]

  let t = enum all
end

type t =
  { display : Display.t
  }

let default =
  { display = Progress
  }

let t =
  record
    (field "display" Display.t >>= fun display ->
     return { display })

let user_config_file = Filename.concat Xdg.config_dir "dune/config"

let load_config_file ~fname =
  t (Sexp.load_many_as_one ~fname)

let load_user_config_file () =
  if Sys.file_exists user_config_file then
    load_config_file ~fname:user_config_file
  else
    default
