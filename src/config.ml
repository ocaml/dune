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

module type S = sig
  type 'a field

  type t =
    { display     : Display.t field
    ; concurrency : int       field
    }
end

module rec M : S with type 'a field = 'a = M
include M

module rec Partial : S with type 'a field := 'a option = Partial

let merge t (partial : Partial.t) =
  let field from_t from_partial =
    Option.value from_partial ~default:from_t
  in
  { display     = field t.display     partial.display
  ; concurrency = field t.concurrency partial.concurrency
  }

let default =
  { display     = Progress
  ; concurrency = 4
  }

let t =
  record
    (field "display" Display.t ~default:default.display
     >>= fun display ->
     field "jobs" int ~default:default.concurrency
     >>= fun concurrency ->
     return { display
            ; concurrency
            })

let user_config_file = Filename.concat Xdg.config_dir "dune/config"

let load_config_file ~fname =
  t (Sexp.load_many_as_one ~fname)

let load_user_config_file () =
  if Sys.file_exists user_config_file then
    load_config_file ~fname:user_config_file
  else
    default
