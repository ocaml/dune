module Action = Action
module Alias_name = Alias_name
module Build_path_prefix_map = Build_path_prefix_map0
module Gc = Gc
module Global_lock = Global_lock
module Log = Log
module Persistent = Persistent
module Report_error = Report_error
module Stringlike = Stringlike

module type Stringlike = Stringlike_intf.S

open Stdune

let manual_xdg = ref None

let xdg =
  lazy
    (match !manual_xdg with
     | Some xdg -> xdg
     | None ->
       let env_map =
         Env.update Env.initial ~var:"HOME" ~f:(function
           | Some _ as s -> s
           | None ->
             let uid = Unix.getuid () in
             (match Unix.getpwuid uid with
              | exception Not_found -> None
              | s -> Some s.pw_dir))
       in
       Xdg.create ~env:(Env.get env_map) ())
;;

let override_xdg : Xdg.t -> unit =
  fun new_xdg ->
  if Lazy.is_val xdg
  then Code_error.raise "xdg overridden after being set" []
  else manual_xdg := Some new_xdg
;;

let ( / ) = Path.relative

(** The default directory of all caches (build and others), used when
    environment variables are unset.
    Set to [$XDG_CACHE_HOME/dune]. *)
let default_cache_dir =
  lazy
    (let cache_dir = Xdg.cache_dir (Lazy.force xdg) in
     Path.of_filename_relative_to_initial_cwd cache_dir / "dune")
;;

let cache_home_dir =
  lazy
    (let var = "DUNE_CACHE_HOME" in
     match Sys.getenv_opt var with
     | Some path ->
       if Filename.is_relative path
       then
         User_error.raise
           [ Pp.paragraphf "$%s should be an absolute path, but is %S" var path ];
       Path.external_ (Path.External.of_string path)
     | None -> Lazy.force default_cache_dir)
;;

let frames_per_second () =
  match Dune_config.Config.(get threaded_console_frames_per_second) with
  | `Custom fps -> fps
  | `Default when Stdune.Execution_env.inside_emacs -> 15
  | `Default -> 60
;;
