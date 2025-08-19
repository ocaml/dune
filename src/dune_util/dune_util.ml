module Log = Log
module Persistent = Persistent
module Report_error = Report_error
module Stringlike = Stringlike

module type Stringlike = Stringlike_intf.S

module Build_path_prefix_map = Build_path_prefix_map0
module Global_lock = Global_lock
module Action = Action
module Alias_name = Alias_name
module Gc = Gc
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

let frames_per_second () =
  match Dune_config.Config.(get threaded_console_frames_per_second) with
  | `Custom fps -> fps
  | `Default when Stdune.Execution_env.inside_emacs -> 15
  | `Default -> 60
;;
