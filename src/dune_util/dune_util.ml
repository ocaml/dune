module Execution_env = Execution_env
module Log = Log
module Persistent = Persistent
module Report_error = Report_error
module Stringlike = Stringlike

module type Stringlike = Stringlike_intf.S

module Build_path_prefix_map = Build_path_prefix_map0
module Flock = Flock
module Global_lock = Global_lock
module Action = Action
module Alias_name = Alias_name
module Terminal_signals = Terminal_signals
module Gc = Gc
open Stdune

let xdg =
  lazy
    (let env_map =
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

let frames_per_second () =
  match Dune_config.Config.(get threaded_console_frames_per_second) with
  | `Custom fps -> fps
  | `Default when Execution_env.inside_emacs -> 15
  | `Default -> 60
;;
