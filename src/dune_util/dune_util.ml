module Config = Config
module Log = Log
module Persistent = Persistent
module Report_error = Report_error
module Stringlike = Stringlike
module Stringlike_intf = Stringlike_intf
module Value = Value
module Build_path_prefix_map = Build_path_prefix_map0
module Flock = Flock
module Global_lock = Global_lock
open Stdune

let xdg =
  lazy
    (let env_map =
       Env.update Env.initial ~var:"HOME" ~f:(function
         | Some _ as s -> s
         | None -> (
           let uid = Unix.getuid () in
           match Unix.getpwuid uid with
           | exception Not_found -> None
           | s -> Some s.pw_dir))
     in
     Xdg.create ~env:(Env.get env_map) ())
