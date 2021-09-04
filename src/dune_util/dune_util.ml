module Config = Config
module Log = Log
module Ml_kind = Ml_kind
module Persistent = Persistent
module Report_error = Report_error
module Stringlike = Stringlike
module Stringlike_intf = Stringlike_intf
module Value = Value
open Stdune

let xdg =
  lazy
    (let env_map =
       let env = Env.to_map Env.initial in
       Env.Map.update env "HOME" ~f:(function
         | Some _ as s -> s
         | None -> (
           let uid = Unix.getuid () in
           match Unix.getpwuid uid with
           | exception Not_found -> None
           | s -> Some s.pw_dir))
     in
     Xdg.of_assoc (Env.Map.to_list env_map))
