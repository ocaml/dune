open Import
include Action_builder0

open struct
  module List = Stdune.List
end

let record_action_deps : type m. m eval_mode -> Dep.Set.t -> m Memo.t =
  fun mode deps ->
  match mode with
  | Eager -> Build_system.build_deps deps
  | Lazy -> Memo.return deps
;;

let record (deps : Dep.Set.t) =
  of_thunk
    { f =
        (fun mode ->
           let open Memo.O in
           let+ deps = record_action_deps mode deps in
           (), deps)
    }
;;
