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

let contents =
  let read_file =
    Memo.exec
      (Memo.create_with_store
         "Action_builder.contents"
         ~store:(module Path.Table)
         ~input:(module Path)
         ~cutoff:String.equal
         (fun p -> Build_system.read_file p ~f:Io.read_file))
  in
  fun p ->
    of_thunk
      { f =
          (fun mode ->
             let open Memo.O in
             let+ x = read_file p in
             x, Deps_or_facts.empty mode)
      }
;;

let if_file_exists p ~then_ ~else_ =
  of_thunk
    { f =
        (fun mode ->
           let open Memo.O in
           Build_system.file_exists p
           >>= function
           | true -> run then_ mode
           | false -> run else_ mode)
    }
;;
