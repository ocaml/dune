open Import

type t =
  { workspace : Package_variable_name.Set.t
  ; dependencies : Package_variable_name.Set.t
  }

let empty =
  { workspace = Package_variable_name.Set.empty
  ; dependencies = Package_variable_name.Set.empty
  }
;;

module Updater = struct
  type stats = t
  type t = stats ref

  let init () = ref empty
  let snapshot t = !t

  let expand_variable t ~scope variable =
    let stats = !t in
    t
    := match scope with
       | Solver_env.Scope.Workspace ->
         { stats with workspace = Package_variable_name.Set.add stats.workspace variable }
       | Dependencies ->
         { stats with
           dependencies = Package_variable_name.Set.add stats.dependencies variable
         }
  ;;

  let wrap_env t ~scope env variable =
    (match OpamVariable.Full.scope variable with
     | Self | Package _ -> ()
     | Global ->
       let variable_name =
         OpamVariable.Full.variable variable |> Package_variable_name.of_opam
       in
       expand_variable t ~scope variable_name);
    env variable
  ;;
end
