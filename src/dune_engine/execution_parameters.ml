open Import

module Action_output_on_success = struct
  type t =
    | Print
    | Swallow
    | Must_be_empty

  let all = [ "print", Print; "swallow", Swallow; "must-be-empty", Must_be_empty ]

  let to_dyn = function
    | Print -> Dyn.Variant ("Print", [])
    | Swallow -> Variant ("Swallow", [])
    | Must_be_empty -> Variant ("Must_be_empty", [])
  ;;

  let equal = Poly.equal
  let hash = Poly.hash
end

module Action_output_limit = struct
  type t = int

  let default = 100_000
  let to_string = Int.to_string
  let to_dyn = Dyn.int
  let equal = Int.equal
end

module Workspace_root_for_build_prefix_map = struct
  type t =
    | Unset
    | Set of string

  let equal x y =
    match x, y with
    | Unset, Unset -> true
    | Unset, _ | _, Unset -> false
    | Set x, Set y -> String.equal x y
  ;;

  let to_dyn =
    let open Dyn in
    function
    | Unset -> variant "Unset" []
    | Set s -> variant "Set" [ string s ]
  ;;
end

type t =
  { action_stdout_on_success : Action_output_on_success.t
  ; action_stderr_on_success : Action_output_on_success.t
  ; action_stdout_limit : Action_output_limit.t
  ; action_stderr_limit : Action_output_limit.t
  ; expand_aliases_in_sandbox : bool
  ; workspace_root_to_build_path_prefix_map : Workspace_root_for_build_prefix_map.t
  ; should_remove_write_permissions_on_generated_files : bool
  }

let equal
  { action_stdout_on_success
  ; action_stderr_on_success
  ; action_stdout_limit
  ; action_stderr_limit
  ; expand_aliases_in_sandbox
  ; workspace_root_to_build_path_prefix_map
  ; should_remove_write_permissions_on_generated_files
  }
  t
  =
  Action_output_on_success.equal action_stdout_on_success t.action_stdout_on_success
  && Action_output_on_success.equal action_stderr_on_success t.action_stderr_on_success
  && Action_output_limit.equal action_stdout_limit t.action_stdout_limit
  && Action_output_limit.equal action_stderr_limit t.action_stderr_limit
  && Bool.equal expand_aliases_in_sandbox t.expand_aliases_in_sandbox
  && Workspace_root_for_build_prefix_map.equal
       workspace_root_to_build_path_prefix_map
       t.workspace_root_to_build_path_prefix_map
  && Bool.equal
       should_remove_write_permissions_on_generated_files
       t.should_remove_write_permissions_on_generated_files
;;

let hash
  { action_stdout_on_success
  ; action_stderr_on_success
  ; action_stdout_limit
  ; action_stderr_limit
  ; expand_aliases_in_sandbox
  ; workspace_root_to_build_path_prefix_map
  ; should_remove_write_permissions_on_generated_files
  }
  =
  Poly.hash
    ( Action_output_on_success.hash action_stdout_on_success
    , Action_output_on_success.hash action_stderr_on_success
    , action_stdout_limit
    , action_stderr_limit
    , expand_aliases_in_sandbox
    , workspace_root_to_build_path_prefix_map
    , should_remove_write_permissions_on_generated_files )
;;

let to_dyn
  { action_stdout_on_success
  ; action_stderr_on_success
  ; action_stdout_limit
  ; action_stderr_limit
  ; expand_aliases_in_sandbox
  ; workspace_root_to_build_path_prefix_map
  ; should_remove_write_permissions_on_generated_files
  }
  =
  Dyn.Record
    [ "action_stdout_on_success", Action_output_on_success.to_dyn action_stdout_on_success
    ; "action_stderr_on_success", Action_output_on_success.to_dyn action_stderr_on_success
    ; "action_stdout_limit", Action_output_limit.to_dyn action_stdout_limit
    ; "action_stderr_limit", Action_output_limit.to_dyn action_stderr_limit
    ; "expand_aliases_in_sandbox", Bool expand_aliases_in_sandbox
    ; ( "workspace_root_to_build_path_prefix_map"
      , Workspace_root_for_build_prefix_map.to_dyn workspace_root_to_build_path_prefix_map
      )
    ; ( "should_remove_write_permissions_on_generated_files"
      , Bool should_remove_write_permissions_on_generated_files )
    ]
;;

let builtin_default =
  { action_stdout_on_success = Print
  ; action_stderr_on_success = Print
  ; action_stdout_limit = Action_output_limit.default
  ; action_stderr_limit = Action_output_limit.default
  ; expand_aliases_in_sandbox = true
  ; workspace_root_to_build_path_prefix_map = Set "/workspace_root"
  ; should_remove_write_permissions_on_generated_files = true
  }
;;

let set_action_stdout_on_success x t = { t with action_stdout_on_success = x }
let set_action_stderr_on_success x t = { t with action_stderr_on_success = x }
let set_action_stdout_limit x t = { t with action_stdout_limit = x }
let set_action_stderr_limit x t = { t with action_stderr_limit = x }
let set_expand_aliases_in_sandbox x t = { t with expand_aliases_in_sandbox = x }

let set_workspace_root_to_build_path_prefix_map x t =
  { t with workspace_root_to_build_path_prefix_map = x }
;;

let set_should_remove_write_permissions_on_generated_files x t =
  { t with should_remove_write_permissions_on_generated_files = x }
;;

let expand_aliases_in_sandbox t = t.expand_aliases_in_sandbox
let workspace_root_to_build_path_prefix_map t = t.workspace_root_to_build_path_prefix_map
let action_stdout_on_success t = t.action_stdout_on_success
let action_stderr_on_success t = t.action_stderr_on_success
let action_stdout_limit t = t.action_stdout_limit
let action_stderr_limit t = t.action_stderr_limit

let should_remove_write_permissions_on_generated_files t =
  t.should_remove_write_permissions_on_generated_files
;;

let default = Fdecl.create Dyn.opaque
let init t = Fdecl.set default t

let default =
  let open Memo.O in
  let* () = Memo.return () in
  Fdecl.get default
;;
