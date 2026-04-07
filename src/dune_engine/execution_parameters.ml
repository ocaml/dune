open Import

module Action_output_on_success = struct
  type t =
    | Print
    | Swallow
    | Must_be_empty

  let for_digest = function
    | Print -> 0
    | Swallow -> 1
    | Must_be_empty -> 2
  ;;

  let all = [ "print", Print; "swallow", Swallow; "must-be-empty", Must_be_empty ]
  let equal = Poly.equal
  let hash = Poly.hash

  let repr =
    Repr.variant
      "action-output-on-success"
      [ Repr.case0 "Print" ~test:(equal Print)
      ; Repr.case0 "Swallow" ~test:(equal Swallow)
      ; Repr.case0 "Must_be_empty" ~test:(equal Must_be_empty)
      ]
  ;;

  let to_dyn = Repr.to_dyn repr
end

module Action_output_limit = struct
  type t = int

  let default = 100_000
  let to_string = Int.to_string
  let equal = Int.equal
  let repr = Repr.int
  let to_dyn = Repr.to_dyn repr
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

  let to_option = function
    | Unset -> None
    | Set root -> Some root
  ;;

  let repr =
    Repr.variant
      "workspace-root-for-build-prefix-map"
      [ Repr.case0 "Unset" ~test:(equal Unset)
      ; Repr.case "Set" Repr.string ~proj:(function
          | Unset -> None
          | Set root -> Some root)
      ]
  ;;

  let to_dyn = Repr.to_dyn repr
end

type t =
  { action_stdout_on_success : Action_output_on_success.t
  ; action_stderr_on_success : Action_output_on_success.t
  ; action_stdout_limit : Action_output_limit.t
  ; action_stderr_limit : Action_output_limit.t
  ; expand_aliases_in_sandbox : bool
  ; workspace_root_to_build_path_prefix_map : Workspace_root_for_build_prefix_map.t
  ; action_project_root : Path.Source.t option
  ; should_remove_write_permissions_on_generated_files : bool
  }

let equal
      { action_stdout_on_success
      ; action_stderr_on_success
      ; action_stdout_limit
      ; action_stderr_limit
      ; expand_aliases_in_sandbox
      ; workspace_root_to_build_path_prefix_map
      ; action_project_root
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
  && Option.equal Path.Source.equal action_project_root t.action_project_root
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
      ; action_project_root
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
    , action_project_root
    , should_remove_write_permissions_on_generated_files )
;;

let bool_to_int b = if b then 1 else 0

let digest_fields
      ~action_stdout_on_success
      ~action_stderr_on_success
      ~action_stdout_limit
      ~action_stderr_limit
      ~expand_aliases_in_sandbox
      ~workspace_root_to_build_path_prefix_map
      ~action_project_root
      ~should_remove_write_permissions_on_generated_files
  =
  let d = Digest.Manual.create () in
  let root =
    Workspace_root_for_build_prefix_map.to_option workspace_root_to_build_path_prefix_map
  in
  let flags =
    Action_output_on_success.for_digest action_stdout_on_success
    lor (Action_output_on_success.for_digest action_stderr_on_success lsl 2)
    lor (bool_to_int expand_aliases_in_sandbox lsl 4)
    lor (bool_to_int (Option.is_some root) lsl 5)
    lor (bool_to_int should_remove_write_permissions_on_generated_files lsl 6)
  in
  Digest.Manual.int d flags;
  Digest.Manual.int d action_stdout_limit;
  Digest.Manual.int d action_stderr_limit;
  Option.iter root ~f:(Digest.Manual.string d);
  Digest.Manual.option
    d
    ~f:(fun d root -> Digest.Manual.string d (Path.Source.to_string root))
    action_project_root;
  Digest.Manual.get d
;;

let make
      ~action_stdout_on_success
      ~action_stderr_on_success
      ~action_stdout_limit
      ~action_stderr_limit
      ~expand_aliases_in_sandbox
      ~workspace_root_to_build_path_prefix_map
      ~action_project_root
      ~should_remove_write_permissions_on_generated_files
  =
  { action_stdout_on_success
  ; action_stderr_on_success
  ; action_stdout_limit
  ; action_stderr_limit
  ; expand_aliases_in_sandbox
  ; workspace_root_to_build_path_prefix_map
  ; action_project_root
  ; should_remove_write_permissions_on_generated_files
  }
;;

let repr =
  Repr.record
    "execution-parameters"
    [ Repr.field "action_stdout_on_success" Action_output_on_success.repr ~get:(fun t ->
        t.action_stdout_on_success)
    ; Repr.field "action_stderr_on_success" Action_output_on_success.repr ~get:(fun t ->
        t.action_stderr_on_success)
    ; Repr.field "action_stdout_limit" Action_output_limit.repr ~get:(fun t ->
        t.action_stdout_limit)
    ; Repr.field "action_stderr_limit" Action_output_limit.repr ~get:(fun t ->
        t.action_stderr_limit)
    ; Repr.field "expand_aliases_in_sandbox" Repr.bool ~get:(fun t ->
        t.expand_aliases_in_sandbox)
    ; Repr.field
        "workspace_root_to_build_path_prefix_map"
        Workspace_root_for_build_prefix_map.repr
        ~get:(fun t -> t.workspace_root_to_build_path_prefix_map)
    ; Repr.field
        "action_project_root"
        Repr.(option Path.Source.repr)
        ~get:(fun t -> t.action_project_root)
    ; Repr.field
        "should_remove_write_permissions_on_generated_files"
        Repr.bool
        ~get:(fun t -> t.should_remove_write_permissions_on_generated_files)
    ]
;;

let digest
      { action_stdout_on_success
      ; action_stderr_on_success
      ; action_stdout_limit
      ; action_stderr_limit
      ; expand_aliases_in_sandbox
      ; workspace_root_to_build_path_prefix_map
      ; action_project_root
      ; should_remove_write_permissions_on_generated_files
      }
  =
  digest_fields
    ~action_stdout_on_success
    ~action_stderr_on_success
    ~action_stdout_limit
    ~action_stderr_limit
    ~expand_aliases_in_sandbox
    ~workspace_root_to_build_path_prefix_map
    ~action_project_root
    ~should_remove_write_permissions_on_generated_files
;;

let to_dyn = Repr.to_dyn repr

let builtin_default =
  make
    ~action_stdout_on_success:Print
    ~action_stderr_on_success:Print
    ~action_stdout_limit:Action_output_limit.default
    ~action_stderr_limit:Action_output_limit.default
    ~expand_aliases_in_sandbox:true
    ~workspace_root_to_build_path_prefix_map:
      (Workspace_root_for_build_prefix_map.Set "/workspace_root")
    ~action_project_root:None
    ~should_remove_write_permissions_on_generated_files:true
;;

let set_action_stdout_on_success x t = { t with action_stdout_on_success = x }
let set_action_stderr_on_success x t = { t with action_stderr_on_success = x }
let set_action_stdout_limit x t = { t with action_stdout_limit = x }
let set_action_stderr_limit x t = { t with action_stderr_limit = x }
let set_expand_aliases_in_sandbox x t = { t with expand_aliases_in_sandbox = x }

let set_workspace_root_to_build_path_prefix_map x t =
  { t with workspace_root_to_build_path_prefix_map = x }
;;

let set_action_project_root x t = { t with action_project_root = x }

let set_should_remove_write_permissions_on_generated_files x t =
  { t with should_remove_write_permissions_on_generated_files = x }
;;

let expand_aliases_in_sandbox t = t.expand_aliases_in_sandbox
let workspace_root_to_build_path_prefix_map t = t.workspace_root_to_build_path_prefix_map
let action_stdout_on_success t = t.action_stdout_on_success
let action_stderr_on_success t = t.action_stderr_on_success
let action_stdout_limit t = t.action_stdout_limit
let action_stderr_limit t = t.action_stderr_limit
let action_project_root t = t.action_project_root

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
