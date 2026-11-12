open Import
open Action_types

module Action_output_limit = struct
  type t = int

  let default = 100_000
  let equal = Int.equal
  let repr = Repr.int
end

module Workspace_root_for_build_prefix_map = struct
  type t =
    | Unset
    | Set of string

  (* CR-soon rgrinberg: remove this *)
  let equal x y =
    match x, y with
    | Unset, Unset -> true
    | Unset, _ | _, Unset -> false
    | Set x, Set y -> String.equal x y
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
end

module Promotion = struct
  type t =
    | Disabled
    | Enabled of
        { context : Context_name.t
        ; workspace_sources : Filename.Set.t Context_name.Map.t
        }

  let disabled = Disabled

  let workspace_source workspace_sources context source =
    if Path.Source.is_in_build_dir source
    then None
    else (
      match Context_name.Map.find workspace_sources context with
      | None -> None
      | Some build_only_sub_dirs ->
        (match Path.Source.split_first_component source with
         | Some (name, _) when Filename.Set.mem build_only_sub_dirs name -> None
         | Some _ | None -> Some source))
  ;;

  let destination t path =
    match t with
    | Disabled -> None
    | Enabled { context; workspace_sources } ->
      (match Path.drop_optional_sandbox_root path with
       | In_source_tree source -> workspace_source workspace_sources context source
       | External _ -> None
       | In_build_dir path ->
         (match Dpath.Target_dir.of_target path with
          | Regular (With_context (context, source))
          | Anonymous_action (With_context (context, source)) ->
            workspace_source workspace_sources context source
          | Regular Root | Anonymous_action Root | Invalid _ -> None))
  ;;

  let for_rule ~workspace_sources ~dir =
    match Dpath.Target_dir.of_target dir with
    | Regular (With_context (context, source))
    | Anonymous_action (With_context (context, source)) ->
      (match workspace_source workspace_sources context source with
       | None -> Disabled
       | Some _ -> Enabled { context; workspace_sources })
    | Regular Root | Anonymous_action Root | Invalid _ -> Disabled
  ;;

  let is_enabled = function
    | Disabled -> false
    | Enabled _ -> true
  ;;

  let equal a b =
    match a, b with
    | Disabled, Disabled -> true
    | Enabled a, Enabled b ->
      Context_name.equal a.context b.context
      && Context_name.Map.equal
           a.workspace_sources
           b.workspace_sources
           ~equal:Filename.Set.equal
    | Disabled, Enabled _ | Enabled _, Disabled -> false
  ;;

  let repr =
    Repr.variant
      "promotion"
      [ Repr.case0 "Disabled" ~test:(fun t -> not (is_enabled t))
      ; Repr.case
          "Enabled"
          Repr.(pair string (list (pair string (list Filename.repr))))
          ~proj:(function
            | Disabled -> None
            | Enabled { context; workspace_sources } ->
              let sources =
                Context_name.Map.to_list workspace_sources
                |> List.map ~f:(fun (context, dirs) ->
                  Context_name.to_string context, Filename.Set.to_list dirs)
              in
              Some (Context_name.to_string context, sources))
      ]
  ;;
end

type t =
  { action_stdout_on_success : Action_output_on_success.t
  ; action_stderr_on_success : Action_output_on_success.t
  ; action_stdout_limit : Action_output_limit.t
  ; action_stderr_limit : Action_output_limit.t
  ; expand_aliases_in_sandbox : bool
  ; workspace_root_to_build_path_prefix_map : Workspace_root_for_build_prefix_map.t
  ; action_project_root : Path.Local.t option
  ; sandbox_actions : bool
  ; use_sandbox_policy : bool
  ; promotion : Promotion.t
  }

let equal
      { action_stdout_on_success
      ; action_stderr_on_success
      ; action_stdout_limit
      ; action_stderr_limit
      ; expand_aliases_in_sandbox
      ; workspace_root_to_build_path_prefix_map
      ; action_project_root
      ; sandbox_actions
      ; use_sandbox_policy
      ; promotion
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
  && Option.equal Path.Local.equal action_project_root t.action_project_root
  && Bool.equal sandbox_actions t.sandbox_actions
  && Bool.equal use_sandbox_policy t.use_sandbox_policy
  && Promotion.equal promotion t.promotion
;;

let bool_to_int b = if b then 1 else 0

let digest
      { action_stdout_on_success
      ; action_stderr_on_success
      ; action_stdout_limit
      ; action_stderr_limit
      ; expand_aliases_in_sandbox
      ; workspace_root_to_build_path_prefix_map
      ; action_project_root
      ; sandbox_actions
      ; use_sandbox_policy
      ; promotion
      }
  =
  let d = Digest.Manual.create () in
  let root_is_set =
    match workspace_root_to_build_path_prefix_map with
    | Unset -> false
    | Set _ -> true
  in
  let flags =
    Action_output_on_success.for_digest action_stdout_on_success
    lor (Action_output_on_success.for_digest action_stderr_on_success lsl 2)
    lor (bool_to_int expand_aliases_in_sandbox lsl 4)
    lor (bool_to_int root_is_set lsl 5)
    lor (bool_to_int sandbox_actions lsl 6)
    lor (bool_to_int use_sandbox_policy lsl 7)
  in
  Digest.Manual.int d flags;
  Digest.Manual.repr d Promotion.repr promotion;
  Digest.Manual.int d action_stdout_limit;
  Digest.Manual.int d action_stderr_limit;
  (match workspace_root_to_build_path_prefix_map with
   | Unset -> ()
   | Set root -> Digest.Manual.string d root);
  Digest.Manual.option
    d
    ~f:(fun d root -> Digest.Manual.string d (Path.Local.to_string root))
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
      ~sandbox_actions
      ~use_sandbox_policy
      ~promotion
  =
  { action_stdout_on_success
  ; action_stderr_on_success
  ; action_stdout_limit
  ; action_stderr_limit
  ; expand_aliases_in_sandbox
  ; workspace_root_to_build_path_prefix_map
  ; action_project_root
  ; sandbox_actions
  ; use_sandbox_policy
  ; promotion
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
        Repr.(option Path.Local.repr)
        ~get:(fun t -> t.action_project_root)
    ; Repr.field "sandbox_actions" Repr.bool ~get:(fun t -> t.sandbox_actions)
    ; Repr.field "use_sandbox_policy" Repr.bool ~get:(fun t -> t.use_sandbox_policy)
    ; Repr.field "promotion" Promotion.repr ~get:(fun t -> t.promotion)
    ]
;;

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
    ~sandbox_actions:false
    ~use_sandbox_policy:false
    ~promotion:Promotion.disabled
;;

let set_action_stdout_on_success x t = { t with action_stdout_on_success = x }
let set_action_stderr_on_success x t = { t with action_stderr_on_success = x }
let set_expand_aliases_in_sandbox x t = { t with expand_aliases_in_sandbox = x }

let set_workspace_root_to_build_path_prefix_map x t =
  { t with workspace_root_to_build_path_prefix_map = x }
;;

let set_action_project_root x t = { t with action_project_root = x }
let set_sandbox_actions x t = { t with sandbox_actions = x }
let set_use_sandbox_policy x t = { t with use_sandbox_policy = x }
let set_promotion x t = { t with promotion = x }
let expand_aliases_in_sandbox t = t.expand_aliases_in_sandbox
let workspace_root_to_build_path_prefix_map t = t.workspace_root_to_build_path_prefix_map
let action_stdout_on_success t = t.action_stdout_on_success
let action_stderr_on_success t = t.action_stderr_on_success
let action_stdout_limit t = t.action_stdout_limit
let action_stderr_limit t = t.action_stderr_limit
let action_project_root t = t.action_project_root
let use_sandbox_policy t = t.use_sandbox_policy
let promotion t = t.promotion
let default = Fdecl.create Dyn.opaque
let init t = Fdecl.set default t

let default =
  let open Memo.O in
  let* () = Memo.return () in
  Fdecl.get default
;;
