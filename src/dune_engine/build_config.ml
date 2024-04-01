open Import

module Gen_rules = struct
  module Context_type = Build_config_intf.Context_type

  module Build_only_sub_dirs = struct
    type t = Subdir_set.t Path.Build.Map.t

    let iter_dirs_containing_sub_dirs t ~f =
      Path.Build.Map.iteri t ~f:(fun dir _ -> f dir)
    ;;

    let empty = Path.Build.Map.empty
    let singleton ~dir sub_dirs = Path.Build.Map.singleton dir sub_dirs
    let find t dir = Path.Build.Map.find t dir |> Option.value ~default:Subdir_set.empty
    let union a b = Path.Build.Map.union a b ~f:(fun _ a b -> Some (Subdir_set.union a b))
  end

  module Rules = struct
    include Build_config_intf.Rules

    let empty =
      { build_dir_only_sub_dirs = Path.Build.Map.empty
      ; directory_targets = Path.Build.Map.empty
      ; rules = Memo.return Rules.empty
      }
    ;;

    let create
      ?(build_dir_only_sub_dirs = empty.build_dir_only_sub_dirs)
      ?(directory_targets = empty.directory_targets)
      rules
      =
      { build_dir_only_sub_dirs; directory_targets; rules }
    ;;

    let combine_exn r { build_dir_only_sub_dirs; directory_targets; rules } =
      { build_dir_only_sub_dirs =
          Build_only_sub_dirs.union r.build_dir_only_sub_dirs build_dir_only_sub_dirs
      ; directory_targets = Path.Build.Map.union_exn r.directory_targets directory_targets
      ; rules =
          (let open Memo.O in
           let+ r = r.rules
           and+ r' = rules in
           Rules.union r r')
      }
    ;;
  end

  module Gen_rules_result = struct
    include Build_config_intf.Gen_rules_result

    let redirect_to_parent rules = Redirect_to_parent rules
    let rules_here rules = Rules rules
    let unknown_context = Unknown_context
    let no_rules = rules_here Rules.empty
  end

  module type Rule_generator = Build_config_intf.Rule_generator
end

module type Source_tree = Build_config_intf.Source_tree

type t =
  { contexts : (Build_context.t * Gen_rules.Context_type.t) Context_name.Map.t Memo.Lazy.t
  ; rule_generator : (module Gen_rules.Rule_generator)
  ; sandboxing_preference : Sandbox_mode.t list
  ; promote_source :
      chmod:(int -> int)
      -> delete_dst_if_it_is_a_directory:bool
      -> src:Path.Build.t
      -> dst:Path.Source.t
      -> unit Fiber.t
  ; stats : Dune_stats.t option
  ; cache_config : Dune_cache.Config.t
  ; cache_debug_flags : Cache_debug_flags.t
  ; implicit_default_alias : Path.Build.t -> unit Action_builder.t option Memo.t
  ; execution_parameters : dir:Path.Build.t -> Execution_parameters.t Memo.t
  ; source_tree : (module Source_tree)
  ; action_runner : Action_exec.input -> Action_runner.t option
  ; action_runners : unit -> Action_runner.t list
  ; shared_cache : (module Dune_cache.Shared.S)
  ; write_error_summary : Build_system_error.Set.t -> unit Fiber.t
  }

let t : t Fdecl.t = Fdecl.create Dyn.opaque
let get () = Fdecl.get t

let set
  ~action_runner
  ~action_runners
  ~stats
  ~contexts
  ~promote_source
  ~cache_config
  ~cache_debug_flags
  ~sandboxing_preference
  ~rule_generator
  ~implicit_default_alias
  ~execution_parameters
  ~source_tree
  ~shared_cache
  ~write_error_summary
  =
  let contexts =
    Memo.lazy_ ~name:"Build_config.set" (fun () ->
      let open Memo.O in
      let+ contexts = Memo.Lazy.force contexts in
      Context_name.Map.of_list_map_exn
        contexts
        ~f:(fun ((ctx : Build_context.t), ctx_type) -> ctx.name, (ctx, ctx_type)))
  in
  let () =
    match (cache_config : Dune_cache.Config.t) with
    | Disabled -> ()
    | Enabled _ -> Dune_cache_storage.Layout.create_cache_directories ()
  in
  Fdecl.set
    t
    { contexts
    ; rule_generator
    ; sandboxing_preference =
        sandboxing_preference @ Sandbox_mode.all_except_patch_back_source_tree
    ; promote_source
    ; stats
    ; cache_config
    ; cache_debug_flags
    ; implicit_default_alias
    ; execution_parameters
    ; source_tree
    ; action_runner
    ; action_runners
    ; shared_cache
    ; write_error_summary
    }
;;
