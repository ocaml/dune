open Import
module Action_builder = Action_builder0

module Context_or_install = struct
  type t =
    | Install of Context_name.t
    | Context of Context_name.t

  let to_dyn = function
    | Install ctx -> Dyn.List [ Dyn.String "install"; Context_name.to_dyn ctx ]
    | Context s -> Context_name.to_dyn s
end

module Rules = struct
  type t =
    { build_dir_only_sub_dirs : Subdir_set.t
    ; directory_targets : Loc.t Path.Build.Map.t
    ; rules : Rules.t Memo.t
    }

  let empty =
    { build_dir_only_sub_dirs = Subdir_set.empty
    ; directory_targets = Path.Build.Map.empty
    ; rules = Memo.return Rules.empty
    }

  let combine_exn r { build_dir_only_sub_dirs; directory_targets; rules } =
    { build_dir_only_sub_dirs =
        Subdir_set.union r.build_dir_only_sub_dirs build_dir_only_sub_dirs
    ; directory_targets =
        Path.Build.Map.union_exn r.directory_targets directory_targets
    ; rules =
        (let open Memo.O in
        let+ r = r.rules
        and+ r' = rules in
        Rules.union r r')
    }
end

type gen_rules_result =
  | Rules of Rules.t
  | Unknown_context_or_install
  | Redirect_to_parent of Rules.t

module type Rule_generator = sig
  val gen_rules :
       Context_or_install.t
    -> dir:Path.Build.t
    -> string list
    -> gen_rules_result Memo.t
end

type t =
  { contexts : Build_context.t Context_name.Map.t Memo.Lazy.t
  ; rule_generator : (module Rule_generator)
  ; sandboxing_preference : Sandbox_mode.t list
  ; promote_source :
         chmod:(int -> int)
      -> delete_dst_if_it_is_a_directory:bool
      -> src:Path.Build.t
      -> dst:Path.Source.t
      -> Build_context.t option
      -> unit Fiber.t
  ; stats : Dune_stats.t option
  ; cache_config : Dune_cache.Config.t
  ; cache_debug_flags : Cache_debug_flags.t
  ; implicit_default_alias : Path.Build.t -> unit Action_builder.t option Memo.t
  }

let t = Fdecl.create Dyn.opaque

let set ~stats ~contexts ~promote_source ~cache_config ~cache_debug_flags
    ~sandboxing_preference ~rule_generator ~implicit_default_alias =
  let contexts =
    Memo.lazy_ ~name:"Build_config.set" (fun () ->
        let open Memo.O in
        let+ contexts = Memo.Lazy.force contexts in
        Context_name.Map.of_list_map_exn contexts ~f:(fun c ->
            (c.Build_context.name, c)))
  in
  let () =
    match (cache_config : Dune_cache.Config.t) with
    | Disabled -> ()
    | Enabled _ -> Dune_cache_storage.Layout.create_cache_directories ()
  in
  Fdecl.set t
    { contexts
    ; rule_generator
    ; sandboxing_preference =
        sandboxing_preference @ Sandbox_mode.all_except_patch_back_source_tree
    ; promote_source
    ; stats
    ; cache_config
    ; cache_debug_flags
    ; implicit_default_alias
    }

let get () = Fdecl.get t
