open! Stdune
module Action_builder = Action_builder0

module Context_or_install = struct
  type t =
    | Install of Context_name.t
    | Context of Context_name.t

  let to_dyn = function
    | Install ctx -> Dyn.List [ Dyn.String "install"; Context_name.to_dyn ctx ]
    | Context s -> Context_name.to_dyn s
end

type extra_sub_directories_to_keep = Subdir_set.t

type gen_rules_result =
  | Rules of extra_sub_directories_to_keep * Rules.t
  | Unknown_context_or_install
  | Redirect_to_parent

module type Rule_generator = sig
  val gen_rules :
       Context_or_install.t
    -> dir:Path.Build.t
    -> string list
    -> gen_rules_result Memo.Build.t
end

module Error = struct
  module Id = Id.Make ()

  type t =
    { exn : Exn_with_backtrace.t
    ; id : Id.t
    }

  let create ~exn = { exn; id = Id.gen () }

  let id t = t.id

  let promotion t =
    let e =
      match t.exn.exn with
      | Memo.Error.E e -> Memo.Error.get e
      | e -> e
    in
    match e with
    | User_error.E msg ->
      User_message.Annots.find msg.annots Diff_promotion.Annot.annot
    | _ -> None

  let info (t : t) =
    let e =
      match t.exn.exn with
      | Memo.Error.E e -> Memo.Error.get e
      | e -> e
    in
    match e with
    | User_error.E msg -> (
      let dir =
        User_message.Annots.find msg.annots Process.with_directory_annot
      in
      match User_message.Annots.find msg.annots Compound_user_error.annot with
      | None -> (msg, [], dir)
      | Some { main; related } -> (main, related, dir))
    | e ->
      (* CR-someday jeremiedimino: Use [Report_error.get_user_message] here. *)
      (User_message.make [ Pp.text (Printexc.to_string e) ], [], None)
end

module Handler = struct
  (* CR-someday amokhov: The name [Interrupt] is not precise, because the build
     is restarted, not, e.g. interrupted with Ctrl-C. Similarly, we have other
     imprecise names, like [Cancelled_due_to_file_changes] in [Scheduler] where
     the build is not just cancelled, it's restarted. We should make the naming
     more consistent. *)
  type event =
    | Start
    | Finish
    | Fail
    | Interrupt

  type error =
    | Add of Error.t
    | Remove of Error.t

  type t =
    { errors : error list -> unit Fiber.t
    ; build_progress : complete:int -> remaining:int -> unit Fiber.t
    ; build_event : event -> unit Fiber.t
    }

  let report_progress t ~rule_done ~rule_total =
    t.build_progress ~complete:rule_done ~remaining:(rule_total - rule_done)

  let last_event : event option ref = ref None

  let report_build_event t evt =
    last_event := Some evt;
    t.build_event evt

  let do_nothing =
    { errors = (fun _ -> Fiber.return ())
    ; build_progress = (fun ~complete:_ ~remaining:_ -> Fiber.return ())
    ; build_event = (fun _ -> Fiber.return ())
    }

  let create ~errors ~build_progress ~build_event =
    { errors; build_progress; build_event }
end

type t =
  { contexts : Build_context.t Context_name.Map.t Memo.Lazy.t
  ; rule_generator : (module Rule_generator)
  ; sandboxing_preference : Sandbox_mode.t list
  ; handler : Handler.t
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
  ; implicit_default_alias :
      Path.Build.t -> unit Action_builder.t option Memo.Build.t
  }

let t = Fdecl.create Dyn.opaque

let set ~stats ~contexts ~promote_source ~cache_config ~cache_debug_flags
    ~sandboxing_preference ~rule_generator ~handler ~implicit_default_alias =
  let contexts =
    Memo.lazy_ ~name:"Build_config.set" (fun () ->
        let open Memo.Build.O in
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
    ; handler = Option.value handler ~default:Handler.do_nothing
    ; promote_source
    ; stats
    ; cache_config
    ; cache_debug_flags
    ; implicit_default_alias
    }

let get () = Fdecl.get t
