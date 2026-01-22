open Import

(** Build system's configuration: how to generate rules, how to handle events,
      whether to use the shared cache, etc. *)

module Context_type : sig
  type t =
    | Empty
    | With_sources
end

module Gen_rules : sig
  module Build_only_sub_dirs : sig
    (** The set of either directory targets or internally generated directories,
        indexed by their parent build directory. *)
    type t = Subdir_set.t Path.Build.Map.t

    val empty : t
    val iter_dirs_containing_sub_dirs : t -> f:(Path.Build.t -> unit) -> unit
    val singleton : dir:Path.Build.t -> Subdir_set.t -> t
    val find : t -> Path.Build.t -> Subdir_set.t
    val union : t -> t -> t
  end

  module Rules : sig
    type nonrec t =
      { build_dir_only_sub_dirs : Build_only_sub_dirs.t
        (** Sub-directories that don't exist in the source tree but exists in
        the build directory. This is for internal directories such as
        [.dune] or [.ppx]. *)
      ; directory_targets : Loc.t Path.Build.Map.t
        (** Directories that are target of a rule. For each directory target,
        give the location of the rule that generates it. The keys in this
        map must correspond exactly to the set of directory targets that
        will be produces by [rules]. The values should be the locations of
        the rules that are going to produce these targets. However, it's
        ok to have an approximate location as the rule that produces the
        target will be responsible for producing the final location*)
      ; rules : Rules.t Memo.t
      }

    type rules := Rules.t

    val create
      :  ?build_dir_only_sub_dirs:Build_only_sub_dirs.t
      -> ?directory_targets:Loc.t Path.Build.Map.t
      -> rules Memo.t
      -> t

    val empty : t

    (** Raises a code error if there are multiple rules for the same target. *)
    val combine_exn : t -> t -> t
  end

  module Gen_rules_result : sig
    type t =
      | Rules of Rules.t
      | Unknown_context
      | Redirect_to_parent of Rules.t

    val redirect_to_parent : Rules.t -> t
    val rules_here : Rules.t -> t
    val unknown_context : t
    val no_rules : t
  end

  module type Rule_generator = sig
    (** The rule generator.

      This callback is used to generate the rules for a given directory in the
      corresponding build context. It receives the directory for which to
      generate the rules and the split part of the path after the build context.
      It must return an additional list of sub-directories to keep. This is in
      addition to the ones that are present in the source tree and the ones that
      already contain rules.

      [gen_rules] may only generate rules whose targets are descendant of [dir]. *)
    val gen_rules
      :  Context_name.t
      -> dir:Path.Build.t
      -> string list
      -> Gen_rules_result.t Memo.t
  end
end

module type Source_tree = sig
  module Dir : sig
    type t

    val sub_dir_names : t -> Filename.Set.t
    val filenames : t -> Filename.Set.t
  end

  val find_dir : Path.Source.t -> Dir.t option Memo.t
end

(** Initialise the build system. This must be called before running the build
      system and only once. *)
val set
  :  contexts:(Build_context.t * Context_type.t) list Memo.Lazy.t
  -> promote_source:
       (chmod:(int -> int)
        -> delete_dst_if_it_is_a_directory:bool
        -> src:Path.Build.t
        -> dst:Path.Source.t
        -> unit Fiber.t)
  -> sandboxing_preference:Sandbox_mode.t list
  -> rule_generator:(module Gen_rules.Rule_generator)
  -> implicit_default_alias:(Path.Build.t -> unit Action_builder.t option Memo.t)
  -> execution_parameters:
       (Context_name.t -> dir:Path.Build.t -> Execution_parameters.t Memo.t)
  -> source_tree:(module Source_tree)
  -> unit

type t = private
  { contexts : (Build_context.t * Context_type.t) Context_name.Map.t Memo.Lazy.t
  ; rule_generator : (module Gen_rules.Rule_generator)
  ; sandboxing_preference : Sandbox_mode.t list
  ; promote_source :
      chmod:(int -> int)
      -> delete_dst_if_it_is_a_directory:bool
      -> src:Path.Build.t
      -> dst:Path.Source.t
      -> unit Fiber.t
  ; implicit_default_alias : Path.Build.t -> unit Action_builder.t option Memo.t
  ; execution_parameters :
      Context_name.t -> dir:Path.Build.t -> Execution_parameters.t Memo.t
  ; source_tree : (module Source_tree)
  }

val get : unit -> t
