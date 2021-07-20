(** Build rules *)

open! Stdune
open! Import
module Action_builder := Action_builder0

(** {1 Setup} *)

(** {2 Creation} *)

module Error : sig
  (** Errors when building a target *)
  type t

  val info : t -> User_message.t * User_message.t list * Path.t option

  val promotion : t -> Promotion.Annot.t option

  val id : t -> int
end

(** The current set of active errors *)
val errors : unit -> Error.t list

module Context_or_install : sig
  type t =
    | Install of Context_name.t
    | Context of Context_name.t

  val to_dyn : t -> Dyn.t
end

module Subdir_set : sig
  type t =
    | All
    | These of String.Set.t

  val empty : t

  val union : t -> t -> t

  val union_all : t list -> t

  val mem : t -> string -> bool
end

type extra_sub_directories_to_keep = Subdir_set.t

module type Rule_generator = sig
  (** The rule generator.

      This callback is used to generate the rules for a given directory in the
      corresponding build context. It receives the directory for which to
      generate the rules and the split part of the path after the build context.
      It must return an additional list of sub-directories to keep. This is in
      addition to the ones that are present in the source tree and the ones that
      already contain rules.

      It is expected that [gen_rules] only generate rules whose targets are
      descendant of [dir].

      The callback should return [None] if it doesn't know about the given
      [Context_or_install.t]. *)
  val gen_rules :
       Context_or_install.t
    -> dir:Path.Build.t
    -> string list
    -> (extra_sub_directories_to_keep * Rules.t) option Memo.Build.t

  (** [global_rules] is a way to generate rules in arbitrary directories
      upfront. *)
  val global_rules : Rules.t Memo.Lazy.t
end

module Handler : sig
  (** Callbacks for build related events *)
  type t

  type event =
    | Start  (** New build started *)
    | Finish  (** Build finished successfully *)
    | Fail
    | Interrupt

  type error =
    | Add of Error.t  (** Error encountered while building *)
    | Remove of Error.t  (** An existing error is invalidated *)

  val create :
       error:(error list -> unit Fiber.t) (** Callback for build [error] *)
    -> build_progress:(complete:int -> remaining:int -> unit Fiber.t)
         (** Callback whenever there's build progress to report *)
    -> build_event:(event -> unit Fiber.t) (** Called for every [event] *)
    -> t
end

(** Initializes the build system. This must be called first. *)
val init :
     stats:Dune_stats.t option
  -> contexts:Build_context.t list Memo.Lazy.t
  -> promote_source:
       (   ?chmod:(int -> int)
        -> src:Path.Build.t
        -> dst:Path.Source.t
        -> Build_context.t option
        -> unit Fiber.t)
  -> cache_config:Dune_cache.Config.t
  -> cache_debug_flags:Cache_debug_flags.t
  -> sandboxing_preference:Sandbox_mode.t list
  -> rule_generator:(module Rule_generator)
  -> handler:Handler.t option
  -> implicit_default_alias:
       (Path.Build.t -> unit Action_builder.t option Memo.Build.t)
  -> unit

(** {2 Primitive for rule generations} *)

(** [eval_pred glob] returns the list of files in [File_selector.dir glob] that
    matches [File_selector.predicate glob]. The list of files includes the list
    of targets. *)
val eval_pred : File_selector.t -> Path.Set.t Memo.Build.t

(** Same as [eval_pred] but also build the resulting set of files. *)
val build_pred : File_selector.t -> Dep.Fact.Files.t Memo.Build.t

(** Returns the set of targets in the given directory. *)
val targets_of : dir:Path.t -> Path.Set.t Memo.Build.t

(** Load the rules for this directory. *)
val load_dir : dir:Path.t -> unit Memo.Build.t

(** Assuming [files] is the list of files in [_build/install] that belong to
    package [pkg], [package_deps t pkg files] is the set of direct package
    dependencies of [package]. *)
val package_deps :
     packages_of:(Path.Build.t -> Package.Id.Set.t Memo.Build.t)
  -> Package.t
  -> Path.Set.t
  -> Package.Id.Set.t Memo.Build.t

(** {1 Requests} *)

(** Build a file and return the digest of its contents *)
val build_file : Path.t -> Digest.t Memo.Build.t

(** Build a file and return its contents with [f] *)
val read_file : Path.t -> f:(Path.t -> 'a) -> 'a Memo.Build.t

(** Return [true] if a file exists or is buildable *)
val file_exists : Path.t -> bool Memo.Build.t

val alias_exists : Alias.t -> bool Memo.Build.t

val is_target : Path.t -> bool Memo.Build.t

val build_deps : Dep.Set.t -> Dep.Facts.t Memo.Build.t

(** Execute a action. The execution is cached. *)
val execute_action :
  observing_facts:Dep.Facts.t -> Rule.Anonymous_action.t -> unit Memo.Build.t

(** Execute a action and capture its output. The execution is cached. *)
val execute_action_stdout :
  observing_facts:Dep.Facts.t -> Rule.Anonymous_action.t -> string Memo.Build.t

(** Return the rule that has the given file has target, if any *)
val get_rule : Path.t -> Rule.t option Memo.Build.t

type alias_definition

val dep_on_alias_definition : alias_definition -> unit Action_builder.t

(** Return the definition of an alias *)
val get_alias_definition :
  Alias.t -> (Loc.t * alias_definition) list Memo.Build.t

(** List of all buildable targets. *)
val all_targets : unit -> Path.Build.Set.t Memo.Build.t

(** The set of files that were created in the source tree and need to be
    deleted. *)
val files_in_source_tree_to_delete : unit -> Path.Set.t

(** {2 Running a build} *)

val run :
  (unit -> 'a Memo.Build.t) -> ('a, [ `Already_reported ]) Result.t Fiber.t

(** A variant of [run] that raises an [Already_reported] exception on error. *)
val run_exn : (unit -> 'a Memo.Build.t) -> 'a Fiber.t

(** {2 Misc} *)

module Progress : sig
  type t =
    { number_of_rules_discovered : int
    ; number_of_rules_executed : int
    }
end

val get_current_progress : unit -> Progress.t

(** Returns the last event reported to the handler *)
val last_event : unit -> Handler.event option
