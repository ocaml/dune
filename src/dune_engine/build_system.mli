(** The core of the build system *)

open Import
module Action_builder := Action_builder0

(** Build a file and return the digest of its contents. *)
val build_file : Path.t -> Digest.t Memo.t

(** Build a file and access its contents with [f]. *)
val read_file : Path.t -> f:(Path.t -> 'a) -> 'a Memo.t

(** Return [true] if a file exists or is buildable *)
val file_exists : Path.t -> bool Memo.t

(** Build a set of dependencies and return learned facts about them. *)
val build_deps : Dep.Set.t -> Dep.Facts.t Memo.t

(** [eval_pred glob] returns the list of files in [File_selector.dir glob] that
    matches [File_selector.predicate glob]. The list of files includes the list
    of file targets. Currently, this function ignores directory targets, which
    is a limitation we'd like to remove in future. *)
val eval_pred : File_selector.t -> Path.Set.t Memo.t

(** Same as [eval_pred] with [Predicate.true_] as predicate. *)
val files_of : dir:Path.t -> Path.Set.t Memo.t

(** Like [eval_pred] but also builds the resulting set of files. This function
    doesn't have [eval_pred]'s limitation about directory targets and takes them
    into account. *)
val build_pred : File_selector.t -> Dep.Fact.Files.t Memo.t

(** Execute an action. The execution is cached. *)
val execute_action :
  observing_facts:Dep.Facts.t -> Rule.Anonymous_action.t -> unit Memo.t

(** Execute an action and capture its stdout. The execution is cached. *)
val execute_action_stdout :
  observing_facts:Dep.Facts.t -> Rule.Anonymous_action.t -> string Memo.t

type rule_execution_result =
  { deps : Dep.Fact.t Dep.Map.t
  ; targets : Digest.t Path.Build.Map.t
  }

val execute_rule : Rule.t -> rule_execution_result Memo.t

val dep_on_alias_definition :
  Rules.Dir_rules.Alias_spec.item -> unit Action_builder.t

(** {2 Running the build system} *)

val run : (unit -> 'a Memo.t) -> ('a, [ `Already_reported ]) Result.t Fiber.t

(** A variant of [run] that raises an [Already_reported] exception on error. *)
val run_exn : (unit -> 'a Memo.t) -> 'a Fiber.t

(** {2 Misc} *)

module Progress : sig
  type t =
    { number_of_rules_discovered : int
    ; number_of_rules_executed : int
    }

  val equal : t -> t -> bool

  val complete : t -> int

  val remaining : t -> int
end

module State : sig
  type t =
    | Initializing
    | Building of Progress.t
    | Restarting_current_build
    | Build_succeeded__now_waiting_for_changes
    | Build_failed__now_waiting_for_changes

  val equal : t -> t -> bool
end

val state : State.t Fiber.Svar.t

(** Errors found when building targets. *)
module Error : sig
  type t

  module Id : sig
    type t

    module Map : Map.S with type key = t

    val compare : t -> t -> Ordering.t

    val to_int : t -> int

    val to_dyn : t -> Dyn.t
  end

  module Event : sig
    type nonrec t =
      | Add of t
      | Remove of t
  end

  module Set : sig
    type error := t

    type t

    (** [one_event_diff ~prev ~next] returns the event that constructs [next]
        from [prev] if [next] is in the successive "generation" of [prev] *)
    val one_event_diff : prev:t -> next:t -> Event.t option

    val equal : t -> t -> bool

    val current : t -> error Id.Map.t

    val empty : t
  end

  val create : exn:Exn_with_backtrace.t -> t

  val info : t -> User_message.t * User_message.t list * Path.t option

  val promotion : t -> Diff_promotion.Annot.t option

  val id : t -> Id.t
end

(** The current set of active errors. *)
val errors : Error.Set.t Fiber.Svar.t
