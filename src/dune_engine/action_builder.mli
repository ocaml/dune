open Import

type 'a t

module O : sig
  val ( >>> ) : unit t -> 'a t -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

val return : 'a -> 'a t
val bind : 'a t -> f:('a -> 'b t) -> 'b t
val map : 'a t -> f:('a -> 'b) -> 'b t
val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
val both : 'a t -> 'b t -> ('a * 'b) t
val all : 'a t list -> 'a list t
val all_unit : unit t list -> unit t

module List : Monad.List with type 'a t := 'a t

(** [memoize ?cutoff name t] is an action builder that behaves like [t] except
    that its result is computed only once.

    If the caller provides the [cutoff] equality check, we will use it to check
    if the result of the computation has changed. If it didn't, we will be able
    to skip the recomputation of values that depend on it. *)
val memoize : ?cutoff:('a -> 'a -> bool) -> string -> 'a t -> 'a t

type ('input, 'output) memo

(** Same as [Memo.create] but for [Action_builder] *)
val create_memo
  :  string
  -> input:(module Memo.Input with type t = 'i)
  -> ?cutoff:('o -> 'o -> bool)
  -> ?human_readable_description:('i -> User_message.Style.t Pp.t)
  -> ('i -> 'o t)
  -> ('i, 'o) memo

(** Same as [Memo.exec] but for [Action_builder]'s memos *)
val exec_memo : ('i, 'o) memo -> 'i -> 'o t

(** [goal t] ignores all facts that have been accumulated about the dependencies
    of [t]. For example, [goal (path p)] declares that a path [p] contributes to
    the "goal" of the resulting action builder, which means [p] must be built,
    but the contents of [p] is irrelevant. *)
val goal : 'a t -> 'a t

(** An action builder with no dependencies. Consider passing [Memo.of_thunk] to delay
    forcing the computation until the action's dependencies need to be determined.

    If you're thinking of using [Process.run] here, check that: (i) you don't in
    fact need [Command.run], and that (ii) [Process.run] only reads the declared
    build rule dependencies. *)
val of_memo : 'a Memo.t -> 'a t

(** Record the given set as dependencies of the action produced by the action builder. *)
val record : 'a -> Dep.Set.t -> f:(Dep.t -> Dep.Fact.t Memo.t) -> 'a t

(** Record a given Memo computation as a "dependency" of the action builder, i.e., require
    that it must succeed. Consider passing [Memo.of_thunk] to delay forcing the computation
    until the action's dependencies need to be determined. *)
val record_success : unit Memo.t -> unit t

module Expert : sig
  (** Like [record] but records a dependency on a *source* file. Evaluating the resulting
      [t] in the [Eager] mode will raise a user error if the file can't be digested.

      This function is in the [Expert] module because depending on files in the source
      directory is usually a mistake. As of 2023-11-14, we use this function only for
      setting up the rules that copy files from the source to the build directory. *)
  val record_dep_on_source_file_exn
    :  'a
    -> ?loc:(unit -> Loc.t option Memo.t)
    -> Path.Source.t
    -> 'a t
end

(** {1 Evaluation} *)

(** Evaluate a [t] and collect the set of its dependencies. This avoids doing the build
    work required for finding the facts about those dependencies, so you should use this
    function if you don't need the facts. *)
val evaluate_and_collect_deps : 'a t -> ('a * Dep.Set.t) Memo.t

(** Evaluate a [t] and collect the set of its dependencies along with facts about them.
    Note that finding [t]'s facts requires building all of [t]'s dependencies. *)
val evaluate_and_collect_facts : 'a t -> ('a * Dep.Facts.t) Memo.t

(** only used in the public rules *)
val push_stack_frame
  :  human_readable_description:(unit -> User_message.Style.t Pp.t)
  -> (unit -> 'a t)
  -> 'a t
