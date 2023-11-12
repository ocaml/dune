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

(** If you're thinking of using [Process.run] here, check that: (i) you don't in
    fact need [Command.run], and that (ii) [Process.run] only reads the declared
    build rule dependencies. *)
val of_memo : 'a Memo.t -> 'a t

(** {1 Execution} *)

(** Evaluation mode.

    In [Lazy] mode, dependencies are only collected. In [Eager] mode,
    dependencies are build as soon as they are recorded and their facts are
    returned.

    If you want to both evaluate an action builder and build the collected
    dependencies, using [Eager] mode will increase parallelism. If you only want
    to know the set of dependencies, using [Lazy] will avoid unnecessary work. *)
type 'm eval_mode =
  | Lazy : Dep.Set.t eval_mode
  | Eager : Dep.Facts.t eval_mode

(** Execute an action builder. *)
val run : 'a t -> 'm eval_mode -> ('a * 'm) Memo.t

(** {1 Low-level} *)

type 'a thunk = { f : 'm. 'm eval_mode -> ('a * 'm) Memo.t } [@@unboxed]

val of_thunk : 'a thunk -> 'a t
