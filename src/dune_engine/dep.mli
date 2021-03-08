open Stdune

type t = private
  | Env of Env.Var.t
  | File of Path.t
  | Alias of Alias.t
  | File_selector of File_selector.t
  | Universe
  | Sandbox_config of Sandbox_config.t

val file : Path.t -> t

val env : Env.Var.t -> t

val universe : t

val file_selector : File_selector.t -> t

val alias : Alias.t -> t

val sandbox_config : Sandbox_config.t -> t

val compare : t -> t -> Ordering.t

(** The evaluation of file predicates is done by the build_system. This
    necessitates a forward declaration to use in this module. *)
val eval_pred : (File_selector.t -> Path.Set.t) Fdecl.t

val peek_alias_expansion : (Alias.t -> Path.Set.t) Fdecl.t

val eval_alias_expansion : (Alias.t -> Path.Set.t Memo.Build.t) Fdecl.t

module Trace : sig
  type t
end

module Set : sig
  include Set.S with type elt = t

  val has_universe : t -> bool

  val sandbox_config : t -> Sandbox_config.t

  (** It's weird to return a [Path.t list] here, but the call site needs it and
      this lets us avoid having to choose between [static_paths] and
      [eval_paths] both of which seem awkward. *)
  val source_tree : Path.t -> t * Path.Set.t

  val of_files : Path.t list -> t

  val of_files_set : Path.Set.t -> t

  val static_paths : t -> Path.Set.t * Alias.t list

  (** A pre-condition for calling [eval_paths] is that the current memoized node
      must have forced the computation of the aliases returned by
      [static_paths]. *)
  val eval_paths : t -> Path.Set.t

  val files_approx : t -> Path.Set.t

  val encode : t -> Dune_lang.t

  val trace : t -> sandbox_mode:Sandbox_mode.t -> env:Env.t -> Trace.t

  val add_paths : t -> Path.Set.t -> t

  val parallel_iter : t -> f:(elt -> unit Memo.Build.t) -> unit Memo.Build.t

  val parallel_iter_files_approx :
    t -> f:(Path.t -> unit Memo.Build.t) -> unit Memo.Build.t

  val eval_dirs : t -> Path.Set.t
end
