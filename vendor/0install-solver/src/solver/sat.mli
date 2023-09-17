(* Copyright (C) 2013, Thomas Leonard
 * See the README file for details, or visit http://0install.net.
 *)

(** A general purpose SAT solver. *)

module type USER =
  sig
    type t
    val pp : Format.formatter -> t -> unit
  end

module Make (User : USER) : sig
  (** A SAT problem consists of a set of variables and a set of clauses which must be satisfied. *)
  type t

  type var_value = True | False | Undecided

  (** A literal is either a variable (e.g. [A]) or a negated variable ([not A]). *)
  type lit
  val neg : lit -> lit

  val add_variable : t -> User.t -> lit

  (** A clause is a boolean expression made up of literals. e.g. [A and B and not(C)] *)
  type clause

  (** {2 Setting up the problem.} *)

  (** Create a problem. *)
  val create : unit -> t

  (** Get the assignment for this literal in the discovered solution. *)
  type solution = lit -> bool

  (** Indicate that the problem is unsolvable, before even starting. This is a convenience
      feature so that clients don't need a separate code path for problems they discover
      during setup vs problems discovered by the solver. *)
  val impossible : t -> unit -> unit

  (** Add a clause requiring at least one literal to be [True]. e.g. [A or B or not(C)].
      [reason] is used in debug messages. *)
  val at_least_one : t -> ?reason:string -> lit list -> unit

  (** If the first variable is true, at least one of the others must be.
      [implies p a bs] is equivalent to [at_least_one p ((neg a) :: bs)].
      [reason] is used in debug messages. *)
  val implies : t -> ?reason:string -> lit -> lit list -> unit

  type at_most_one_clause

  (** Add a clause preventing more than one literal in the list from being [True].
      @raise Invalid_argument if the list contains duplicates. *)
  val at_most_one : t -> lit list -> at_most_one_clause

  (** [run_solver decider] tries to solve the SAT problem. It simplifies it as much as possible first. When it
      has two paths which both appear possible, it calls [decider ()] to choose which to explore first. If this
      leads to a solution, it will be used. If not, the other path will be tried. If [decider] returns [None],
      we try setting the remaining variables to [False] ([decider] will not be called again unless we backtrack).
      Use this to tidy up at the end, when you no longer care about the order. *)
  val run_solver : t -> (unit -> lit option) -> solution option

  (** Return the first literal in the list whose value is [Undecided], or [None] if they're all decided.
      The decider function may find this useful. *)
  val get_best_undecided : at_most_one_clause -> lit option

  (** Return the selected literal, if any. *)
  val get_selected : at_most_one_clause -> lit option

  (** {2 Debugging} *)

  type reason = Clause of clause | External of string

  val lit_value : lit -> var_value
  val get_user_data_for_lit : lit -> User.t
  val explain_reason : lit -> string
end
