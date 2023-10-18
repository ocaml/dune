(** This module maps between the opam and 0install concepts. Roughly:
    
    - An opam package name is a 0install role.
    - An opam package is a 0install implementation.
    - An opam version formula is a 0install restriction.

    For dependencies:

    - depends become "essential" dependencies
    - depopts are ignored (the opam solver ignores them too; they don't have constraints)
    - conflicts become "restricts" (with the test reversed)

    Dependencies on alternatives (e.g. "ocaml-base-compiler | ocaml-variants")
    become a dependency on a virtual package which has each choice as an
    implementation. *)

module Make (Monad : S.Monad) (Context : S.CONTEXT with type 'a monad = 'a Monad.t) : sig
  include Zeroinstall_solver.S.SOLVER_INPUT with type rejection = Context.rejection and type 'a monad = 'a Monad.t

  val role : Context.t -> OpamPackage.Name.t -> Role.t

  val version : impl -> OpamPackage.t option
  (** [version impl] is the Opam package for [impl], if any.
      Virtual and dummy implementations return [None]. *)

  val virtual_role : impl list -> Role.t
  (** [virtual_role impls] is a virtual package name with candidates [impls].
      This is used if the user requests multiple packages on the command line
      (the single [impl] will also be virtual). *)

  val virtual_impl : context:Context.t -> depends:OpamPackage.Name.t list -> unit -> impl
  (** [virtual_impl ~context ~depends] is a virtual package which just depends
      on [depends]. This is used if the user requests multiple packages on the
      command line - each requested package becomes a dependency of the virtual
      implementation. *)

  val package_name : Role.t -> OpamPackage.Name.t option
  (** [package_name role] is the Opam package name for [role], if any.
      Return [None] on virtual roles. *)

  val formula : restriction -> [`Ensure | `Prevent] * OpamFormula.version_formula
  (** [formula restriction] returns the version formula represented by this
      restriction along with its negation status: [(`Prevent, formula)] roughly
      means [not formula]. *)
end
