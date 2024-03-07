(** Extra information for the OCaml stdlib.

    Contrary to normal libraries, the library interface of the stdlib (the
    Stdlib module) is used as the alias module when compiling all the other
    modules. We cannot generate an implicit one as that would break hard-coded
    names inside the compiler. *)
open Import

type t = private
  { modules_before_stdlib : Module_name.Set.t
  (** Modules that the Stdlib module depend on. *)
  ; exit_module : Module_name.t option
  (** Modules that are implicitly added by the compiler at the end when
      linking an executable *)
  ; internal_modules : Predicate_lang.Glob.t
  (** Module names that are hardcoded in the compiler and so cannot be
      wrapped *)
  ; loc : Loc.t
  }

val syntax : Dune_lang.Syntax.t
val decode : t Dune_lang.Decoder.t
