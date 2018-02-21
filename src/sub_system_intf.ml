open! Import

module type S = sig
  (** Name of the system. Used in jbuild and META files *)
  val name : Sub_system_name.t

  (** Raw informations *)
  module Info : sig
    type t
    val of_sexp : t Sexp.Of_sexp.t

    (** For the short syntax: [(name)] *)
    val short : t option
  end

  (** Instantiated informations *)
  type t

  (** Create an instance of the sub-system *)
  val instantiate : Lib.DB.t -> Info.t -> t
end

(** Representation of the sub-system backend *)
module type Backend = sig
  include S

  (** Dump the sub-system configuration. This is used to generate META
      files. *)
  val to_sexp : t Sexp.To_sexp.t
end

(* This is probably what we'll give to plugins *)
module Library_compilation_context = struct
  type t =
    { super_context  : Super_context.t
    ; dir            : Path.t
    ; stanza         : Jbuild.Library.t
    ; scope          : Scope.t
    ; source_modules : Module.t String_map.t
    ; compile_info   : Lib.Compile.t
    }
end

(** A sub-system that takes multiple backends. The backends are
    obtained by scanning the dependencies of the library where the
    sub-system is being used. *)
module type Multi_backends = sig
  module Backend : Backend

  val name : Sub_system_name.t

  module Info : sig
    type t
    val of_sexp : t Sexp.Of_sexp.t
    val short : t option

    (** Additional backends specified by the user at use-site *)
    val backends : t -> (Loc.t * string) list
  end

  val gen_rules
    :  Library_compilation_context.t
    -> info:Info.t
    -> backends:Backend.t list
    -> unit
end
