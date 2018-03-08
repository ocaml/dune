open! Import

module type Info = Jbuild.Sub_system_info.S

module type S = sig
  module Info : Info

  (** Instantiated representation of the sub-system. I.e. with names
      resolved using a library database. *)
  type t

  (** Create an instance of the sub-system *)
  val instantiate
    :  resolve:(Loc.t * string -> (Lib.t, exn) result)
    -> get:(Lib.t -> t option)
    -> Lib.t
    -> Info.t
    -> t
end

(** A backend, for implementors of the sub-system *)
module type Backend = sig
  include S

  (** Description of a backend, such as "inline tests framework" or
      "ppx driver". *)
  val desc : plural:bool -> string

  (** "a" or "an" *)
  val desc_article : string

  (** Library the backend is attached to *)
  val lib : t -> Lib.t

  (** Dependencies on other backends *)
  val deps : t -> (t list, exn) result option

  (** Dump the sub-system configuration. This is used to generate META
      files. *)
  val to_sexp : t -> Syntax.Version.t * Sexp.t
end

module type Registered_backend = sig
  type t

  val get : Lib.t -> t option

  (** Choose a backend by either using the ones written by the user or
      by by scanning the dependencies.

      The returned list is sorted by order of dependencies. It is not
      allowed to have two different backend that are completely
      independant, i.e. none of them is in the transitive closure of
      the other one. *)
  val select_backends
    :  loc:Loc.t
    -> scope:Scope.t
    -> written_by_user:(Loc.t * string) list option
    -> Lib.t list
    -> (t list, exn) result
end

(* This is probably what we'll give to plugins *)
module Library_compilation_context = struct
  type t =
    { super_context  : Super_context.t
    ; dir            : Path.t
    ; stanza         : Jbuild.Library.t
    ; scope          : Scope.t
    ; source_modules : Module.t Module.Name.Map.t
    ; compile_info   : Lib.Compile.t
    }
end

(** An end-point, for users of the systems *)
module type End_point = sig
  module Backend : Registered_backend

  module Info : sig
    include Info

    (** Additional backends specified by the user at use-site *)
    val backends : t -> (Loc.t * string) list option
  end

  val gen_rules
    :  Library_compilation_context.t
    -> info:Info.t
    -> backends:Backend.t list
    -> unit
end
