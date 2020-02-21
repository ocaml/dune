open! Stdune

module Var : sig
  type t =
    | Values of Value.t list
    | Project_root
    | First_dep
    | Deps
    | Targets
    | Target
    | Named_local
    | Cc
    | Cxx
end

module Artifact : sig
  type t =
    | Mod of Cm_kind.t
    | Lib of Mode.t

  val ext : t -> string
end

module Macro : sig
  type t =
    | Exe
    | Dep
    | Bin
    (* All four combinations are allowed and correspond to variables [lib],
       [libexec], [lib-private], and [libexec-private]. *)
    | Lib of
        { lib_exec : bool
        ; lib_private : bool
        }
    | Lib_available
    | Version
    | Read
    | Read_strings
    | Read_lines
    | Path_no_dep
    | Ocaml_config
    | Env
    | Artifact of Artifact.t
end

module Expansion : sig
  type t =
    | Var of Var.t
    | Macro of Macro.t * string

  val to_dyn : t -> Dyn.t

  module Map : Map.S with type key = t
end

module Map : sig
  type t

  val create : context:Context.t -> t

  val superpose : t -> t -> t

  (** Map with all named values as [Named_local] *)
  val of_bindings : _ Bindings.t -> t

  val singleton : string -> Var.t -> t

  val of_list_exn : (string * Var.t) list -> t

  val input_file : Path.t -> t

  val expand : t -> Expansion.t option String_with_vars.expander

  val expand_exn : t -> Expansion.t String_with_vars.expander

  val empty : t

  type stamp

  val to_stamp : t -> stamp

  val to_dyn : t -> Dyn.t
end
