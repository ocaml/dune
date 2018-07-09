type t =
  (* Variables *)
  | Values of Value.t list
  | Project_root
  | First_dep
  | Deps
  | Targets
  | Named_local

  (* Macros *)
  | Exe
  | Dep
  | Bin
  | Lib
  | Libexec
  | Lib_available
  | Version
  | Read
  | Read_strings
  | Read_lines
  | Path_no_dep
  | Ocaml_config

module Map : sig
  type pform
  type t

  val create : context:Context.t -> cxx_flags:string list -> t

  val superpose : t -> t -> t

  (** Map with all named values as [Named_local] *)
  val of_bindings : _ Jbuild.Bindings.t -> t

  val singleton : string -> pform -> t

  val of_list_exn : (string * pform) list -> t

  val expand
    :  t
    -> syntax_version:Syntax.Version.t
    -> pform:String_with_vars.Var.t
    -> pform option

  val empty : t
end with type pform := t
