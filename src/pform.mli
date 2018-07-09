module Var : sig
  type t =
    | Values of Value.t list
    | Project_root
    | First_dep
    | Deps
    | Targets
    | Named_local

  val to_value_no_deps_or_targets : t -> scope:Scope.t -> Value.t list option
end

module Macro : sig
  type t =
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
end

module Map : sig
  type 'a t

  val create_vars : context:Context.t -> cxx_flags:string list -> Var.t t

  val macros : Macro.t t

  val static_vars : Var.t t

  val superpose : 'a t -> 'a t -> 'a t

  val of_bindings : 'a Jbuild.Bindings.t -> Var.t t

  val singleton : string -> 'a -> 'a t

  val of_list_exn : (string * 'a) list -> 'a t

  val expand
    :  'a t
    -> syntax_version:Syntax.Version.t
    -> var:String_with_vars.Var.t
    -> 'a option

  val empty : 'a t
end
