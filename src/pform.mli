module Var : sig
  type t =
    | Values of Value.t list
    | Project_root
    | First_dep
    | Deps
    | Targets

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

type 'a t =
  | No_info    of 'a
  | Since      of 'a * Syntax.Version.t
  | Deleted_in of 'a * Syntax.Version.t * string option
  | Renamed_in of Syntax.Version.t * string

module Map : sig
  type 'a var
  type 'a t

  val create_vars : context:Context.t -> cxx_flags:string list -> Var.t t

  val macros : Macro.t t

  val static_vars : Var.t t

  val expand
    :  'a t
    -> syntax_version:Syntax.Version.t
    -> var:String_with_vars.Var.t
    -> 'a option
end with type 'a var := 'a t
