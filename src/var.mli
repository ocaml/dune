module Kind : sig
  type t =
    | Values of Value.t list
    | Project_root
    | First_dep
    | Deps
    | Targets

  val to_value_no_deps_or_targets : t -> scope:Scope.t -> Value.t list option
end

module Form : sig
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
end

type 'a t =
  | No_info    of 'a
  | Since      of 'a * Syntax.Version.t
  | Deleted_in of 'a * Syntax.Version.t
  | Renamed_in of Syntax.Version.t * string

module Map : sig
  type 'a var
  type 'a t

  val create_vars : context:Context.t -> cxx_flags:string list -> Kind.t t

  val forms : Form.t t

  val static_vars : Kind.t t

  val expand
    :  'a t
    -> syntax_version:Syntax.Version.t
    -> var:String_with_vars.Var.t
    -> 'a option
end with type 'a var := 'a t
