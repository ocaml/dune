open! Import

type var_expansion =
  | Not_found
  | Path  of Path.t
  | Paths of Path.t list
  | Str   of string

module Mini_shexp : sig
  module Ast : sig
    type ('a, 'path) t =
      | Run            of 'path * 'a list
      | Chdir          of 'path * ('a, 'path) t
      | Setenv         of 'a * 'a * ('a, 'path) t
      | With_stdout_to of 'path * ('a, 'path) t
      | Progn          of ('a, 'path) t list
      | Echo           of 'a
      | Create_file    of 'path
      | Cat            of 'path
      | Copy           of 'path * 'path
      | Symlink        of 'path * 'path
      | Copy_and_add_line_directive of 'path * 'path
      | System         of 'a
      | Bash           of 'a
      | Update_file    of 'path * 'a
    val t : 'a Sexp.Of_sexp.t -> 'b Sexp.Of_sexp.t -> ('a, 'b) t Sexp.Of_sexp.t
    val sexp_of_t : 'a Sexp.To_sexp.t -> 'b Sexp.To_sexp.t -> ('a, 'b) t Sexp.To_sexp.t
  end

  type t = (string, Path.t) Ast.t
  val t : t Sexp.Of_sexp.t
  val sexp_of_t : t Sexp.To_sexp.t

  module Unexpanded : sig
    type desc = t
    type t = (String_with_vars.t, String_with_vars.t) Ast.t
    val t : t Sexp.Of_sexp.t
    val sexp_of_t : t Sexp.To_sexp.t
    val fold_vars : t -> init:'a -> f:('a -> string -> 'a) -> 'a
    val expand : Context.t -> Path.t -> t -> f:(string -> var_expansion) -> desc
  end with type desc := t
end

type t =
  { context : Context.t option
  ; dir     : Path.t
  ; action  : Mini_shexp.t
  }

val t : Context.t String_map.t -> t Sexp.Of_sexp.t
val sexp_of_t : t Sexp.To_sexp.t
val exec : t -> unit Future.t

type for_hash
val for_hash : t -> for_hash
