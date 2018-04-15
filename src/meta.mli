(** META file parsing/printing *)

open! Import

type t =
  { name    : string
  ; entries : entry list
  }

and entry =
  | Comment of string
  | Rule    of rule
  | Package of t

and rule =
  { var        : string
  ; predicates : predicate list
  ; action     : action
  ; value      : string
  }

and action = Set | Add

and predicate =
  | Pos of string
  | Neg of string

module Simplified : sig
  module Rules : sig
    type t =
      { set_rules : rule list
      ; add_rules : rule list
      }
  end

  type t =
    { name : string
    ; vars : Rules.t String_map.t
    ; subs : t list
    }

  val pp : Format.formatter -> t -> unit
end

val load : fn:string -> name:string -> Simplified.t

(** Builtin META files for libraries distributed with the compiler. For when ocamlfind is
    not installed. *)
val builtins : stdlib_dir:Path.t -> Simplified.t String_map.t

val pp : Format.formatter -> entry list -> unit
