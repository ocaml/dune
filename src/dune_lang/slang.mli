open! Stdune
open Dune_sexp

(** Slang (string-language) is a DSL for computing lists of strings. *)
type t =
  | Nil
  | Literal of String_with_vars.t
  (** A string literal which may contain pforms. It's possible for a single
      unquoted pform to expand to a list of multiple strings. *)
  | Form of (Loc.t * form)

and blang = t Blang.Ast.t

and form =
  | Concat of t list (** Concatenate a list of strings recursively with no delimiter *)
  | When of (blang * t)
  (** If the LHS evaluates to true then this is the RHS, otherwise evaluates to the empty list *)
  | If of
      { condition : blang
      ; then_ : t
      ; else_ : t
      }
  | Has_undefined_var of t
  | Catch_undefined_var of
      { value : t
      ; fallback : t
      }
  | And_absorb_undefined_var of blang list
    (* like [Blang.And] but undefined variable exceptions are only propagated
       if none of the arguments evaluate to `false` *)
  | Or_absorb_undefined_var of blang list
    (* like [Blang.Or] but undefined variable exceptions are only propagated
       if none of the arguments evaluate to true *)
  | Blang of blang (** convert a boolean returned by a blang expression into a string *)

val decode : t Decoder.t
val encode : t Encoder.t
val decode_blang : blang Decoder.t
val encode_blang : blang Encoder.t
val to_dyn : t -> Dyn.t
val loc : t -> Loc.t
val concat : ?loc:Loc.t -> t list -> t
val when_ : ?loc:Loc.t -> blang -> t -> t
val if_ : ?loc:Loc.t -> blang -> then_:t -> else_:t -> t
val has_undefined_var : ?loc:Loc.t -> t -> t
val catch_undefined_var : ?loc:Loc.t -> t -> fallback:t -> t
val and_absorb_undefined_var : ?loc:Loc.t -> blang list -> t
val or_absorb_undefined_var : ?loc:Loc.t -> blang list -> t
val blang : ?loc:Loc.t -> blang -> t
val pform : ?loc:Loc.t -> Pform.t -> t
val text : ?loc:Loc.t -> string -> t
val bool : ?loc:Loc.t -> bool -> t

(** Applies some transformations that preserve the meaning of [t] while
    simplifying its syntactic representation. This is intended to help make
    generated slang expressions more human-readable. *)
val simplify : t -> t

val simplify_blang : blang -> blang
