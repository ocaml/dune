open Stdune

module Flags : sig
  type 'flag t =
    { compile : 'flag
    ; link    : 'flag
    }

  val empty : string list t

  val default : profile:string -> string list t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  type user_written = Ordered_set_lang.Unexpanded.t t
end

module In_buildable : sig
  type t =
    { flags            : Flags.user_written
    ; javascript_files : string list
    }

  val field : t Sexp.Of_sexp.fields_parser

  val flags : t -> Flags.user_written

  val default : t
end

module Compilation : sig
  type t =
    | Separate
    | Classic

  val default : profile:string -> t
end

module Env : sig
  type t =
    { flags       : Flags.user_written
    ; compilation : Compilation.t option
    }

  val field : t Sexp.Of_sexp.fields_parser
end
