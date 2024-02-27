open Stdune

type t

include Comparable_intf.S with type key := t
include Dune_sexp.Conv.S with type t := t
module Infix : Comparator.OPS with type t = t
include Dune_util.Stringlike with type t := t

val dune_site_syntax : Dune_sexp.Syntax.t

module Modulelike (S : sig
    type t

    (** The name of the module, for use in error messages. For example
        ["Lib_name"], ["Context_name"]. *)
    val module_ : string

    (** A short description of the type, for use in user-facing error messages.
        For example "context name", "library name". *)
    val description : string

    val to_string : t -> string

    (** The string is always a correct module name, except not capitalized *)
    val make : string -> t
  end) : Dune_util.Stringlike with type t = S.t

val valid_format_doc : User_message.Style.t Pp.t
