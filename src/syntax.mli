(** Management of syntaxes *)

open Stdune

module Version : sig
  (** A syntax version.

      It is always assumed that a parser with version [(X, Y)] can
      read the output produced by a printer at version [(X, Z)] for any
      [Z <= Y]. *)
  type t = int * int

  include Sexp.Sexpable with type t := t

  val to_string : t -> string

  (** Whether the parser can read the data or not *)
  val can_read : parser_version:t -> data_version:t -> bool
end

type t

(** [create ~name supported_versions] defines a new
    syntax. [supported_version] is the list of the last minor version
    of each supported major version. *)
val create : name:string -> Version.t list -> t

(** Return the name of the syntax. *)
val name : t -> string

(** Check that the given version is supported and raise otherwise. *)
val check_supported : t -> Loc.t * Version.t -> unit

val greatest_supported_version : t -> Version.t

val set
  :  t
  -> Version.t
  -> ('a, 'k) Sexp.Of_sexp.parser
  -> ('a, 'k) Sexp.Of_sexp.parser

val get_exn : t -> (Version.t, 'k) Sexp.Of_sexp.parser

val key : t -> Version.t Univ_map.Key.t
