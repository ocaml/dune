(** Management of syntaxes *)

open! Stdune

module Version : sig
  (** A syntax version.

      It is always assumed that a parser with version [(X, Y)] can read the
      output produced by a printer at version [(X, Z)] for any [Z <= Y]. *)
  type t = int * int

  include Conv.S with type t := t

  val pp : t Fmt.t

  val to_dyn : t Dyn.Encoder.t

  val hash : t -> int

  val equal : t -> t -> bool

  val to_string : t -> string

  (** Whether the parser can read the data or not *)
  val can_read : parser_version:t -> data_version:t -> bool

  val compare : t -> t -> Ordering.t

  module Infix : Comparator.OPS with type t = t
end

type t

module Error : sig
  val since : Loc.t -> t -> Version.t -> what:string -> _

  val renamed_in : Loc.t -> t -> Version.t -> what:string -> to_:string -> _

  val deleted_in :
       Loc.t
    -> t
    -> ?repl:User_message.Style.t Pp.t list
    -> Version.t
    -> what:string
    -> _
end

module Warning : sig
  val deprecated_in :
       Loc.t
    -> t
    -> ?repl:User_message.Style.t Pp.t list
    -> Version.t
    -> what:string
    -> unit
end

(** [create ~name ~desc supported_versions] defines a new syntax.
    [supported_version] is the list of the last minor version of each supported
    major version. [desc] is used to describe what this syntax represent in
    error messages. *)
val create : name:string -> desc:string -> Version.t list -> t

(** Return the name of the syntax. *)
val name : t -> string

(** Check that the given version is supported and raise otherwise. *)
val check_supported : t -> Loc.t * Version.t -> unit

val greatest_supported_version : t -> Version.t

(** {1 S-expression parsing} *)

(** {2 High-level functions} *)

(** Indicate the field/constructor being parsed was deleted in the given
    version *)
val deleted_in : t -> Version.t -> (unit, _) Decoder.parser

(** Indicate the field/constructor being parsed was deprecated in the given
    version *)
val deprecated_in : t -> Version.t -> (unit, _) Decoder.parser

(** Indicate the field/constructor being parsed was renamed in the given
    version *)
val renamed_in : t -> Version.t -> to_:string -> (unit, _) Decoder.parser

(** Indicate the field/constructor being parsed was introduced in the given
    version. When [fatal] is false, simply emit a warning instead of error.
    [fatal] defaults to true. *)
val since :
     ?desc:(unit -> (Stdune.Loc.t * string, 'a) Decoder.parser)
  -> ?fatal:bool
  -> t
  -> Version.t
  -> (unit, 'a) Decoder.parser

(** {2 Low-level functions} *)

val set : t -> Version.t -> ('a, 'k) Decoder.parser -> ('a, 'k) Decoder.parser

val get_exn : t -> (Version.t, 'k) Decoder.parser

val key : t -> Version.t Univ_map.Key.t
