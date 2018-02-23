(** Versioned syntaxes *)

module Version : sig
  (** A syntax version.

      It is always assumed that a parser with version [(X, Y)] can
      read the output produced by a printer at version [(X, Z)] for any
      [Z <= Y]. *)
  type t = int * int

  val sexp_of_t : t Sexp.To_sexp.t
  val t_of_sexp : t Sexp.Of_sexp.t

  (** Whether the parser can read the data or not *)
  val can_read : parser_version:t -> data_version:t -> bool
end

module Versioned_parser : sig
  (** Versioned parser *)
  type 'a t

  (** Create a versionned parser. There must be exactly one parser per
      major version. *)
  val make : (Version.t * 'a) list -> 'a t

  val last : 'a t -> Version.t * 'a

  (** Find a parser that can parse data of this version *)
  val find_exn : 'a t -> loc:Loc.t -> data_version:Version.t -> 'a
end
