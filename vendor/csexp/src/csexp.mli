(** Canonical S-expressions *)

(** This module provides minimal support for reading and writing S-expressions
    in canonical form.

    https://en.wikipedia.org/wiki/Canonical_S-expressions

    Note that because the canonical representation of S-expressions is so
    simple, this module doesn't go out of his way to provide a fully generic
    parser and printer and instead just provides a few simple functions. If you
    are using fancy input sources, simply copy the parser and adapt it. The
    format is so simple that it's pretty difficult to get it wrong by accident.

    To avoid a dependency on a particular S-expression library, the only module
    of this library is parameterised by the type of S-expressions.

    {[
      let rec print = function
        | Atom str -> Printf.printf "%d:%s" (String.length s)
        | List l -> List.iter print l
    ]} *)

module type Sexp = sig
  type t =
    | Atom of string
    | List of t list
end

module Make (Sexp : Sexp) : sig
  (** {2 Parsing} *)
  include module type of Result

  (** [parse_string s] parses a single S-expression encoded in canonical form in
      [s]. It is an error for [s] to contain a S-expression followed by more
      data. In case of error, the offset of the error as well as an error
      message is returned. *)
  val parse_string : string -> (Sexp.t, int * string) result

  (** [parse_string s] parses a sequence of S-expressions encoded in canonical
      form in [s] *)
  val parse_string_many : string -> (Sexp.t list, int * string) result

  (** Read exactly one canonical S-expressions from the given channel. Note that
      this function never raises [End_of_file]. Instead, it returns [Error]. *)
  val input : in_channel -> (Sexp.t, string) result

  (** Same as [input] but returns [Ok None] if the end of file has already been
      reached. If some more characters are available but the end of file is
      reached before reading a complete S-expression, this function returns
      [Error]. *)
  val input_opt : in_channel -> (Sexp.t option, string) result

  (** Read many S-expressions until the end of input is reached. *)
  val input_many : in_channel -> (Sexp.t list, string) result

  (** {2 Serialising} *)

  (** The length of the serialised representation of a S-expression *)
  val serialised_length : Sexp.t -> int

  (** [to_string sexp] converts S-expression [sexp] to a string in canonical
      form. *)
  val to_string : Sexp.t -> string

  (** [to_buffer buf sexp] outputs the S-expression [sexp] converted to its
      canonical form to buffer [buf]. *)
  val to_buffer : Buffer.t -> Sexp.t -> unit

  (** [output oc sexp] outputs the S-expression [sexp] converted to its
      canonical form to channel [oc]. *)
  val to_channel : out_channel -> Sexp.t -> unit

  (** {3 Low level parser}

      Low level parsing interface with fine-grain control over the input monad.
      Suitable for Lwt or Async integration. *)

  module type Input = sig
    (** Type of an input source. *)
    type t

    (** Monad wrapping values returned by the input source *)
    module Monad : sig
      type 'a t

      val return : 'a -> 'a t

      val bind : 'a t -> ('a -> 'b t) -> 'b t
    end

    (** [read_string source size] reads exactly [size] bytes from [source] and
        return them as a string. Reaching the end of the input before [size]
        bytes have been read is an [Error]. *)
    val read_string : t -> int -> (string, string) result Monad.t

    (** [read_char source] is [read_string source 1], except the result is
        returned as a single character.*)
    val read_char : t -> (char, string) result Monad.t
  end

  module Make_parser (Input : Input) : sig
    (** Read exactly one canonical S-expressions from the input. Note that this
        function never raises [End_of_file]. Instead, it returns [Error]. *)
    val parse : Input.t -> (Sexp.t, string) result Input.Monad.t

    (** Read many S-expressions until the end of input is reached. *)
    val parse_many : Input.t -> (Sexp.t list, string) result Input.Monad.t
  end
end
