(** Implementation of versioned files *)

module First_line = Versioned_file_first_line

module type S = Versioned_file_intf.S

module Make (Data : sig
    type t
  end) : S with type data := Data.t

(** Raise with an informative message when seeing a (lang ...) field. *)
val no_more_lang : unit Decoder.fields_parser
