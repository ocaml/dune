(** Represent the parsed but uninterpreted output of [ocamlc -config] or
    contents of [Makefile.config]. *)
module Vars : sig
  type t

  val find : t -> string -> string option
  val of_list_exn : (string * string) list -> t

  (** Parse the output of [ocamlc -config] given as a list of lines. *)
  val of_lines : string list -> (t, string) result
end
