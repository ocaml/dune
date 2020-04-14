(** API for jbuild plugins *)

(* CR-someday amokhov: rename to [dune_plugin]. *)

module V1 : sig
  val context : string
  (** Current build context *)

  val ocaml_version : string
  (** OCaml version for the current buid context. It might not be the same as
      [Sys.ocaml_version] *)

  val ocamlc_config : (string * string) list
  (** Output of [ocamlc -config] for this context *)

  val send : string -> unit
  (** [send s] send [s] to Dune. [s] should be the contents of a [dune] file
      following the specification described in the manual. *)

  val run_and_read_lines : string -> string list
  (** Execute a command and read its output *)
end
