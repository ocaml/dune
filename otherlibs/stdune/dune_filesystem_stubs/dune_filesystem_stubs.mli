(** Efficient directory listing with kinds. *)

(** Auxiliary functions for working with Unix errors. *)
module Unix_error : sig
  type t = Unix.error

  val equal : t -> t -> bool

  (** A Unix error along with the corresponding system call and argument, as
      thrown in Unix exceptions. *)
  module Detailed : sig
    type nonrec t = t * string * string

    val raise : t -> 'a

    val create : Unix.error -> syscall:string -> arg:string -> t

    (** Apply a function to an argument, catching a detailed Unix error. *)
    val catch : ('a -> 'b) -> 'a -> ('b, t) result

    val equal : t -> t -> bool

    val to_string_hum : t -> string
  end
end

(** Auxiliary functions for working with Unix file kinds. *)
module File_kind : sig
  type t = Unix.file_kind =
    | S_REG
    | S_DIR
    | S_CHR
    | S_BLK
    | S_LNK
    | S_FIFO
    | S_SOCK

  val to_string : t -> string

  val to_string_hum : t -> string

  val equal : t -> t -> bool
end

(** [read_directory_with_kinds] is similar to [Sys.readdir], while additionally
    returning kinds of the filesystem entries. *)
val read_directory_with_kinds :
  string -> ((string * File_kind.t) list, Unix_error.Detailed.t) Result.t

(** [read_directory_with_kinds d] returns all the filesystem entries in [d]
    except for "." and "..", similar to [Sys.readdir]. *)
val read_directory : string -> (string list, Unix_error.Detailed.t) Result.t
