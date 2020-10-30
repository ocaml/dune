module File_kind : sig
  type t = Unix.file_kind =
    | S_REG
    | S_DIR
    | S_CHR
    | S_BLK
    | S_LNK
    | S_FIFO
    | S_SOCK
end

(** [read_directory_with_kinds] is similar to [Sys.readdir], while additionally
    returning kinds of the filesystem entries. *)
val read_directory_with_kinds :
  string -> ((string * File_kind.t) list, Unix.error) Result.t

(** [read_directory_with_kinds d] returns all the filesystem entries in [d]
    except for "." and "..", similar to [Sys.readdir]. *)
val read_directory : string -> (string list, Unix.error) Result.t
