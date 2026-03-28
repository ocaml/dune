open Import

module Error : sig
  type t =
    | No_such_file
    | Broken_symlink
    | Cyclic_symlink
    | Unexpected_kind of File_kind.t
    | Unix_error of Unix_error.Detailed.t
    | Unrecognized of exn

  val no_such_file : t -> bool
  val pp : t -> Path.t -> _ Pp.t
  val to_dyn : t -> Dyn.t
  val of_exn : exn -> t
end

type t = (Digest.t, Error.t) result

val catch_fs_errors : (unit -> ('a, Error.t) result) -> ('a, Error.t) result
val equal : t -> t -> bool
val to_option : t -> Digest.t option
val to_dyn : t -> Dyn.t
