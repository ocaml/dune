open Stdune

type t = OpamUrl.t

val equal : t -> t -> bool
val hash : t -> int
val to_string : t -> string
val of_string : string -> t
val decode_loc : (Stdune.Loc.t * t) Dune_sexp.Decoder.t
val rev : t -> string option
val base_url : t -> string
val is_version_control : t -> bool

(** [is_file t] is true iff [t] is a url beginning with "file://" *)
val is_local : t -> bool

(* [local_or_git_only t loc] returns [`Path p] for a URL pointing to a local
   file system or [`Git] if it's a git repository (remote or otherwise). If
   it's neither of those cases, it will error out. *)
val local_or_git_only : t -> Loc.t -> [ `Path of Path.t | `Git ]
