open Stdune

type t = OpamUrl.t

val equal : t -> t -> bool
val hash : t -> int
val to_string : t -> string
val to_dyn : t -> Dyn.t
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

module Map : Map.S with type key = t
module Set : Set.S with type elt = t and type 'a map = 'a Map.t

val remote : t -> loc:Loc.t -> Rev_store.t -> Rev_store.Remote.t

type resolve =
  | Resolved of Rev_store.Object.resolved
  | Unresolved of Rev_store.Object.t

val resolve : t -> loc:Loc.t -> Rev_store.t -> (resolve, User_message.t) result Fiber.t

val fetch_revision
  :  t
  -> loc:Loc.t
  -> resolve
  -> Rev_store.t
  -> (Rev_store.At_rev.t, User_message.t) result Fiber.t

val set_rev : t -> Rev_store.Object.t -> t
