(** Functions on paths that are represented as strings *)

(** Represent the result of [mkdir_p] *)
type mkdir_p =
  | Already_exists  (** The path already exists. No action was taken *)
  | Created  (** The directory was created. *)

val mkdir_p : ?perms:int -> string -> mkdir_p

type follow_symlink_error =
  | Not_a_symlink
  | Max_depth_exceeded
  | Unix_error of Unix.error

val follow_symlink : string -> (string, follow_symlink_error) result
