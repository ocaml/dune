(** Functions on paths that are represented as strings *)

(** Represent the result of [mkdir_p] *)
type mkdir_p =
  | Already_exists  (** The path already exists. No action was taken *)
  | Created  (** The directory was created. *)

val mkdir_p : ?perms:int -> string -> mkdir_p
