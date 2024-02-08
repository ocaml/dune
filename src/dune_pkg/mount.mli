open Import

(** Access the contents of OpamUrl.t without (necessarily) extracting it *)

type t

type backend =
  | Path of Path.t
  | Git of Rev_store.At_rev.t

val backend : t -> backend
val of_opam_url : Loc.t -> OpamUrl.t -> t Fiber.t
val stat : t -> Path.Local.t -> [ `Absent_or_unrecognized | `Dir | `File ] Fiber.t
val read : t -> Path.Local.t -> string option Fiber.t
val readdir : t -> Path.Local.t -> [ `File | `Dir ] Filename.Map.t Fiber.t
