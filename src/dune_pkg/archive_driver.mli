open Stdune

(** Knows how to extract archives of a particular format *)
type t

(** Driver for tarballs, possibly compressed *)
val tar : t

(** Driver for zip files *)
val zip : t

(** Return the driver that can extract a file of a given name. The decision is
    made based on the file's suffix. *)
val choose_for_filename : string -> t option

(** Extract an archive into a given target directory *)
val extract : t -> archive:Path.t -> target:Path.t -> (unit, unit) result Fiber.t
