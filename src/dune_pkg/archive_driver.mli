open Stdune

(** Knows how to extract archives of a particular format *)
type t

(** Driver for tarballs, possibly compressed *)
val tar : t

(** Returns the driver that can extract a file of a given name. The decision is
    made based on the file's suffix. *)
val choose_for_filename : Filename.t -> t option

(** Returns the driver that can extract a file of a given name. The decision is
    made based on the file's suffix. If the archive format isn't clear from the
    filename then this function will default to using tar as tar archives
    (possibly compressed) are by far the most common archive format used by
    opam packages. *)
val choose_for_filename_default_to_tar : Filename.t -> t

(** [extract t ~archive ~target] uses the archive driver [t] to extract the
    archive at [archive] into the directory at [target], creating the directory
    if it doesn't already exist. *)
val extract : t -> archive:Path.t -> target:Path.t -> (unit, unit) result Fiber.t
