open Stdune

(** Returns [true] if the filename has a supported archive extension
    (.tar, .tar.gz, .tgz, .tar.bz2, .tbz, .zip). *)
val is_supported : Filename.t -> bool

(* CR-soon Alizter: better type here. *)
(** [extract ~archive ~target] extracts the archive at [archive] into the
    directory at [target], creating the directory if it doesn't already exist.
    The archive format is determined from the file extension. If the extension
    is not recognized, defaults to tar. *)
val extract : archive:Path.t -> target:Path.t -> (unit, unit) result Fiber.t
