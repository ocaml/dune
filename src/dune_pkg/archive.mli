open Stdune

(** Returns [true] if the filename has a supported archive extension
    (.tar, .tar.gz, .tgz, .tar.bz2, .tbz, .zip). *)
val is_supported : Filename.t -> bool

module Error : sig
  type t = private
    | No_extractor of
        { ext : string
        ; tried : string list
        }
    | Command_failed of
        { bin : Path.t
        ; archive : Path.t
        ; exit_code : int
        ; stderr : string list
        }
    | Read_dir_failed of
        { archive : Path.t
        ; error : Unix_error.Detailed.t
        }

  val message : t -> User_message.Style.t Pp.t list
  val raise : t -> 'a
end

(** [extract ~archive ~target] extracts the archive at [archive] into the
    directory at [target], creating the directory if it doesn't already exist.
    The archive format is determined from the file extension. If the extension
    is not recognized, defaults to tar. *)
val extract : archive:Path.t -> target:Path.t -> (unit, Error.t) result Fiber.t

(** Same as [extract] but raises [User_error] on failure. *)
val extract_exn : archive:Path.t -> target:Path.t -> unit Fiber.t
