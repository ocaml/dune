module Protocol = Protocol
module Path = Path

type 'a t

(* TODO jstaron: Add documentation. *)

val return : 'a -> 'a t

val map : 'a t -> f:('a -> 'b) -> 'b t

val both : 'a t -> 'b t -> ('a * 'b) t

val stage : 'a t -> f:('a -> 'b t) -> 'b t

val read_file : path:Path.t -> (string, string) Stdune.Result.t t

val write_file : path:Path.t -> data:string -> (unit, string) Stdune.Result.t t

(* TODO jstaron: Right now, if program tries to read directory that contain no
  files, directory is not copied by dune so we get an error. *)
val read_directory : path:Path.t -> (string list, string) Stdune.Result.t t

val run : unit t -> unit
