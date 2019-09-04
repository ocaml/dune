module Protocol = Protocol
module Path = Path

(* TODO jstaron: Add documentation. *)

module Execution_error : sig
  exception E of string
end

type 'a t

val return : 'a -> 'a t

val map : 'a t -> f:('a -> 'b) -> 'b t

val both : 'a t -> 'b t -> ('a * 'b) t

val stage : 'a t -> f:('a -> 'b t) -> 'b t

val read_file : path:Path.t -> (string, string) Result.t t

val write_file : path:Path.t -> data:string -> (unit, string) Result.t t

(* TODO jstaron: If program tries to read empty directory, dune does not copy
  it to `_build` so we get a "No such file or directory" error. *)
val read_directory : path:Path.t -> (string list, string) Result.t t

val run : unit t -> unit
