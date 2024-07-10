(** Http helpers used for tests *)

module Server : sig
  (* Dummy http server that requires manual accepting of clients *)

  type t
  type session

  val make : Unix.sockaddr -> t
  val accept : t -> f:(session -> unit) -> unit
  val stop : t -> unit
  val port : t -> int
  val start : t -> unit

  (** Respond with a 404 error *)
  val respond_not_found : session -> unit

  (** Send the content of the file at path [file]. Works for both text
      and binary files. Raises an exception if the [file] doesn't
      refer to a file. *)
  val respond_file : session -> file:string -> unit
end
