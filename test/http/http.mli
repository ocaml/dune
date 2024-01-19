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
  val respond : session -> status:[ `Ok | `Not_found ] -> content:string -> unit
  val respond_file : session -> file:string -> unit
end
