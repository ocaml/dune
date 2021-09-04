module File : sig
  type t =
    { path : string
    ; contents : string
    }
end

module Dune : sig
  type t

  val where : t -> Where.t

  val root : t -> string

  val pid : t -> int

  val create : where:Where.t -> root:string -> pid:int -> t

  type error =
    | Of_sexp of Conv.error
    | Csexp of
        { position : int
        ; message : string
        }

  val of_file : File.t -> (t, error) result
end

module Config : sig
  type t

  val create : Xdg.t -> t

  val register : t -> Dune.t -> [ `Caller_should_write of File.t ]

  val watch_dir : t -> string
end

type t

val create : Config.t -> t

val current : t -> Dune.t list

type refresh =
  { added : Dune.t list
  ; removed : Dune.t list
  ; errored : (string * exn) list
  }

module Poll (Fiber : sig
  type 'a t

  val return : 'a -> 'a t

  val parallel_map : 'a list -> f:('a -> 'b t) -> 'b list t

  module O : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  end
end) (IO : sig
  val scandir : string -> (string list, exn) result Fiber.t

  val stat : string -> ([ `Mtime of float ], exn) result Fiber.t

  val read_file : string -> (string, exn) result Fiber.t
end) : sig
  val poll : t -> (refresh, exn) result Fiber.t
end
