module File : sig
  type t =
    { path : string
    ; contents : string
    }
end

module Dune : sig
  type t

  val where : t -> Where.t
  val to_dyn : t -> Dyn.t
  val compare : t -> t -> Ordering.t
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

module Refresh : sig
  type t

  val added : t -> Dune.t list
  val removed : t -> Dune.t list
  val errored : t -> (string * exn) list
end

module Poll
    (Fiber : sig
       type 'a t

       val return : 'a -> 'a t
       val parallel_map : 'a list -> f:('a -> 'b t) -> 'b list t

       module O : sig
         val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
         val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
       end
     end)
    (_ : sig
       val scandir : string -> (string list, exn) result Fiber.t
       val stat : string -> ([ `Mtime of float ], exn) result Fiber.t
       val read_file : string -> (string, exn) result Fiber.t
     end) : sig
  val poll : t -> (Refresh.t, exn) result Fiber.t
end
