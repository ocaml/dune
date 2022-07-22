open Import

type t =
  [ `Unix of string
  | `Ip of [ `Host of string ] * [ `Port of int ]
  ]

val to_string : t -> string

val compare : t -> t -> Ordering.t

val to_dyn : t -> Dyn.t

val sexp : t Conv.value

val add_to_env : t -> Env.t -> Env.t

module type S = sig
  type 'a fiber

  val get :
       env:(string -> string option)
    -> build_dir:string
    -> (t option, exn) result fiber

  val default : ?win32:bool -> build_dir:string -> unit -> t
end

type error = Invalid_where of string

exception E of error

module Make (Fiber : sig
  type 'a t

  val return : 'a -> 'a t

  module O : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  end
end) (_ : sig
  val read_file : string -> (string, exn) result Fiber.t

  val analyze_path :
    string -> ([ `Unix_socket | `Normal_file | `Other ], exn) result Fiber.t
end) : S with type 'a fiber := 'a Fiber.t
