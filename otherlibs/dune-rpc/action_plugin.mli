module Glob = Glob

type action_id = Action_id.t

module Make
    (Fiber : Fiber_intf.S)
    (Chan : sig
       type t
     end)
    (_ : Client.Public with type 'a fiber := 'a Fiber.t and type chan := Chan.t) : sig
  type t

  val run : Chan.t -> action_id:action_id -> f:(t -> unit Fiber.t) -> unit Fiber.t
  val outside_of_dune : t
  val read_file : t -> path:string -> string Fiber.t
  val read_directory_with_glob : t -> path:string -> glob:Glob.t -> string list Fiber.t
end

module Rpc : sig
  val action_id_env_variable : Stdune.Env.Var.t
  val initialize : (Action_id.t, unit) Types.Decl.Request.t

  val build_deps
    : (Procedures.Public.Action_plugin.Build_deps.t, string option) Types.Decl.Request.t
end

type run_context =
  | Outside_of_dune
  | Under_dune of
      { action_id : action_id
      ; where : Where.t
      }

val run_context : unit -> run_context

module Error : sig
  exception E of string
end
