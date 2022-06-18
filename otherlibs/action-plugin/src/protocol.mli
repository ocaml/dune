open Import
open Sexpable_intf

module Error : module type of Error

module Dependency : sig
  type t =
    | File of string
    | Directory of string
    | Glob of
        { path : string
        ; glob : string
        }

  module Map : Map.S with type key = t

  module Set : sig
    include Set.S with type elt = t and type 'a map = 'a Map.t
  end
end

module Greeting : sig
  type t =
    { run_arguments_fn : string
    ; response_fn : string
    }

  include Sexpable with type t := t
end

module Run_arguments : sig
  type t =
    { prepared_dependencies : Dependency.Set.t
    ; targets : String.Set.t
    }

  include Sexpable with type t := t
end

module Response : sig
  type t =
    | Done
    | Need_more_deps of Dependency.Set.t

  include Sexpable with type t := t
end

(** Dune sets this environment variable to pass [Greeting.t] to client. *)
val run_by_dune_env_variable : string

module Context : sig
  type t

  type create_result =
    | Ok of t
    | Run_outside_of_dune
    | Error of string

  val create : unit -> create_result

  val prepared_dependencies : t -> Dependency.Set.t

  val targets : t -> String.Set.t

  val respond : t -> Response.t -> unit
end
