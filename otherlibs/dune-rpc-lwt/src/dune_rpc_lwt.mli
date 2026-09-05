module V1 : sig
  open Dune_rpc.V1

  module Client :
    Client.S
    with type 'a fiber := 'a Lwt.t
     and type chan := Lwt_io.input_channel * Lwt_io.output_channel

  module Where : Where.S with type 'a fiber := 'a Lwt.t

  val connect_chan
    :  Dune_rpc.V1.Where.t
    -> (Lwt_io.input_channel * Lwt_io.output_channel) Lwt.t

  module Action_plugin : sig
    module Glob = Dune_rpc.V1.Action_plugin.Glob

    type t

    module Error = Dune_rpc.V1.Action_plugin.Error

    val outside_of_dune : t
    val read_file : t -> path:string -> string Lwt.t
    val read_directory_with_glob : t -> path:string -> glob:Glob.t -> string list Lwt.t

    (** Run a dynamic action using Lwt for RPC communication. This function never
        returns. When run outside Dune, dependencies are assumed to be available. *)
    val run : (t -> unit Lwt.t) -> 'a
  end
end
