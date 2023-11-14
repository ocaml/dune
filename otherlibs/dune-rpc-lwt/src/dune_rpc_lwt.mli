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
end
