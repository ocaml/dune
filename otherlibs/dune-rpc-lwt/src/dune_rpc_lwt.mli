module V1 : sig
  module Client :
    Dune_rpc.V1.S
      with type 'a fiber := 'a Lwt.t
       and type chan := Lwt_io.input_channel * Lwt_io.output_channel
end
