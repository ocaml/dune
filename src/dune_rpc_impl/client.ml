module Stream = struct
  module In = Fiber.Stream.In

  module Out = struct
    type 'a t = 'a Fiber.Stream.Out.t

    let write w a = Fiber.Stream.Out.write w (Some a)

    let close w = Fiber.Stream.Out.write w None
  end

  let create () = Fiber.Stream.pipe ()
end

module Fiber = struct
  include Fiber

  let parallel_iter t ~f =
    let stream = Fiber.Stream.In.create t in
    Fiber.Stream.In.parallel_iter stream ~f
end

include Dune_rpc_private.Client.Make (Fiber) (Csexp_rpc.Session) (Stream)
