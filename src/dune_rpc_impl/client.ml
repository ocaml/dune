module Fiber = struct
  include Fiber

  let parallel_iter t ~f =
    let stream = Fiber.Stream.In.create t in
    Fiber.Stream.In.parallel_iter stream ~f
end

include Dune_rpc_private.Client.Make (Fiber) (Csexp_rpc.Session)
