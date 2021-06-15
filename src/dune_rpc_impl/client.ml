include
  Dune_rpc_private.Client.Make
    (struct
      include Fiber

      let parallel_iter t ~f =
        let stream = Fiber.Stream.In.create t in
        Fiber.Stream.In.parallel_iter stream ~f
    end)
    (Csexp_rpc.Session)
