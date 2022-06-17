open Import
include Dune_rpc.Client.Make (Private.Fiber) (Csexp_rpc.Session)

module Connect = struct
  let csexp_client p = Csexp_rpc.Client.create (Where.to_socket p)
end

let client ?handler p init ~f =
  let open Fiber.O in
  let* c = Connect.csexp_client p in
  let* session = Csexp_rpc.Client.connect_exn c in
  connect_with_menu ?handler
    ~private_menu:[ Request Decl.build; Request Decl.status ]
    session init ~f

let client_with_session ?handler init ~session ~f =
  connect_with_menu ?handler
    ~private_menu:[ Request Decl.build; Request Decl.status ]
    session init ~f
