module Decl = Decl
module Client = Client
module Server = Server
module For_handlers = For_handlers
module Private = Dune_rpc_client.Private
module Watch_mode_config = Watch_mode_config
module Where = Dune_rpc_client.Where

module Diagnostics = struct
  module For_tests = struct
    let diagnostic_of_error = Diagnostics.diagnostic_of_error
  end
end

module Poll_active =
  Dune_rpc_private.Registry.Poll
    (Fiber)
    (struct
      let scandir dir =
        Fiber.return
          (match Dune_filesystem_stubs.read_directory dir with
           | Ok s -> Ok s
           | Error (e, _, _) -> Error (Failure (dir ^ ": " ^ Unix.error_message e)))
      ;;

      let stat s =
        Fiber.return
          (match Unix.stat s with
           | exception exn -> Error exn
           | s -> Ok (`Mtime s.st_mtime))
      ;;

      let read_file s =
        Fiber.return
          (match Stdune.Io.String_path.read_file s with
           | s -> Ok s
           | exception exn -> Error exn)
      ;;
    end)
