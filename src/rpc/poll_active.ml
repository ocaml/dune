open Stdune

include
  Dune_rpc.Private.Registry.Poll
    (Fiber)
    (struct
      let scandir dir =
        Fiber.return
          (match Stdune.Readdir.read_directory dir with
           | Ok s -> Ok s
           | Error (e, _, _) -> Error (Failure (dir ^ ": " ^ Unix.error_message e)))
      ;;

      let stat s =
        Fiber.return
          (match Stdune.Stat.stat s with
           | exception exn -> Error exn
           | stat -> Ok (`Mtime (Time.to_secs stat.mtime)))
      ;;

      let read_file s =
        Fiber.return
          (match Stdune.Io.String_path.read_file s with
           | s -> Ok s
           | exception exn -> Error exn)
      ;;
    end)
