open Stdune

module Where =
  Dune_rpc_private.Where.Make
    (struct
      type 'a t = 'a

      let return a = a

      module O = struct
        let ( let+ ) x f = f x

        let ( let* ) x f = f x
      end
    end)
    (struct
      let getenv = Env.get Env.initial

      let is_win32 () = Sys.win32

      let read_file f = Io.String_path.read_file f

      let readlink s =
        match Unix.readlink s with
        | s -> Some s
        | exception Unix.Unix_error (Unix.EINVAL, _, _) -> None

      let analyze_path s =
        match (Unix.stat s).st_kind with
        | Unix.S_SOCK -> `Unix_socket
        | S_REG -> `Normal_file
        | _
        | (exception Unix.Unix_error (Unix.ENOENT, _, _)) ->
          `Other
    end)

let root =
  lazy
    (Path.reach
       (Path.build Path.Build.root)
       ~from:(Path.external_ Path.External.initial_cwd))

let get () = Where.get ~build_dir:(Lazy.force root)

let default () = Where.default ~build_dir:(Lazy.force root)

let to_socket = function
  | `Unix p -> Unix.ADDR_UNIX p
  | `Ip (`Host host, `Port port) ->
    Unix.ADDR_INET (Unix.inet_addr_of_string host, port)
