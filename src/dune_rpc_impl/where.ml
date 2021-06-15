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

      let read_file s = Io.String_path.read_file s

      let readlink s = Unix.readlink s

      let analyze_path s =
        if Sys.file_exists s then
          let stat = Unix.lstat s in
          match stat.st_kind with
          | Unix.S_SOCK -> `Unix_socket
          | Unix.S_LNK -> `Symlink
          | _ -> `Normal_file
        else
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
