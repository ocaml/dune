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
      let read_file f = Ok (Io.String_path.read_file f)

      let analyze_path s =
        match (Unix.stat s).st_kind with
        | Unix.S_SOCK -> Ok `Unix_socket
        | S_REG -> Ok `Normal_file
        | _ | (exception Unix.Unix_error (Unix.ENOENT, _, _)) -> Ok `Other
        | exception (Unix.Unix_error _ as e) -> Error e
    end)

let build_dir =
  lazy
    (let build_dir = Path.Build.to_string Path.Build.root in
     match String.drop_prefix build_dir ~prefix:(Sys.getcwd () ^ "/") with
     | None -> build_dir
     | Some s -> Filename.concat "." s)

let get () =
  let env = Env.get Env.initial in
  match Where.get ~env ~build_dir:(Lazy.force build_dir) with
  | Ok s -> s
  | Error exn ->
    User_error.raise [ Pp.text "Unable to find dune rpc address"; Exn.pp exn ]

let default () = Where.default ~build_dir:(Lazy.force build_dir) ()

let to_socket = function
  | `Unix p -> Unix.ADDR_UNIX p
  | `Ip (`Host host, `Port port) ->
    Unix.ADDR_INET (Unix.inet_addr_of_string host, port)

let to_string = function
  | `Unix p -> sprintf "unix://%s" p
  | `Ip (`Host host, `Port port) -> sprintf "%s:%d" host port
