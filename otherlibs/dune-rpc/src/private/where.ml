open Import

type t =
  [ `Unix of string
  | `Ip of [ `Host of string ] * [ `Port of int ]
  ]

let default_port = 8587

let compare = Poly.compare

let ( let* ) x f =
  match x with
  | Ok s -> f s
  | Error _ as e -> e

type error = Invalid_where of string

exception E of error

let () =
  Printexc.register_printer (function
    | E (Invalid_where w) -> Some (Printf.sprintf "Invalid RPC address: %s" w)
    | _ -> None)

let of_dbus { Dbus_address.name; args } =
  match name with
  | "unix" -> (
    match List.assoc args "path" with
    | None -> Error "missing path field"
    | Some path -> Ok (`Unix path))
  | "tcp" ->
    let* port =
      match List.assoc args "port" with
      | None -> Ok default_port
      | Some p -> (
        match int_of_string p with
        | exception Failure _ -> Error "invalid port"
        | s -> Ok s)
    in
    let* addr =
      match List.assoc args "host" with
      | None -> Error "missing host field"
      | Some host -> Ok host
    in
    Ok (`Ip (`Host addr, `Port port))
  | _ -> Error "invalid connection type"

let of_string s : (t, exn) result =
  match Dbus_address.of_string s with
  | Error _ -> Error (E (Invalid_where ("invalid address format " ^ s)))
  | Ok s -> (
    match of_dbus s with
    | Ok s -> Ok s
    | Error e -> Error (E (Invalid_where e)))

let rpc_socket_relative_to_build_dir = ".rpc/dune"

let _DUNE_RPC = "DUNE_RPC"

let to_dbus : t -> Dbus_address.t = function
  | `Unix p -> { name = "unix"; args = [ ("path", p) ] }
  | `Ip (`Host host, `Port port) ->
    let port = string_of_int port in
    { name = "tcp"; args = [ ("host", host); ("port", port) ] }

let to_dyn : t -> Dyn.t =
  let open Dyn in
  function
  | `Unix s -> variant "Unix" [ string s ]
  | `Ip (`Host host, `Port port) ->
    variant "Ip" [ variant "Host" [ string host ]; variant "Port" [ int port ] ]

let to_string t = Dbus_address.to_string (to_dbus t)

let sexp : t Conv.value =
  let open Conv in
  iso_result Conv.string of_string to_string

let add_to_env t env =
  let value = to_string t in
  Env.add env ~var:_DUNE_RPC ~value

module type S = sig
  type 'a fiber

  val get :
       env:(string -> string option)
    -> build_dir:string
    -> (t option, exn) result fiber

  val default : ?win32:bool -> build_dir:string -> unit -> t
end

let win32 = Sys.win32

module Make (Fiber : sig
  type 'a t

  val return : 'a -> 'a t

  module O : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  end
end) (IO : sig
  val read_file : string -> (string, exn) result Fiber.t

  val analyze_path :
    string -> ([ `Unix_socket | `Normal_file | `Other ], exn) result Fiber.t
end) : S with type 'a fiber := 'a Fiber.t = struct
  let default ?(win32 = win32) ~build_dir () =
    if win32 then `Ip (`Host "0.0.0.0", `Port default_port)
    else `Unix (Filename.concat build_dir rpc_socket_relative_to_build_dir)

  let ( let** ) x f =
    let open Fiber.O in
    let* x = x in
    match x with
    | Error e -> Fiber.return (Error e)
    | Ok x -> f x

  let get ~env ~build_dir : (t option, exn) result Fiber.t =
    let open Fiber.O in
    let* () = Fiber.return () in
    match env _DUNE_RPC with
    | Some d ->
      Fiber.return
        (match of_string d with
        | Ok s -> Ok (Some s)
        | Error exn -> Error exn)
    | None -> (
      let of_file f =
        let+ contents = IO.read_file f in
        match contents with
        | Error e -> Error e
        | Ok contents -> (
          match of_string contents with
          | Error e -> Error e
          | Ok s -> Ok (Some s))
      in
      let file = Filename.concat build_dir rpc_socket_relative_to_build_dir in
      let** analyze = IO.analyze_path file in
      match analyze with
      | `Other -> Fiber.return (Ok None)
      | `Normal_file -> of_file file
      | `Unix_socket -> Fiber.return (Ok (Some (`Unix file))))
end
