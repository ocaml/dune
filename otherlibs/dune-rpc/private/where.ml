open Import

type t =
  [ `Unix of string
  | `Ip of [ `Host of string ] * [ `Port of int ]
  ]

let default_port = 8587

let compare = Poly.compare

let of_dbus { Dbus_address.name; args } =
  match name with
  | "unix" ->
    let path = Option.value_exn (List.assoc args "path") in
    Ok (`Unix path)
  | "tcp" ->
    let port =
      match List.assoc args "port" with
      | None -> default_port
      | Some p -> int_of_string p
    in
    let addr = List.assoc args "host" |> Option.value_exn in
    Ok (`Ip (`Host addr, `Port port))
  | _ -> Error "invalid connection type"

let of_string s : (t, string) result =
  match Dbus_address.of_string s with
  | Error _ -> Error ("invalid address format " ^ s)
  | Ok s -> of_dbus s

type error = Invalid_where of string

exception E of error

let rpc_socket_relative_to_build_dir = "rpc/dune"

let _DUNE_RPC = "DUNE_RPC"

let to_dbus : t -> Dbus_address.t = function
  | `Unix p -> { name = "unix"; args = [ ("path", p) ] }
  | `Ip (`Host host, `Port port) ->
    let port = string_of_int port in
    { name = "tcp"; args = [ ("host", host); ("port", port) ] }

let to_dyn : t -> Dyn.t =
  let open Dyn.Encoder in
  function
  | `Unix s -> constr "Unix" [ string s ]
  | `Ip (`Host host, `Port port) ->
    constr "Ip" [ constr "Host" [ string host ]; constr "Port" [ int port ] ]

let to_string t = Dbus_address.to_string (to_dbus t)

let sexp : t Conv.value =
  let open Conv in
  (* TODO of_string should raise the right error *)
  iso Conv.string
    (fun s ->
      match of_string s with
      | Error e -> failwith e
      | Ok s -> s)
    to_string

let add_to_env t env =
  let value = to_string t in
  Env.add env ~var:_DUNE_RPC ~value

module type S = sig
  type 'a fiber

  val get :
       env:(string * string) list
    -> build_dir:string
    -> (t option, exn) result fiber

  val default : ?is_win32:bool -> build_dir:string -> unit -> t
end

let is_win32 = Sys.win32

module Make (Fiber : sig
  type 'a t

  val return : 'a -> 'a t

  module O : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  end
end) (IO : sig
  val read_file : string -> (string, exn) result Fiber.t

  val readlink : string -> (string option, exn) result Fiber.t

  val analyze_path :
    string -> ([ `Unix_socket | `Normal_file | `Other ], exn) result Fiber.t
end) : S with type 'a fiber := 'a Fiber.t = struct
  let default ?(is_win32 = is_win32) ~build_dir () =
    if is_win32 then
      `Ip (`Host "0.0.0.0", `Port default_port)
    else
      `Unix (Filename.concat build_dir rpc_socket_relative_to_build_dir)

  let ( let** ) x f =
    let open Fiber.O in
    let* x = x in
    match x with
    | Error e -> Fiber.return (Error e)
    | Ok x -> f x

  let get ~env ~build_dir : (t option, exn) result Fiber.t =
    let open Fiber.O in
    match List.assoc_opt _DUNE_RPC env with
    | Some d -> (
      match of_string d with
      | Ok s -> Fiber.return (Ok (Some s))
      | Error e -> Fiber.return (Error (E (Invalid_where e))))
    | None -> (
      let of_file f =
        let+ contents = IO.read_file f in
        match contents with
        | Error e -> Error e
        | Ok contents -> (
          match of_string contents with
          | Error e -> Error (E (Invalid_where e))
          | Ok s -> Ok (Some s))
      in
      let file = Filename.concat build_dir rpc_socket_relative_to_build_dir in
      let** analyze = IO.analyze_path file in
      match analyze with
      | `Other -> Fiber.return (Ok None)
      | `Normal_file -> of_file file
      | `Unix_socket -> (
        let unix file = Fiber.return (Ok (Some (`Unix file))) in
        if String.length file < 104 then
          unix file
        else
          let** readlink = IO.readlink file in
          match readlink with
          | None -> unix file
          | Some p ->
            let shorter s1 s2 =
              if String.length s1 > String.length s2 then
                s2
              else
                s1
            in
            unix
              (shorter file
                 (if Filename.is_relative p then
                   Filename.concat (Filename.dirname file) p
                 else
                   p))))
end
