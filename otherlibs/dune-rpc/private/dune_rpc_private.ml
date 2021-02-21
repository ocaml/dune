open Import
module Conv = Conv
include Types

module Where = struct
  type t =
    [ `Unix of Path.t
    | `Ip of Unix.inet_addr * [ `Port of int ]
    ]

  let default_port = 8587

  let of_dbus { Dbus_address.name; args } =
    match name with
    | "unix" ->
      let path = Path.of_string (Option.value_exn (List.assoc args "path")) in
      `Unix path
    | "tcp" ->
      let port =
        match List.assoc args "port" with
        | None -> default_port
        | Some p -> int_of_string p
      in
      let addr =
        List.assoc args "host" |> Option.value_exn |> Unix.inet_addr_of_string
      in
      `Ip (addr, `Port port)
    | _ -> failwith "invalid connection type"

  let of_string s =
    match Dbus_address.of_string s with
    | Error _ -> failwith ("invalid address format " ^ s)
    | Ok s -> of_dbus s

  let rpc_dir = lazy Path.Build.(relative root "rpc")

  let fname = "conn"

  let _DUNE_RPC = "DUNE_RPC"

  let default : unit -> t =
    let s =
      lazy
        ( if Sys.win32 then
          `Ip (Unix.inet_addr_of_string "0.0.0.0", `Port default_port)
        else
          `Unix (Path.build (Path.Build.relative (Lazy.force rpc_dir) fname)) )
    in
    fun () -> Lazy.force s

  let get () : t option =
    match Sys.getenv_opt _DUNE_RPC with
    | Some d -> Some (of_string d)
    | None -> (
      let path = Path.build (Path.Build.relative (Lazy.force rpc_dir) fname) in
      match Path.exists path with
      | false -> None
      | true ->
        let stat = Unix.stat (Path.to_string path) in
        Some
          ( match stat.st_kind with
          | Unix.S_SOCK -> `Unix path
          | _ -> of_string (Io.read_file path) ) )

  let to_dbus : t -> Dbus_address.t = function
    | `Unix p -> { name = "unix"; args = [ ("path", Path.to_string p) ] }
    | `Ip (host, `Port port) ->
      let port = string_of_int port in
      let host = Unix.string_of_inet_addr host in
      { name = "tcp"; args = [ ("host", host); ("port", port) ] }

  let to_string t = Dbus_address.to_string (to_dbus t)

  let add_to_env t env =
    let value = to_string t in
    Env.add env ~var:_DUNE_RPC ~value
end

module Loc = struct
  include Loc

  let pos_sexp =
    let open Conv in
    let to_ (pos_fname, pos_lnum, pos_bol, pos_cnum) =
      { Lexing.pos_fname; pos_lnum; pos_bol; pos_cnum }
    in
    let from { Lexing.pos_fname; pos_lnum; pos_bol; pos_cnum } =
      (pos_fname, pos_lnum, pos_bol, pos_cnum)
    in
    let pos_fname = field "pos_fname" (required string) in
    let pos_lnum = field "pos_lnum" (required int) in
    let pos_bol = field "pos_bol" (required int) in
    let pos_cnum = field "pos_cnum" (required int) in
    iso (record (four pos_fname pos_lnum pos_bol pos_cnum)) to_ from

  let sexp =
    let open Conv in
    let to_ (start, stop) = { start; stop } in
    let from { start; stop } = (start, stop) in
    let start = field "start" (required pos_sexp) in
    let stop = field "stop" (required pos_sexp) in
    iso (record (both start stop)) to_ from
end

module Error = struct
  type t =
    { target : string option
    ; message : string
    ; loc : Loc.t option
    }

  let sexp =
    let open Conv in
    let from { target; message; loc } = (target, message, loc) in
    let to_ (target, message, loc) = { target; message; loc } in
    let loc = field "loc" (optional Loc.sexp) in
    let message = field "message" (required string) in
    let target = field "target" (optional string) in
    iso (record (three target message loc)) to_ from
end

module Log = struct
  type t =
    { payload : Sexp.t option
    ; message : string
    }

  let sexp =
    let open Conv in
    let from { payload; message } = (payload, message) in
    let to_ (payload, message) = { payload; message } in
    let payload = field "payload" (optional sexp) in
    let message = field "message" (required string) in
    iso (record (both payload message)) to_ from
end

module Promotion = struct
  type t =
    { in_build : string
    ; in_source : string
    }

  let sexp =
    let open Conv in
    let from { in_build; in_source } = (in_build, in_source) in
    let to_ (in_build, in_source) = { in_build; in_source } in
    let in_build = field "in_build" (required string) in
    let in_source = field "in_source" (required string) in
    iso (record (both in_build in_source)) to_ from
end

module Subscribe = struct
  type t =
    | Error
    | Promotion

  let sexp = Conv.enum [ ("Error", Error); ("Promotion", Promotion) ]
end

module Public = struct
  module Request = struct
    type ('a, 'b) t = ('a, 'b) Decl.request
  end

  module Notification = struct
    type 'a t = 'a Decl.notification

    let shutdown = Decl.notification ~method_:"shutdown" Conv.unit

    let subscribe = Decl.notification ~method_:"subscribe" Subscribe.sexp

    let unsubscribe = Decl.notification ~method_:"unsubscribe" Subscribe.sexp
  end
end

module Server_notifications = struct
  let errors = Decl.notification ~method_:"notify/errors" (Conv.list Error.sexp)

  let promotions =
    Decl.notification ~method_:"notify/promotions" (Conv.list Promotion.sexp)

  let log = Decl.notification ~method_:"notify/log" Log.sexp
end

module Client (S : sig
  module Fiber : sig
    type 'a t

    val return : 'a -> 'a t

    val fork_and_join_unit : (unit -> unit t) -> (unit -> 'a t) -> 'a t

    val parallel_iter : (unit -> 'a option t) -> f:('a -> unit t) -> unit t

    module O : sig
      val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

      val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    end

    module Ivar : sig
      type 'a fiber

      type 'a t

      val create : unit -> 'a t

      val read : 'a t -> 'a fiber

      val fill : 'a t -> 'a -> unit fiber
    end
    with type 'a fiber := 'a t
  end

  module Chan : sig
    type t

    val write : t -> Sexp.t option -> unit Fiber.t

    val read : t -> Sexp.t option Fiber.t
  end
end) =
struct
  open S
  open Fiber.O

  exception Invalid_session of Conv.error

  type t =
    { chan : Chan.t
    ; requests : (Id.t, Response.t Fiber.Ivar.t) Table.t
    ; on_notification : Call.t -> unit Fiber.t
    ; initialize : Initialize.Request.t
    ; mutable next_id : int
    }

  let send t (packet : Message.t option) =
    let sexp =
      Option.map packet ~f:(function
        | Notification p -> Conv.to_sexp (Conv.record Call.fields) p
        | Request (id, request) ->
          let conv = Conv.record (Conv.both Id.required_field Call.fields) in
          Conv.to_sexp conv (id, request))
    in
    Chan.write t.chan sexp

  let create ~chan ~initialize ~on_notification =
    let requests = Table.create (module Id) 16 in
    { chan; requests; on_notification; next_id = 0; initialize }

  let request_untyped t (id, req) =
    let ivar = Fiber.Ivar.create () in
    ( match Table.add t.requests id ivar with
    | Ok () -> ()
    | Error _ -> Code_error.raise "duplicate id" [ ("id", Id.to_dyn id) ] );
    let* () = send t (Some (Request (id, req))) in
    Fiber.Ivar.read ivar

  let request ?id t (decl : _ Decl.request) req =
    let id =
      match id with
      | Some id -> id
      | None ->
        let id = Sexp.List [ Atom "auto"; Atom (Int.to_string t.next_id) ] in
        t.next_id <- t.next_id + 1;
        Id.make id
    in
    let req =
      { Call.params = Conv.to_sexp decl.req req; method_ = decl.method_ }
    in
    let+ res = request_untyped t (id, req) in
    match res with
    | Error e -> Error e
    | Ok res -> (
      match Conv.of_sexp decl.resp ~version:t.initialize.version res with
      | Ok s -> Ok s
      | Error e ->
        (* XXX we need to handle this better *)
        Code_error.raise "response not matched by decl"
          [ ("e", Conv.dyn_of_error e) ] )

  let notification t (decl : _ Decl.notification) n =
    let call =
      { Call.params = Conv.to_sexp decl.req n; method_ = decl.method_ }
    in
    send t (Some (Notification call))

  let read_packets t packets =
    Fiber.parallel_iter packets ~f:(function
      | Packet.Notification n -> t.on_notification n
      | Response (id, response) -> (
        match Table.find t.requests id with
        | Some ivar -> Fiber.Ivar.fill ivar response
        | None ->
          Code_error.raise "unexpected response"
            [ ("id", Id.to_dyn id); ("response", Response.to_dyn response) ] ))

  module Handler = struct
    type t =
      { log : Log.t -> unit Fiber.t
      ; errors : Error.t list -> unit Fiber.t
      ; promotions : Promotion.t list -> unit Fiber.t
      }

    let on_notification (t : t) ~version =
      let table = Table.create (module String) 16 in
      let to_callback (decl : _ Decl.notification) f payload =
        match Conv.of_sexp decl.req payload ~version with
        | Error _ -> Code_error.raise "invalid notification" []
        | Ok s -> f s
      in
      let add (decl : _ Decl.notification) f =
        Table.add_exn table decl.method_ (to_callback decl f)
      in
      add Server_notifications.errors t.errors;
      add Server_notifications.promotions t.promotions;
      add Server_notifications.log t.log;
      fun { Call.method_; params } ->
        match Table.find table method_ with
        | None -> Code_error.raise "invalid method from server" []
        | Some v -> v params

    let log { Log.payload; message } =
      ( match payload with
      | None -> Format.eprintf "%s@." message
      | Some payload ->
        Format.eprintf "%s: %s@." message (Sexp.to_string payload) );
      Fiber.return ()

    let errors _ = failwith "unexpect errors notifications"

    let promotions _ = failwith "unexpeted promotion notifications"

    let default = { log; promotions; errors }

    let create ?log ?errors ?promotions () =
      let t =
        let t = default in
        match log with
        | None -> t
        | Some log -> { t with log }
      in
      let t =
        match promotions with
        | None -> t
        | Some promotions -> { t with promotions }
      in
      let t =
        match errors with
        | None -> t
        | Some errors -> { t with errors }
      in
      t
  end

  let connect_raw chan (initialize : Initialize.Request.t) ~on_notification ~f =
    let packets () =
      let+ read = Chan.read chan in
      Option.map read ~f:(fun sexp ->
          match Conv.of_sexp Packet.sexp ~version:initialize.version sexp with
          | Error e -> raise (Invalid_session e)
          | Ok message -> message)
    in
    let client = create ~initialize ~chan ~on_notification in
    let run () =
      let* init =
        let id = Id.make (List [ Atom "initialize" ]) in
        let initialize = Initialize.Request.to_call initialize in
        request_untyped client (id, initialize)
      in
      match init with
      | Error e -> raise (Response.Error.E e)
      | Ok csexp ->
        let _resp = Conv.of_sexp Initialize.Response.sexp csexp in
        let* res = f client in
        let+ () = Chan.write chan None in
        res
    in
    Fiber.fork_and_join_unit (fun () -> read_packets client packets) run

  let connect ?(handler = Handler.default) chan
      (initialize : Initialize.Request.t) ~f =
    let on_notification =
      Handler.on_notification handler ~version:initialize.version
    in
    connect_raw chan initialize ~f ~on_notification
end
