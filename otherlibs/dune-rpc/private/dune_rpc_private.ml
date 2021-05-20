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
        (if Sys.win32 then
          `Ip (Unix.inet_addr_of_string "0.0.0.0", `Port default_port)
        else
          `Unix (Path.build (Path.Build.relative (Lazy.force rpc_dir) fname)))
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
          (match stat.st_kind with
          | Unix.S_SOCK -> `Unix path
          | _ -> of_string (Io.read_file path)))

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

module Target = struct
  type t =
    | Path of string
    | Alias of string
    | Library of string
    | Executables of string list
    | Preprocess of string list
    | Loc of Loc.t

  let sexp =
    let open Conv in
    let path = constr "Path" string (fun p -> Path p) in
    let alias = constr "Alias" string (fun a -> Alias a) in
    let lib = constr "Library" string (fun l -> Library l) in
    let executables =
      constr "Executables" (list string) (fun es -> Executables es)
    in
    let preprocess =
      constr "Preprocess" (list string) (fun ps -> Preprocess ps)
    in
    let loc = constr "Loc" Loc.sexp (fun l -> Loc l) in
    sum
      [ econstr path
      ; econstr alias
      ; econstr lib
      ; econstr executables
      ; econstr preprocess
      ; econstr loc
      ] (function
      | Path p -> case p path
      | Alias a -> case a alias
      | Library l -> case l lib
      | Executables es -> case es executables
      | Preprocess ps -> case ps preprocess
      | Loc l -> case l loc)
end

module Diagnostic = struct
  type severity =
    | Error
    | Warning

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

  type t =
    { targets : Target.t list
    ; message : unit Stdune.Pp.t
    ; loc : Loc.t option
    ; severity : severity option
    ; promotion : Promotion.t list
    ; directory : string option
    }

  let sexp_pp : (unit Stdune.Pp.t, Conv.values) Conv.t =
    let open Conv in
    let open Stdune.Pp.Ast in
    let nop = constr "Nop" unit (fun () -> Nop) in
    let verbatim = constr "Verbatim" string (fun s -> Verbatim s) in
    let char = constr "Char" char (fun c -> Char c) in
    let newline = constr "Newline" unit (fun () -> Newline) in
    let t_fdecl = Fdecl.create Dyn.Encoder.opaque in
    let t = fdecl t_fdecl in
    let text = constr "Text" string (fun s -> Text s) in
    let seq = constr "Seq" (pair t t) (fun (x, y) -> Seq (x, y)) in
    let concat =
      constr "Concat" (pair t (list t)) (fun (x, y) -> Concat (x, y))
    in
    let box = constr "Box" (pair int t) (fun (x, y) -> Box (x, y)) in
    let vbox = constr "Vbox" (pair int t) (fun (x, y) -> Vbox (x, y)) in
    let hbox = constr "Hbox" t (fun t -> Hbox t) in
    let hvbox = constr "Hvbox" (pair int t) (fun (x, y) -> Hvbox (x, y)) in
    let hovbox = constr "Hovbox" (pair int t) (fun (x, y) -> Hovbox (x, y)) in
    let break =
      constr "Break"
        (pair (triple string int string) (triple string int string))
        (fun (x, y) -> Break (x, y))
    in
    let tag = constr "Tag" t (fun t -> Tag ((), t)) in
    let conv =
      sum
        [ econstr nop
        ; econstr verbatim
        ; econstr char
        ; econstr newline
        ; econstr text
        ; econstr seq
        ; econstr concat
        ; econstr box
        ; econstr vbox
        ; econstr hbox
        ; econstr hvbox
        ; econstr hovbox
        ; econstr break
        ; econstr tag
        ] (function
        | Nop -> case () nop
        | Seq (x, y) -> case (x, y) seq
        | Concat (x, y) -> case (x, y) concat
        | Box (i, t) -> case (i, t) box
        | Vbox (i, t) -> case (i, t) vbox
        | Hbox t -> case t hbox
        | Hvbox (i, t) -> case (i, t) hvbox
        | Hovbox (i, t) -> case (i, t) hovbox
        | Verbatim s -> case s verbatim
        | Char c -> case c char
        | Break (x, y) -> case (x, y) break
        | Newline -> case () newline
        | Text s -> case s text
        | Tag ((), t) -> case t tag)
    in
    Fdecl.set t_fdecl conv;
    let to_ast x =
      match Pp.to_ast x with
      | Ok s -> s
      | Error () ->
        (* We don't use the format constructor in dune. *)
        assert false
    in
    iso (Fdecl.get t_fdecl) Pp.of_ast to_ast

  let loc t = t.loc

  let message t = t.message

  let severity t = t.severity

  let promotion t = t.promotion

  let targets t = t.targets

  let directory t = t.directory

  let sexp_severity =
    let open Conv in
    enum [ ("error", Error); ("warning", Warning) ]

  let sexp =
    let open Conv in
    let from { targets; message; loc; severity; promotion; directory } =
      (targets, message, loc, severity, promotion, directory)
    in
    let to_ (targets, message, loc, severity, promotion, directory) =
      { targets; message; loc; severity; promotion; directory }
    in
    let loc = field "loc" (optional Loc.sexp) in
    let message = field "message" (required sexp_pp) in
    let targets = field "targets" (required (list Target.sexp)) in
    let severity = field "severity" (optional sexp_severity) in
    let directory = field "directory" (optional string) in
    let promotion = field "promotion" (required (list Promotion.sexp)) in
    iso (record (six targets message loc severity promotion directory)) to_ from

  module Event = struct
    type nonrec t =
      | Add of t
      | Remove of t

    let sexp =
      let diagnostic = sexp in
      let open Conv in
      let add = constr "Add" diagnostic (fun a -> Add a) in
      let remove = constr "Remove" diagnostic (fun a -> Remove a) in
      sum [ econstr add; econstr remove ] (function
        | Add t -> case t add
        | Remove t -> case t remove)
  end
end

module Build = struct
  module Event = struct
    type t =
      | Start
      | Finish
      | Fail
      | Interrupt
  end
end

module Progress = struct
  type t =
    { complete : int
    ; remaining : int
    }

  let sexp =
    let open Conv in
    let from { complete; remaining } = (complete, remaining) in
    let to_ (complete, remaining) = { complete; remaining } in
    let complete = field "complete" (required int) in
    let remaining = field "remaining" (required int) in
    iso (record (both complete remaining)) to_ from
end

module Message = struct
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

module Subscribe = struct
  type t =
    | Diagnostics
    | Build_progress

  let sexp =
    Conv.enum
      [ ("Diagnostics", Diagnostics); ("Build_progress", Build_progress) ]
end

module Public = struct
  module Request = struct
    type ('a, 'b) t = ('a, 'b) Decl.request

    let ping = Decl.request ~method_:"ping" Conv.unit Conv.unit

    let diagnostics =
      Decl.request ~method_:"diagnostics" Conv.unit (Conv.list Diagnostic.sexp)
  end

  module Notification = struct
    type 'a t = 'a Decl.notification

    let shutdown = Decl.notification ~method_:"shutdown" Conv.unit

    let subscribe = Decl.notification ~method_:"subscribe" Subscribe.sexp

    let unsubscribe = Decl.notification ~method_:"unsubscribe" Subscribe.sexp
  end
end

module Server_notifications = struct
  let abort = Decl.notification ~method_:"notify/abort" Message.sexp

  let diagnostic =
    let open Conv in
    Decl.notification ~method_:"notify/diagnostic" (list Diagnostic.Event.sexp)

  let log = Decl.notification ~method_:"notify/log" Message.sexp

  let progress = Decl.notification ~method_:"notify/progress" Progress.sexp
end

module type S = sig
  type t

  type 'a fiber

  type chan

  val request :
       ?id:Id.t
    -> t
    -> ('a, 'b) Decl.request
    -> 'a
    -> ('b, Response.Error.t) result fiber

  val notification : t -> 'a Decl.notification -> 'a -> unit fiber

  module Handler : sig
    type t

    val create :
         ?log:(Message.t -> unit fiber)
      -> ?diagnostic:(Diagnostic.Event.t list -> unit fiber)
      -> ?build_event:(Build.Event.t -> unit fiber)
      -> ?build_progress:(Progress.t -> unit fiber)
      -> ?abort:(Message.t -> unit fiber)
      -> unit
      -> t
  end

  val connect_raw :
       chan
    -> Initialize.Request.t
    -> on_notification:(Call.t -> unit fiber)
    -> f:(t -> 'a fiber)
    -> 'a fiber

  val connect :
       ?handler:Handler.t
    -> chan
    -> Initialize.Request.t
    -> f:(t -> 'a fiber)
    -> 'a fiber

  val connect_persistent :
       ?on_disconnect:('a -> unit fiber)
    -> chan
    -> on_connect:(unit -> ('a * Initialize.Request.t * Handler.t option) fiber)
    -> on_connected:('a -> t -> unit fiber)
    -> unit fiber
end

module Client (Fiber : sig
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
end) (Chan : sig
  type t

  val write : t -> Sexp.t option -> unit Fiber.t

  val read : t -> Sexp.t option Fiber.t
end) =
struct
  open Fiber.O

  module Chan = struct
    type t =
      { read : unit -> Sexp.t option Fiber.t
      ; write : Sexp.t option -> unit Fiber.t
      ; mutable closed_read : bool
      ; mutable closed_write : bool
      }

    let make read write =
      { read; write; closed_read = false; closed_write = false }

    let of_chan c =
      { read = (fun () -> Chan.read c)
      ; write = (fun s -> Chan.write c s)
      ; closed_read = false
      ; closed_write = false
      }

    let write t s =
      match s with
      | Some _ -> t.write s
      | None ->
        if t.closed_write then
          Fiber.return ()
        else (
          t.closed_write <- true;
          t.write None
        )

    let close_read t = t.closed_read <- true

    let read t =
      if t.closed_read then
        Fiber.return None
      else
        t.read ()
  end

  exception Invalid_session of Conv.error

  type t =
    { chan : Chan.t
    ; requests : (Id.t, Response.t Fiber.Ivar.t) Table.t
    ; on_notification : Call.t -> unit Fiber.t
    ; initialize : Initialize.Request.t
    ; mutable next_id : int
    ; mutable running : bool
    }

  (* When the client is terminated via this function, the session is considered
     to be dead without a way to recover. *)
  let terminate t =
    match t.running with
    | false -> Fiber.return ()
    | true ->
      t.running <- false;
      let ivars = ref [] in
      Table.filteri_inplace t.requests ~f:(fun ~key:_ ~data:ivar ->
          ivars := ivar :: !ivars;
          false);
      let ivars () =
        Fiber.return
          (match !ivars with
          | [] -> None
          | x :: xs ->
            ivars := xs;
            Some x)
      in
      Fiber.fork_and_join_unit
        (fun () -> Chan.write t.chan None)
        (fun () ->
          Fiber.parallel_iter ivars ~f:(fun ivar ->
              let error =
                Response.Error.create ~kind:Code_error
                  ~message:"connection terminated" ()
              in
              Fiber.Ivar.fill ivar (Error error)))

  let terminate_with_error t message info =
    Fiber.fork_and_join_unit
      (fun () -> terminate t)
      (fun () -> Code_error.raise message info)

  let send t (packet : Packet.Query.t option) =
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
    { chan; requests; on_notification; next_id = 0; initialize; running = true }

  let request_untyped t (id, req) =
    match t.running with
    | false ->
      let err =
        Response.Error.create ~message:"connection terminated" ~kind:Code_error
          ()
      in
      Fiber.return (Error err)
    | true ->
      let ivar = Fiber.Ivar.create () in
      (match Table.add t.requests id ivar with
      | Ok () -> ()
      | Error _ -> Code_error.raise "duplicate id" [ ("id", Id.to_dyn id) ]);
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
    let* res = request_untyped t (id, req) in
    match res with
    | Error e -> Fiber.return (Error e)
    | Ok res -> (
      match Conv.of_sexp decl.resp ~version:t.initialize.version res with
      | Ok s -> Fiber.return (Ok s)
      | Error e ->
        terminate_with_error t "response not matched by decl"
          [ ("e", Conv.dyn_of_error e) ])

  let notification t (decl : _ Decl.notification) n =
    match t.running with
    | false ->
      let err =
        Response.Error.create ~message:"connection terminated" ~kind:Code_error
          ()
      in
      raise (Response.Error.E err)
    | true ->
      let call =
        { Call.params = Conv.to_sexp decl.req n; method_ = decl.method_ }
      in
      send t (Some (Notification call))

  let read_packets t packets =
    let* () =
      Fiber.parallel_iter packets ~f:(function
        | Packet.Reply.Notification n -> t.on_notification n
        | Response (id, response) -> (
          match Table.find t.requests id with
          | Some ivar ->
            Table.remove t.requests id;
            Fiber.Ivar.fill ivar response
          | None ->
            terminate_with_error t "unexpected response"
              [ ("id", Id.to_dyn id); ("response", Response.to_dyn response) ]))
    in
    terminate t

  module Handler = struct
    type t =
      { log : Message.t -> unit Fiber.t
      ; abort : Message.t -> unit Fiber.t
      ; diagnostic : Diagnostic.Event.t list -> unit Fiber.t
      ; build_event : Build.Event.t -> unit Fiber.t
      ; build_progress : Progress.t -> unit Fiber.t
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
      add Server_notifications.diagnostic t.diagnostic;
      add Server_notifications.log t.log;
      add Server_notifications.abort t.abort;
      fun { Call.method_; params } ->
        match Table.find table method_ with
        | None -> Code_error.raise "invalid method from server" []
        | Some v -> v params

    let log { Message.payload; message } =
      (match payload with
      | None -> Format.eprintf "%s@." message
      | Some payload ->
        Format.eprintf "%s: %s@." message (Sexp.to_string payload));
      Fiber.return ()

    let diagnostic _ = failwith "unexpected diagnostic notification"

    let build_progress _ = failwith "unexpected build progress notification"

    let abort { Message.payload = _; message } =
      failwith ("Fatal error from server: " ^ message)

    let build_event _ = failwith "unexpected build event notification"

    let default = { log; diagnostic; build_event; build_progress; abort }

    let create ?log ?diagnostic ?build_event ?build_progress ?abort () =
      let t =
        let t = default in
        match log with
        | None -> t
        | Some log -> { t with log }
      in
      let t =
        match diagnostic with
        | None -> t
        | Some diagnostic -> { t with diagnostic }
      in
      let t =
        match build_event with
        | None -> t
        | Some build_event -> { t with build_event }
      in
      let t =
        match build_progress with
        | None -> t
        | Some build_progress -> { t with build_progress }
      in
      let t =
        match abort with
        | None -> t
        | Some abort -> { t with abort }
      in
      t
  end

  let connect_raw chan (initialize : Initialize.Request.t) ~on_notification ~f =
    let packets () =
      let+ read = Chan.read chan in
      Option.map read ~f:(fun sexp ->
          match
            Conv.of_sexp Packet.Reply.sexp ~version:initialize.version sexp
          with
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

  let connect_persistent ?(on_disconnect = fun _ -> Fiber.return ()) chan
      ~on_connect ~on_connected =
    let chan = Chan.of_chan chan in
    let packets () =
      let+ read = Chan.read chan in
      Option.map read ~f:(fun sexp ->
          match Conv.of_sexp Persistent.In.sexp sexp ~version:(0, 0) with
          | Ok m -> m
          | Error e -> raise (Invalid_session e))
    in
    let make_chan packets =
      let read () =
        let+ packet = packets () in
        match (packet : Persistent.In.t option) with
        | None
        | Some Close_connection ->
          None
        | Some (Packet csexp) -> Some csexp
        | Some New_connection ->
          Code_error.raise "Unexpected new connection." []
      in
      let write p =
        let packet =
          match p with
          | Some p -> Persistent.Out.Packet p
          | None -> Close_connection
        in
        let sexp = Conv.to_sexp Persistent.Out.sexp packet in
        Chan.write chan (Some sexp)
      in
      Chan.make read write
    in
    let rec loop () =
      let* packet = packets () in
      match packet with
      | Some New_connection ->
        let* a, init, handler = on_connect () in
        let chan = make_chan packets in
        let* () = connect ?handler chan init ~f:(on_connected a) in
        Chan.close_read chan;
        let* () = on_disconnect a in
        loop ()
      | Some Close_connection -> loop ()
      | None -> Fiber.return ()
      | Some (Packet p) ->
        Code_error.raise "Expected new connection"
          [ ("received", Sexp.to_dyn p) ]
    in
    loop ()

  let connect_raw chan init ~on_notification ~f =
    let chan = Chan.of_chan chan in
    connect_raw chan init ~on_notification ~f

  let connect ?handler chan init ~f =
    let chan = Chan.of_chan chan in
    connect ?handler chan init ~f
end
