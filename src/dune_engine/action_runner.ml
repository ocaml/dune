open Import
open Fiber.O
module Dune_rpc = Dune_rpc_private

module Decl : sig
  val exec : (Action_exec.input, Action_exec.Exec_result.t) Dune_rpc.Decl.request
  val ready : (string, unit) Dune_rpc.Decl.request
  val cancel_build : (unit, unit) Dune_rpc.Decl.request
end = struct
  module Conv = Dune_rpc_private.Conv
  module Decl = Dune_rpc_private.Decl

  (* CR-someday dkalinichenko: this is an ugly implementation detail; consider
     moving this code to its own file. *)
  module Marshallable_error = struct
    (* We convert [Action_exec.Exec_result.Error.t] into this representation
       since [Annots.t] cannot be marshalled (as it contains a [Univ_map.t]).

       This needs to be updated each time we add a new [Annots.t], otherwise
       we will silently drop those annotations when using action runners. *)
    type t =
      | User_with_annots of
          { message : User_message.t (* Should not have any fields in [Annots.t]. *)
          ; has_embedded_location : bool
          ; needs_stack_trace : bool
          ; compound_user_error : Compound_user_error.t list option
              (* Compound user errors do not contain annotations, so it's fine to
                 marshal them as is. *)
          ; diff_promotion : Diff_promotion.Annot.t option
          ; with_directory : Path.t option
          }
      | Code of Code_error.t
      | Sys of string
      | Unix of Unix.error * string * string
      | Nonreproducible_build_cancelled

    let to_ (t : t) : Action_exec.Exec_result.Error.t =
      match t with
      | User_with_annots
          { message
          ; has_embedded_location
          ; needs_stack_trace
          ; compound_user_error
          ; diff_promotion
          ; with_directory
          } ->
        let annots = User_message.Annots.empty in
        let annots =
          match has_embedded_location with
          | true ->
            User_message.Annots.set annots User_message.Annots.has_embedded_location ()
          | false -> annots
        in
        let annots =
          match needs_stack_trace with
          | true ->
            User_message.Annots.set annots User_message.Annots.needs_stack_trace ()
          | false -> annots
        in
        let annots =
          match compound_user_error with
          | Some annot -> User_message.Annots.set annots Compound_user_error.annot annot
          | None -> annots
        in
        let annots =
          match diff_promotion with
          | Some annot -> User_message.Annots.set annots Diff_promotion.Annot.annot annot
          | None -> annots
        in
        let annots =
          match with_directory with
          | Some annot ->
            User_message.Annots.set annots Process.with_directory_annot annot
          | None -> annots
        in
        User
          { loc = message.loc
          ; paragraphs = message.paragraphs
          ; hints = message.hints
          ; annots
          }
      | Code err -> Code err
      | Sys err -> Sys err
      | Unix (err, call, args) -> Unix (err, call, args)
      | Nonreproducible_build_cancelled -> Nonreproducible_build_cancelled
    ;;

    let from (t : Action_exec.Exec_result.Error.t) : t =
      match t with
      | User message ->
        let annots = message.annots in
        let message = { message with annots = User_message.Annots.empty } in
        let has_embedded_location =
          match
            User_message.Annots.find annots User_message.Annots.has_embedded_location
          with
          | Some () -> true
          | None -> false
        in
        let needs_stack_trace =
          match User_message.Annots.find annots User_message.Annots.needs_stack_trace with
          | Some () -> true
          | None -> false
        in
        let compound_user_error =
          User_message.Annots.find annots Compound_user_error.annot
        in
        let diff_promotion = User_message.Annots.find annots Diff_promotion.Annot.annot in
        let with_directory =
          User_message.Annots.find annots Process.with_directory_annot
        in
        User_with_annots
          { message
          ; has_embedded_location
          ; needs_stack_trace
          ; compound_user_error
          ; diff_promotion
          ; with_directory
          }
      | Code err -> Code err
      | Sys err -> Sys err
      | Unix (err, call, args) -> Unix (err, call, args)
      | Nonreproducible_build_cancelled -> Nonreproducible_build_cancelled
    ;;
  end

  module Exec = struct
    let marshal () =
      let to_ data = Marshal.from_string data 0 in
      let from action = Marshal.to_string action [] in
      Conv.iso Conv.string to_ from
    ;;

    let marshal_result () =
      let to_ = Result.map_error ~f:(List.map ~f:Marshallable_error.to_) in
      let from = Result.map_error ~f:(List.map ~f:Marshallable_error.from) in
      Conv.iso (marshal ()) to_ from
    ;;

    let decl =
      let v1 =
        Decl.Request.make_current_gen
          ~req:(marshal ())
          ~resp:(marshal_result ())
          ~version:1
      in
      Decl.Request.make ~method_:"action/exec" ~generations:[ v1 ]
    ;;
  end

  module Ready = struct
    let decl =
      let v1 =
        Decl.Request.make_current_gen ~req:Conv.string ~resp:Conv.unit ~version:1
      in
      Decl.Request.make ~method_:"action/ready" ~generations:[ v1 ]
    ;;
  end

  module Cancel_build = struct
    let decl =
      let v1 = Decl.Request.make_current_gen ~req:Conv.unit ~resp:Conv.unit ~version:1 in
      Decl.Request.make ~method_:"action/cancel-build" ~generations:[ v1 ]
    ;;
  end

  let exec = Exec.decl
  let ready = Ready.decl
  let cancel_build = Cancel_build.decl
end

module Client = Dune_rpc_client.Client

type session = Session : _ Dune_rpc_server.Session.t -> session

type initialized =
  { session : session
  ; id : (module Stdune.Id.S)
  }

type status =
  | Awaiting_initialization of unit Fiber.Ivar.t
  | Initialized of initialized
  | Closed

let dyn_of_status =
  let open Dyn in
  function
  | Awaiting_initialization _ -> variant "Awaiting_initialization" []
  | Initialized _ -> variant "Initialized" []
  | Closed -> variant "Closed" []
;;

module Id = Stdune.Id.Make ()

type t =
  { name : string
  ; id : Id.t
  ; mutable status : status
  }

let name t = t.name

let send_request ~info ~request ~payload t =
  let* { session; id = (module Id) } =
    match t.status with
    | Closed ->
      Code_error.raise "action runner disconnected" [ "name", Dyn.string t.name ]
    | Initialized s -> Fiber.return s
    | Awaiting_initialization ready ->
      let+ () = Fiber.Ivar.read ready in
      (match t.status with
       | Closed ->
         Code_error.raise
           "action runner disconnected before initialization"
           [ "name", Dyn.string t.name ]
       | Initialized s -> s
       | Awaiting_initialization _ ->
         (* we just finished initializing *)
         assert false)
  in
  let (Session session) = session in
  let id = Dune_rpc.Id.make @@ Csexp.Atom (Int.to_string @@ Id.to_int @@ Id.gen ()) in
  if !Log.verbose then Log.info info;
  Dune_rpc_server.Session.request
    session
    (Dune_rpc.Decl.Request.witness request)
    id
    payload
;;

let exec_action (t : t) (action : Action_exec.input) =
  send_request
    ~info:
      [ Pp.textf
          "dispatching action at %s to %s"
          (Path.to_string_maybe_quoted action.root)
          t.name
      ]
    ~request:Decl.exec
    ~payload:action
    t
;;

let cancel_build (t : t) =
  send_request
    ~info:[ Pp.textf "cancelling all builds at %s" t.name ]
    ~request:Decl.cancel_build
    ~payload:()
    t
;;

let _to_dyn { name; id; status } =
  let open Dyn in
  record [ "name", string name; "id", Id.to_dyn id; "status", dyn_of_status status ]
;;

module Rpc_server = struct
  type nonrec t =
    { workers : (string, t) Table.t
    ; pool : Fiber.Pool.t
    }

  let create () =
    { workers = Table.create (module String) 16; pool = Fiber.Pool.create () }
  ;;

  let all_runners t = Table.values t.workers
  let run t = Fiber.Pool.run t.pool
  let stop t = Fiber.Pool.close t.pool

  let close worker =
    match worker.status with
    | Closed -> ()
    | _ -> worker.status <- Closed
  ;;

  let register t worker =
    match Table.add t.workers worker.name worker with
    | Ok () -> ()
    | Error _ ->
      User_error.raise [ Pp.textf "Cannot register %s as it already exists" worker.name ]
  ;;

  let implement_handler t (handler : _ Dune_rpc_server.Handler.t) =
    Dune_rpc_server.Handler.declare_request handler Decl.exec;
    Dune_rpc_server.Handler.declare_request handler Decl.cancel_build;
    Dune_rpc_server.Handler.implement_request handler Decl.ready
    @@ fun session name ->
    let socket_name = Dune_rpc_server.Session.name session in
    if not (name = socket_name)
    then (
      let error =
        Dune_rpc.Response.Error.create
          ~payload:
            (Sexp.record
               [ "name", Csexp.Atom name; "socket_name", Csexp.Atom socket_name ])
          ~kind:Invalid_request
          ~message:"action runner connected to the wrong socket"
          ()
      in
      raise (Dune_rpc.Response.Error.E error));
    match Table.find t.workers name with
    | None ->
      let error =
        Dune_rpc.Response.Error.create
          ~kind:Invalid_request
          ~message:"unexpected action runner"
          ()
      in
      raise (Dune_rpc.Response.Error.E error)
    | Some worker ->
      (match worker.status with
       | Closed ->
         let error =
           Dune_rpc.Response.Error.create
             ~kind:Invalid_request
             ~message:"disconnected earlier"
             ()
         in
         raise (Dune_rpc.Response.Error.E error)
       | Initialized _ ->
         let error =
           Dune_rpc.Response.Error.create
             ~kind:Invalid_request
             ~message:"already signalled readiness to the server"
             ()
         in
         raise (Dune_rpc.Response.Error.E error)
       | Awaiting_initialization ivar ->
         let initialized =
           { session = Session session; id = (module Stdune.Id.Make ()) }
         in
         worker.status <- Initialized initialized;
         if !Log.verbose then Log.info [ Pp.textf "action runner %s connected" name ];
         let* () =
           Fiber.Pool.task t.pool ~f:(fun () ->
             let+ () = Dune_rpc_server.Session.closed session in
             close worker)
         in
         Fiber.Ivar.fill ivar ())
  ;;
end

let create server ~name =
  let init = Fiber.Ivar.create () in
  let t = { name; id = Id.gen (); status = Awaiting_initialization init } in
  Rpc_server.register server t;
  t
;;

module Worker = struct
  let exec_action =
    let build_deps _ = Code_error.raise "no dynamic actions yet" [] in
    fun (action : Action_exec.input) ->
      Log.info
        [ Pp.text "action runner executing action:"
        ; Action.for_shell action.action |> Action_to_sh.pp
        ];
      Action_exec.exec ~build_deps action
  ;;

  let cancel_build = Scheduler.cancel_current_build

  let start ~name ~where =
    let* connection = Client.Connection.connect_exn where in
    let private_menu : Client.proc list =
      [ Request Decl.ready
      ; Handle_request (Decl.exec, exec_action)
      ; Handle_request (Decl.cancel_build, cancel_build)
      ]
    in
    let id = Dune_rpc.Id.make (Sexp.Atom name) in
    Dune_rpc.Initialize.Request.create ~id
    |> Client.client ~private_menu connection ~f:(fun client ->
      let* request =
        Client.Versioned.prepare_request
          client
          (Dune_rpc_private.Decl.Request.witness Decl.ready)
      in
      match request with
      | Error v ->
        User_error.raise
          [ Pp.textf
              "Server does not agree on the menu. Are you running the same dune binary \
               for the worker?"
          ; Pp.text (Dune_rpc.Version_error.message v)
          ]
      | Ok request ->
        let* response = Client.request client request name in
        (match response with
         | Ok () ->
           if !Log.verbose
           then Log.info [ Pp.textf "action runner %s successfully connected" name ];
           Client.disconnected client
         | Error e ->
           User_error.raise
             [ Pp.textf
                 "Failed to signal readiness to the server %S"
                 (Dune_rpc.Where.to_string where)
             ; Pp.text @@ Dune_rpc.Response.Error.message e
             ]))
  ;;
end
