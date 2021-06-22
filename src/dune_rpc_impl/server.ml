open! Stdune
open Fiber.O
open Dune_rpc_server
module Dune_rpc = Dune_rpc_private
module Subscribe = Dune_rpc.Subscribe
module Initialize = Dune_rpc.Initialize
module Public = Dune_rpc.Public
module Server_notifications = Dune_rpc.Server_notifications
module Progress = Dune_rpc.Progress
module Id = Dune_rpc.Id
module Diagnostic = Dune_rpc.Diagnostic
module Conv = Dune_rpc.Conv
module Dep_conf = Dune_rules.Dep_conf
module Source_tree = Dune_engine.Source_tree
module Build_system = Dune_engine.Build_system
module Dune_project = Dune_engine.Dune_project
module Promotion = Dune_engine.Promotion

module Build_outcome = struct
  type t = Dune_engine.Scheduler.Run.Build_outcome_for_rpc.t =
    | Success
    | Failure

  let sexp = Conv.enum [ ("Success", Success); ("Failure", Failure) ]
end

type pending_build_action =
  | Build of Dep_conf.t list * Build_outcome.t Fiber.Ivar.t

let absolutize_paths ~dir (loc : Loc.t) =
  let make_path name =
    Path.to_absolute_filename
      (if Filename.is_relative name then
        Path.append_local dir (Path.Local.parse_string_exn ~loc name)
      else
        Path.of_string name)
  in
  { Loc.start = { loc.start with pos_fname = make_path loc.start.pos_fname }
  ; stop = { loc.stop with pos_fname = make_path loc.stop.pos_fname }
  }

let diagnostic_of_error : Build_system.Error.t -> Dune_rpc_private.Diagnostic.t
    =
 fun m ->
  let message, related, dir = Build_system.Error.info m in
  let make_loc loc =
    match dir with
    | None -> loc
    | Some dir -> absolutize_paths ~dir loc
  in
  let loc = Option.map message.loc ~f:make_loc in
  let make_message pars = Pp.map_tags (Pp.concat pars) ~f:(fun _ -> ()) in
  let id = Build_system.Error.id m |> Diagnostic.Id.create in
  let promotion =
    match Build_system.Error.promotion m with
    | None -> []
    | Some { in_source; in_build } ->
      [ { Diagnostic.Promotion.in_source =
            Path.to_absolute_filename (Path.source in_source)
        ; in_build = Path.to_absolute_filename (Path.build in_build)
        }
      ]
  in
  let related =
    List.map related ~f:(fun (related : User_message.t) ->
        { Dune_rpc_private.Diagnostic.Related.message =
            make_message related.paragraphs
        ; loc = make_loc (Option.value_exn related.loc)
        })
  in
  { severity = None
  ; id
  ; targets = []
  ; message = make_message message.paragraphs
  ; loc
  ; promotion
  ; related
  ; directory =
      Option.map
        ~f:(fun p ->
          Path.to_absolute_filename
            (Path.drop_optional_build_context_maybe_sandboxed p))
        dir
  }

(* TODO un-copy-paste from dune/bin/arg.ml *)
let dep_parser =
  let open Dune_engine in
  Dune_lang.Syntax.set Stanza.syntax (Active Stanza.latest_version)
    Dep_conf.decode

module Decl = struct
  module Decl = Dune_rpc.Decl

  let build =
    Decl.request ~method_:"build" Conv.(list string) Build_outcome.sexp

  let shutdown = Decl.notification ~method_:"shutdown" Conv.unit

  module Status = struct
    type t = { clients : Id.t list }

    let sexp =
      let open Conv in
      let to_ clients = { clients } in
      let from { clients } = clients in
      iso (list Id.sexp) to_ from
  end

  let status = Decl.request ~method_:"status" Conv.unit Status.sexp
end

module Client = struct
  type t = { mutable next_id : int }

  let create () = { next_id = 0 }

  let to_dyn { next_id = _ } =
    let open Dyn.Encoder in
    opaque ()
end

module Session_comparable = Comparable.Make (struct
  type t = Client.t Session.t

  let compare = Session.compare

  let to_dyn s = Session.to_dyn Client.to_dyn s
end)

module Session_set = Session_comparable.Set

module Subscribers : sig
  type t

  type _ what =
    | Diagnostic : Diagnostic.Event.t list what
    | Progress : Progress.t what

  val empty : t

  val remove : t -> 'a what -> 'a Subscription.t -> t

  (** Remove all subscriptions of a sesssion *)
  val remove_session : t -> Client.t Session.t -> t

  val add : t -> 'a what -> 'a Subscription.t -> t

  val notify : t -> 'a what -> 'a -> unit Fiber.t
end = struct
  module S = struct
    let compare = Subscription.compare

    let to_dyn x = Subscription.to_dyn x
  end

  module Progress_subs_comparable = Comparable.Make (struct
    type t = Progress.t Subscription.t

    include S
  end)

  module Progress_subs = Progress_subs_comparable.Set

  module Diagnostic_subs_comparable = Comparable.Make (struct
    type t = Diagnostic.Event.t list Subscription.t

    include S
  end)

  module Diagnostic_subs = Diagnostic_subs_comparable.Set
  module Session_map = Session_comparable.Map

  module Sub_comparable = Comparable.Make (struct
    type t = Subscription.packed

    let compare x y = Subscription.compare_packed x y

    let to_dyn (Subscription.E x) = Subscription.to_dyn x
  end)

  module Sub_map = Sub_comparable.Map

  type t =
    { build_progress : Progress.t Subscription.t Sub_map.t
    ; diagnostic : Diagnostic.Event.t list Subscription.t Sub_map.t
    }

  type _ what =
    | Diagnostic : Diagnostic.Event.t list what
    | Progress : Progress.t what

  let empty = { build_progress = Sub_map.empty; diagnostic = Sub_map.empty }

  let with_sub_map (type a b) t (what : a what)
      ~(f : a Subscription.t Sub_map.t -> b * a Subscription.t Sub_map.t) :
      b * t =
    match what with
    | Progress ->
      let res, build_progress = f t.build_progress in
      (res, { t with build_progress })
    | Diagnostic ->
      let res, diagnostic = f t.diagnostic in
      (res, { t with diagnostic })

  let modify t what ~f =
    let (), res = with_sub_map t what ~f:(fun map -> ((), f map)) in
    res

  let add (type a) t (what : a what) (sub : a Subscription.t) =
    let key = Subscription.E sub in
    modify t what ~f:(fun map -> Sub_map.add_exn map key sub)

  let remove (type a) t (what : a what) (sub : a Subscription.t) =
    let key = Subscription.E sub in
    modify t what ~f:(fun map -> Sub_map.remove map key)

  let remove_session t session =
    Session.subscriptions session
    |> List.fold_left ~init:t ~f:(fun t sub ->
           { build_progress = Sub_map.remove t.build_progress sub
           ; diagnostic = Sub_map.remove t.diagnostic sub
           })

  let notify (type a) t (what : a what) (a : a) =
    let fiber, _map =
      with_sub_map t what ~f:(fun map ->
          ( Sub_map.values map
            |> Fiber.parallel_iter ~f:(fun sub -> Subscription.update sub a)
          , map ))
    in
    fiber
end

(** Primitive unbounded FIFO channel. Reads are blocking. Writes are not
    blocking. At most one read is allowed at a time. *)
module Job_queue : sig
  type 'a t

  (** Remove the element from the internal queue without waiting for the next
      element. *)
  val pop_internal : 'a t -> 'a option

  val create : unit -> 'a t

  val read : 'a t -> 'a Fiber.t

  val write : 'a t -> 'a -> unit Fiber.t
end = struct
  (* invariant: if reader is Some then queue is empty *)
  type 'a t =
    { queue : 'a Queue.t
    ; mutable reader : 'a Fiber.Ivar.t option
    }

  let create () = { queue = Queue.create (); reader = None }

  let pop_internal t = Queue.pop t.queue

  let read t =
    Fiber.of_thunk (fun () ->
        match t.reader with
        | Some _ ->
          Code_error.raise "multiple concurrent reads of build job queue" []
        | None -> (
          match Queue.pop t.queue with
          | None ->
            let ivar = Fiber.Ivar.create () in
            t.reader <- Some ivar;
            Fiber.Ivar.read ivar
          | Some v -> Fiber.return v))

  let write t elem =
    Fiber.of_thunk (fun () ->
        match t.reader with
        | Some ivar ->
          t.reader <- None;
          Fiber.Ivar.fill ivar elem
        | None ->
          Queue.push t.queue elem;
          Fiber.return ())
end

type t =
  { config : Run.Config.t
  ; pending_build_jobs :
      (Dep_conf.t list * Build_outcome.t Fiber.Ivar.t) Job_queue.t
  ; build_handler : Build_system.Handler.t
  ; pool : Fiber.Pool.t
  ; mutable subscribers : Subscribers.t
  ; mutable clients : Session_set.t
  }

let build_handler t = t.build_handler

let handler (t : t Fdecl.t) : 'a Dune_rpc_server.Handler.t =
  let on_init session (_ : Initialize.Request.t) =
    let t = Fdecl.get t in
    let client = Client.create () in
    t.clients <- Session_set.add t.clients session;
    Fiber.return client
  in
  let on_terminate session =
    let t = Fdecl.get t in
    t.subscribers <- Subscribers.remove_session t.subscribers session;
    Fiber.return ()
  in
  let rpc =
    Handler.create ~on_terminate ~on_init
      ~version:Dune_rpc_private.Version.latest ()
  in
  let () =
    Handler.request rpc
      (Handler.callback (Handler.public ~since:(1, 0) ()) Fiber.return)
      Public.Request.ping
  in
  let () =
    let build targets =
      let ivar = Fiber.Ivar.create () in
      let targets =
        List.map targets ~f:(fun s ->
            Dune_lang.Decoder.parse dep_parser
              (Univ_map.set Univ_map.empty
                 Dune_engine.String_with_vars.decoding_env_key
                 (* CR-someday aalekseyev: hardcoding the version here is not
                    ideal, but it will do for now since this command is not
                    stable and we're only using it in tests. *)
                 (Dune_engine.Pform.Env.initial (3, 0)))
              (Dune_lang.Parser.parse_string ~fname:"dune rpc"
                 ~mode:Dune_lang.Parser.Mode.Single s))
      in
      let* () =
        Job_queue.write (Fdecl.get t).pending_build_jobs (targets, ivar)
      in
      Fiber.Ivar.read ivar
    in
    Handler.request rpc (Handler.callback Handler.private_ build) Decl.build
  in
  let () =
    let rec cancel_pending_jobs () =
      match Job_queue.pop_internal (Fdecl.get t).pending_build_jobs with
      | None -> Fiber.return ()
      | Some (_, job) ->
        let* () = Fiber.Ivar.fill job Build_outcome.Failure in
        cancel_pending_jobs ()
    in
    let shutdown () =
      let t = Fdecl.get t in
      let terminate_sessions () =
        Fiber.fork_and_join_unit cancel_pending_jobs (fun () ->
            Fiber.parallel_iter_set
              (module Session_set)
              t.clients ~f:Session.request_close)
      in
      let shutdown () =
        Fiber.fork_and_join_unit Dune_engine.Scheduler.shutdown Run.stop
      in
      Fiber.fork_and_join_unit terminate_sessions shutdown
    in
    Handler.notification rpc
      (Handler.callback Handler.private_ shutdown)
      Decl.shutdown
  in
  let () =
    let info = Handler.public ~since:(3, 0) () in
    Handler.subscription rpc info Sub.progress
      ~on_subscribe:(fun _ -> Fiber.return ())
      ~subscription:(fun _session () sub ->
        let t = Fdecl.get t in
        let* () =
          let event =
            match Build_system.last_event () with
            | Some Fail -> Progress.Failed
            | Some Interrupt -> Interrupted
            | Some Finish -> Success
            | None -> Progress.Waiting
            | Some Start ->
              let current_progress : Build_system.Progress.t =
                Build_system.get_current_progress ()
              in
              let complete = current_progress.number_of_rules_executed in
              In_progress
                { complete
                ; remaining =
                    current_progress.number_of_rules_discovered - complete
                }
          in
          Subscription.update sub event
        in
        t.subscribers <- Subscribers.add t.subscribers Progress sub;
        let+ () = Subscription.finished sub in
        t.subscribers <- Subscribers.remove t.subscribers Progress sub)
  in
  let () =
    let info = Handler.public ~since:(3, 0) () in
    Handler.subscription rpc info Sub.diagnostic
      ~on_subscribe:(fun _ -> Fiber.return ())
      ~subscription:(fun _session () sub ->
        let t = Fdecl.get t in
        let* () =
          Build_system.errors ()
          |> List.map ~f:(fun e -> Diagnostic.Event.Add (diagnostic_of_error e))
          |> Subscription.update sub
        in
        t.subscribers <- Subscribers.add t.subscribers Diagnostic sub;
        let+ () = Subscription.finished sub in
        t.subscribers <- Subscribers.remove t.subscribers Diagnostic sub)
  in
  let () =
    let f () =
      let t = Fdecl.get t in
      let clients =
        Session_set.to_list_map t.clients ~f:(fun session ->
            Session.initialize session |> Initialize.Request.id)
      in
      Fiber.return { Decl.Status.clients }
    in
    let cb = Handler.callback Handler.private_ f in
    Handler.request rpc cb Decl.status
  in
  let () =
    let cb =
      let f () =
        Build_system.errors ()
        |> List.map ~f:diagnostic_of_error
        |> Fiber.return
      in
      Handler.callback (Handler.public ~since:(1, 0) ()) f
    in
    Handler.request rpc cb Public.Request.diagnostics
  in
  let source_path_of_string path =
    if Filename.is_relative path then
      Path.Source.(relative root path)
    else
      let source_root =
        Path.to_absolute_filename (Path.source Path.Source.root)
      in
      match String.drop_prefix path ~prefix:source_root with
      | None ->
        User_error.raise [ Pp.textf "path isn't available in workspace" ]
      | Some s ->
        let s = String.drop_prefix_if_exists s ~prefix:"/" in
        Path.Source.(relative root s)
  in
  let () =
    let cb =
      let f (path, `Contents contents) =
        let+ version =
          Memo.Build.run
            (let open Memo.Build.O in
            let source_path = source_path_of_string path in
            let+ dir = Source_tree.nearest_dir source_path in
            let project = Source_tree.Dir.project dir in
            Dune_project.dune_version project)
        in
        let module Format_dune_lang = Dune_engine.Format_dune_lang in
        Format_dune_lang.format_string ~version contents
      in
      Handler.callback (Handler.public ~since:(1, 0) ()) f
    in
    Handler.request rpc cb Public.Request.format_dune_file
  in
  let () =
    let cb =
      let f path =
        let files = source_path_of_string path in
        Promotion.promote_files_registered_in_last_run
          (These ([ files ], ignore));
        Fiber.return ()
      in
      Handler.callback (Handler.public ~since:(1, 0) ()) f
    in
    Handler.request rpc cb Public.Request.promote
  in
  rpc

let task t f =
  let* running = Fiber.Pool.running t.pool in
  if running then
    Fiber.Pool.task t.pool ~f
  else
    Fiber.return ()

let error t errors =
  let t = Fdecl.get t in
  task t (fun () ->
      List.map errors ~f:(fun (e : Build_system.Handler.error) ->
          match e with
          | Add x -> Diagnostic.Event.Add (diagnostic_of_error x)
          | Remove x -> Remove (diagnostic_of_error x))
      |> Subscribers.notify t.subscribers Diagnostic)

let progress_of_build_event : Build_system.Handler.event -> Progress.t =
  function
  | Start -> Progress.In_progress { complete = 0; remaining = 0 }
  | Finish -> Success
  | Interrupt -> Interrupted
  | Fail -> Failed

let build_progress t ~complete ~remaining =
  let t = Fdecl.get t in
  task t (fun () ->
      let notification = Progress.In_progress { complete; remaining } in
      Subscribers.notify t.subscribers Progress notification)

let build_event t (event : Build_system.Handler.event) =
  let t = Fdecl.get t in
  task t (fun () ->
      let notification = progress_of_build_event event in
      Subscribers.notify t.subscribers Progress notification)

let create () =
  let t = Fdecl.create Dyn.Encoder.opaque in
  let pending_build_jobs = Job_queue.create () in
  let handler = Dune_rpc_server.make (handler t) in
  let pool = Fiber.Pool.create () in
  let config = Run.Config.Server { handler; backlog = 10; pool } in
  let build_handler =
    Build_system.Handler.create ~error:(error t)
      ~build_progress:(build_progress t) ~build_event:(build_event t)
  in
  let res =
    { config
    ; pending_build_jobs
    ; subscribers = Subscribers.empty
    ; clients = Session_set.empty
    ; build_handler
    ; pool
    }
  in
  Fdecl.set t res;
  res

let config t = t.config

let pending_build_action t =
  Job_queue.read t.pending_build_jobs
  |> Fiber.map ~f:(fun (targets, ivar) -> Build (targets, ivar))
