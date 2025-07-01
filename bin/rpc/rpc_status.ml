open Import
module Client = Dune_rpc_client.Client

let ( let** ) x f =
  let open Fiber.O in
  let* x = x in
  match x with
  | Ok s -> f s
  | Error e -> Fiber.return (Error e)
;;

let ( let++ ) x f =
  let open Fiber.O in
  let+ x = x in
  match x with
  | Ok s -> Ok (f s)
  | Error e -> Error e
;;

(** Get the status of a server at a given location and apply a function to the
    list of clients *)
let server_response_map ~where ~f =
  (* TODO: add timeout for status check *)
  let open Fiber.O in
  let** conn =
    Client.Connection.connect where >>| Result.map_error ~f:User_message.to_string
  in
  Dune_rpc_impl.Client.client
    conn
    (Dune_rpc.Initialize.Request.create ~id:(Dune_rpc.Id.make (Sexp.Atom "status")))
    ~f:(fun session ->
      let open Fiber.O in
      let++ response =
        let** decl =
          Client.Versioned.prepare_request
            session
            (Dune_rpc_private.Decl.Request.witness Dune_rpc_impl.Decl.status)
          >>| Result.map_error ~f:Dune_rpc_private.Version_error.message
        in
        Client.request session decl ()
        >>| Result.map_error ~f:Dune_rpc.Response.Error.message
      in
      f response.Dune_rpc_impl.Decl.Status.clients)
;;

(** Get a list of registered Dunes from the RPC registry *)
let registered_dunes () : Dune_rpc.Registry.Dune.t list Fiber.t =
  let config = Dune_rpc_private.Registry.Config.create (Lazy.force Dune_util.xdg) in
  let registry = Dune_rpc_private.Registry.create config in
  let open Fiber.O in
  let+ _result = Dune_rpc_impl.Poll_active.poll registry in
  Dune_rpc_private.Registry.current registry
;;

(** The type of server statuses *)
type status =
  { root : string
  ; pid : Pid.t
  ; result : (int, string) result
  }

(** Fetch the status of a single Dune instance *)
let get_status (dune : Dune_rpc.Registry.Dune.t) =
  let root = Dune_rpc_private.Registry.Dune.root dune in
  let pid = Dune_rpc_private.Registry.Dune.pid dune |> Pid.of_int in
  let where = Dune_rpc_private.Registry.Dune.where dune in
  let open Fiber.O in
  let+ result = server_response_map ~where ~f:List.length in
  { root; pid; result }
;;

(** Print a list of statuses to the console *)
let print_statuses statuses =
  List.sort statuses ~compare:(fun x y -> String.compare x.root y.root)
  |> Pp.concat_map ~sep:Pp.space ~f:(fun { root; pid; result } ->
    Pp.concat
      ~sep:Pp.space
      [ Pp.textf "root: %s" root
      ; Pp.enumerate
          ~f:Fun.id
          [ Pp.textf "pid: %d" (Pid.to_int pid)
          ; Pp.textf
              "clients: %s"
              (match result with
               | Ok n -> string_of_int n
               | Error e -> e)
          ]
      ])
  |> Pp.vbox
  |> List.singleton
  |> Console.print
;;

let term =
  let+ builder = Common.Builder.term
  and+ all =
    Arg.(
      value
      & flag
      & info
          [ "all" ]
          ~doc:
            "Show all running Dune instances together with their root, pids and number \
             of clients.")
  in
  Rpc_common.client_term builder
  @@ fun () ->
  let open Fiber.O in
  if all
  then
    let* dunes = registered_dunes () in
    let+ statuses = Fiber.parallel_map ~f:get_status dunes in
    print_statuses statuses
  else (
    let where = Rpc_common.active_server () in
    Console.print
      [ Pp.textf "Server is listening on %s" (Dune_rpc.Where.to_string where)
      ; Pp.text "Connected clients (including this one):"
      ];
    server_response_map ~where ~f:(fun clients ->
      List.iter clients ~f:(fun (client, menu) ->
        let id =
          let sexp = Dune_rpc.Conv.to_sexp Dune_rpc.Id.sexp client in
          Sexp.to_string sexp
        in
        let message =
          match (menu : Dune_rpc_impl.Decl.Status.Menu.t) with
          | Uninitialized ->
            User_message.make
              [ Pp.textf "Client [%s], conducting version negotiation" id ]
          | Menu menu ->
            User_message.make
              [ Pp.vbox
                  ~indent:2
                  (Pp.concat
                     ~sep:Pp.space
                     (Pp.textf "Client [%s] with the following RPC versions:" id
                      :: List.map menu ~f:(fun (method_, version) ->
                        Pp.textf "%s: %d" method_ version)))
              ]
        in
        Console.print_user_message message))
    >>| function
    | Ok () -> ()
    | Error e -> Printf.printf "Error: %s\n" e)
;;

let info =
  let doc = "show active connections" in
  Cmd.info "status" ~doc
;;

let cmd = Cmd.v info term
