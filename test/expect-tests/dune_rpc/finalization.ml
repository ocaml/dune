open Stdune
open Fiber.O
module Dune_rpc = Dune_rpc.Private
open Dune_rpc
open Dune_rpc_server
open Common

let print pp = Format.printf "%a@." Pp.to_fmt pp
let print_dyn dyn = print (Dyn.pp dyn)
let decl = simple_request ~method_:(Method.Name.of_string "double") Conv.unit Conv.unit
let witness = Decl.Request.witness decl
let () = Printexc.record_backtrace false

type callback =
  | Print
  | Fail

let dyn_of_callback =
  let open Dyn in
  function
  | Print -> variant "Print" []
  | Fail -> variant "Fail" []
;;

type callbacks =
  { on_init : callback
  ; on_terminate : callback
  ; on_upgrade : callback
  }

let dyn_of_callback { on_init; on_terminate; on_upgrade } =
  Dyn.record
    [ "on_init", dyn_of_callback on_init
    ; "on_terminate", dyn_of_callback on_terminate
    ; "on_upgrade", dyn_of_callback on_upgrade
    ]
;;

let handler { on_init; on_terminate; on_upgrade } =
  let f name what =
    printfn "server: %s" name;
    match what with
    | Print -> Fiber.return ()
    | Fail -> raise Dune_util.Report_error.Already_reported
  in
  let on_init _ _ = f "init" on_init in
  let on_terminate _ = f "terminate" on_terminate in
  let on_upgrade _ _ = f "upgrade" on_upgrade in
  Handler.create ~on_terminate ~on_init ~on_upgrade ~version:(1, 1) ()
;;

let test callback =
  let handler =
    let rpc = handler callback in
    let () =
      let cb _ () = failwith "never works" in
      Handler.implement_request rpc decl cb
    in
    rpc
  in
  let client client =
    printfn "client: sending request";
    let+ resp = request_exn client witness () in
    match resp with
    | Error error -> print_dyn @@ Response.Error.to_dyn error
    | Ok _ -> assert false
  in
  let init =
    { Initialize.Request.dune_version = 1, 1
    ; protocol_version = Protocol.latest_version
    ; id = Id.make (Atom "test-client")
    }
  in
  test ~init ~client ~handler ~private_menu:[ Request decl ] ()
;;

let%expect_test "termination is always called" =
  let kind = [ Print; Fail ] in
  let callbacks =
    List.concat_map kind ~f:(fun on_init ->
      List.concat_map kind ~f:(fun on_terminate ->
        List.concat_map kind ~f:(fun on_upgrade ->
          [ { on_init; on_terminate; on_upgrade } ])))
  in
  List.iter callbacks ~f:(fun callback ->
    dyn_of_callback callback |> print_dyn;
    (try test callback with
     | exn ->
       let exn = Exn_with_backtrace.capture exn in
       Format.printf "%a@.@." Exn_with_backtrace.pp_uncaught exn);
    print_endline "---------------");
  [%expect
    {|
        { on_init = Print; on_terminate = Print; on_upgrade = Print }
        server: init
        server: upgrade
        client: sending request
        { payload =
            Some [ [ [ "exn"; "Failure(\"never works\")" ]; [ "backtrace"; "" ] ] ]
        ; message = "server error"
        ; kind = Code_error
        }
        server: terminate
        server: finished.
        ---------------
        { on_init = Print; on_terminate = Print; on_upgrade = Fail }
        server: init
        server: upgrade
        server: terminate
        server: finished.
        client: sending request
        { payload =
            Some
              [ [ "id"; [ "auto"; "0" ] ]
              ; [ "req"; [ [ "method"; "double" ]; [ "params"; [] ] ] ]
              ]
        ; message = "request sent while connection is dead"
        ; kind = Connection_dead
        }
        ---------------
        { on_init = Print; on_terminate = Fail; on_upgrade = Print }
        server: init
        server: upgrade
        client: sending request
        { payload =
            Some [ [ [ "exn"; "Failure(\"never works\")" ]; [ "backtrace"; "" ] ] ]
        ; message = "server error"
        ; kind = Code_error
        }
        server: terminate
        server: finished.
        ---------------
        { on_init = Print; on_terminate = Fail; on_upgrade = Fail }
        server: init
        server: upgrade
        server: terminate
        /-----------------------------------------------------------------------
        | Internal error: Uncaught exception.
        | Dune_util__Report_error.Already_reported
        \-----------------------------------------------------------------------


        ---------------
        { on_init = Fail; on_terminate = Print; on_upgrade = Print }
        server: init
        server: terminate
        server: finished.
        /-----------------------------------------------------------------------
        | Internal error: Uncaught exception.
        | Response.E
        |   { payload = Some [ [ "id"; [ "initialize" ] ] ]
        |   ; message =
        |       "Connection terminated. This request will never receive a response."
        |   ; kind = Connection_dead
        |   }
        \-----------------------------------------------------------------------


        ---------------
        { on_init = Fail; on_terminate = Print; on_upgrade = Fail }
        server: init
        server: terminate
        server: finished.
        /-----------------------------------------------------------------------
        | Internal error: Uncaught exception.
        | Response.E
        |   { payload = Some [ [ "id"; [ "initialize" ] ] ]
        |   ; message =
        |       "Connection terminated. This request will never receive a response."
        |   ; kind = Connection_dead
        |   }
        \-----------------------------------------------------------------------


        ---------------
        { on_init = Fail; on_terminate = Fail; on_upgrade = Print }
        server: init
        server: terminate
        /-----------------------------------------------------------------------
        | Internal error: Uncaught exception.
        | Dune_util__Report_error.Already_reported
        \-----------------------------------------------------------------------


        ---------------
        { on_init = Fail; on_terminate = Fail; on_upgrade = Fail }
        server: init
        server: terminate
        /-----------------------------------------------------------------------
        | Internal error: Uncaught exception.
        | Dune_util__Report_error.Already_reported
        \-----------------------------------------------------------------------


        ---------------
        |}]
;;
