open Stdune

let%expect_test _ =
  let module Event = Chrome_trace.Event in
  let module Id = Chrome_trace.Id in
  let module Timestamp = Event.Timestamp in
  let events =
    [ Event.complete
        ~dur:(Timestamp.of_float_seconds 1.)
        ~args:[ "foo", `String "bar" ]
        (Event.common_fields ~ts:(Timestamp.of_float_seconds 0.5) ~name:"foo" ())
    ; Event.counter
        (Event.common_fields ~ts:(Timestamp.of_float_seconds 0.5) ~name:"cnt" ())
        [ "bar", `Int 250 ]
    ; Event.async
        (Id.create (`String "foo"))
        Event.Start
        (Event.common_fields ~ts:(Timestamp.of_float_seconds 0.5) ~name:"async" ())
        ~args:[ "foo", `Int 100 ]
    ]
  in
  List.iter events ~f:(fun event ->
    Chrome_trace.Event.to_json event |> Dune_trace.Json.to_string |> print_endline);
  [%expect
    {|
    {"args":{"foo":"bar"},"ph":"X","dur":1000000,"name":"foo","cat":"","ts":500000,"pid":0,"tid":0}
    {"ph":"C","args":{"bar":250},"name":"cnt","cat":"","ts":500000,"pid":0,"tid":0}
    {"args":{"foo":100},"ph":"b","id":"foo","name":"async","cat":"","ts":500000,"pid":0,"tid":0}
    |}]
;;
