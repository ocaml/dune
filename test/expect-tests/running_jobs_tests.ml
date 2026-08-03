open Stdune
open Dune_engine
module Running_jobs = Running_jobs

let run fiber = Fiber.run fiber ~iter:(fun () -> [])
let state () = Fiber.Svar.read Running_jobs.jobs
let running_count () = Running_jobs.current (state ()) |> Running_jobs.Id.Map.cardinal

let start id =
  Running_jobs.start
    id
    (Pid.of_int_exn 1234)
    ~description:(Pp.text "job")
    ~started_at:(Time.of_ns 0)
;;

let stop = Running_jobs.stop
let cleanup ids = List.iter ids ~f:(fun id -> run (stop id))
let print_count label = printfn "%s: %d" label (running_count ())

let expect_code_error label fiber =
  try
    run fiber;
    printfn "%s: no error" label
  with
  | Code_error.E _ -> printfn "%s: code error" label
;;

let%expect_test "running job updates read state when executed" =
  let id1 = Running_jobs.Id.gen () in
  let id2 = Running_jobs.Id.gen () in
  let start1 = start id1 in
  let start2 = start id2 in
  print_count "initial";
  run start1;
  print_count "after first start";
  run start2;
  print_count "after second start";
  cleanup [ id1; id2 ];
  print_count "after cleanup";
  [%expect
    {|
    initial: 0
    after first start: 1
    after second start: 2
    after cleanup: 0 |}]
;;

let%expect_test "one event diff" =
  let id1 = Running_jobs.Id.gen () in
  let id2 = Running_jobs.Id.gen () in
  let last = state () in
  run (start id1);
  let now = state () in
  (match Running_jobs.one_event_diff ~last ~now with
   | Some (Running_jobs.Start job) when Running_jobs.Id.equal job.id id1 ->
     print_endline "single start"
   | _ -> print_endline "unexpected single start diff");
  let last = state () in
  run (stop id1);
  let now = state () in
  (match Running_jobs.one_event_diff ~last ~now with
   | Some (Running_jobs.Stop id) when Running_jobs.Id.equal id id1 ->
     print_endline "single stop"
   | _ -> print_endline "unexpected single stop diff");
  let last = state () in
  run (start id1);
  run (start id2);
  let now = state () in
  (match Running_jobs.one_event_diff ~last ~now with
   | None -> print_endline "multiple events"
   | Some _ -> print_endline "unexpected multiple event diff");
  cleanup [ id1; id2 ];
  [%expect
    {|
    single start
    single stop
    multiple events |}]
;;

let%expect_test "invalid transitions" =
  let id = Running_jobs.Id.gen () in
  run (start id);
  expect_code_error "duplicate start" (start id);
  run (stop id);
  expect_code_error "unknown stop" (stop id);
  print_count "after invalid transitions";
  [%expect
    {|
    duplicate start: code error
    unknown stop: code error
    after invalid transitions: 0 |}]
;;
