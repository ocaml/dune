open Stdune
open Dune_engine
module Running_jobs = Running_jobs

let run fiber = Fiber.run fiber ~iter:(fun () -> [])
let state () = Fiber.Svar.read Running_jobs.jobs
let running_count () = Running_jobs.current (state ()) |> Running_jobs.Id.Map.cardinal

let start id =
  Running_jobs.start
    id
    (Pid.of_int 1234)
    ~description:(Pp.text "job")
    ~started_at:(Time.of_ns 0)
;;

let stop = Running_jobs.stop
let cleanup ids = List.iter ids ~f:(fun id -> run (stop id))
let print_count label = printfn "%s: %d" label (running_count ())

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
  run (start id2);
  run (stop id1);
  let now = state () in
  (match Running_jobs.one_event_diff ~last ~now with
   | None -> print_endline "multiple events"
   | Some _ -> print_endline "unexpected multiple event diff");
  cleanup [ id2 ];
  [%expect
    {|
    single start
    multiple events |}]
;;
