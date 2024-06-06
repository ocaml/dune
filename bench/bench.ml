open Stdune
module Process = Dune_engine.Process

module Console = struct
  include Dune_console

  let printf fmt = printf ("[Bench] " ^^ fmt)
end

module Json = struct
  include Chrome_trace.Json
  include Dune_stats.Json
end

module Output = struct
  type measurement =
    [ `Int of int
    | `Float of float
    ]

  type bench =
    { name : string
    ; metrics : (string * [ measurement | `List of measurement list ] * string) list
    }

  let json_of_bench { name; metrics } : Json.t =
    let metrics =
      List.map metrics ~f:(fun (name, value, units) ->
        let value =
          match value with
          | `Int i -> `Int i
          | `Float f -> `Float f
          | `List xs -> `List (xs :> Json.t list)
        in
        `Assoc [ "name", `String name; "value", value; "units", `String units ])
    in
    `Assoc [ "name", `String name; "metrics", `List metrics ]
  ;;

  type t =
    { config : (string * Json.t) list
    ; version : int
    ; results : bench list
    }

  let to_json { config; version; results } : Json.t =
    let assoc = [ "results", `List (List.map results ~f:json_of_bench) ] in
    let assoc = ("version", `Int version) :: assoc in
    let assoc =
      match config with
      | [] -> assoc
      | _ :: _ -> ("config", `Assoc config) :: assoc
    in
    `Assoc assoc
  ;;
end

let git =
  lazy
    (let path = Env.get Env.initial "PATH" |> Option.value_exn |> Bin.parse_path in
     Bin.which ~path "git" |> Option.value_exn)
;;

let dune = Path.of_string (Filename.concat Fpath.initial_cwd Sys.argv.(1))
let output_limit = Dune_engine.Execution_parameters.Action_output_limit.default
let make_stdout () = Process.Io.make_stdout ~output_on_success:Swallow ~output_limit
let make_stderr () = Process.Io.make_stderr ~output_on_success:Swallow ~output_limit

module Package = struct
  type t =
    { org : string
    ; name : string
    }

  let uri { org; name } = sprintf "https://github.com/%s/%s" org name
  let make org name = { org; name }

  let clone t =
    let stdout_to = make_stdout () in
    let stderr_to = make_stderr () in
    let stdin_from = Process.Io.(null In) in
    Process.run
      Strict
      ~display:Quiet
      ~stdout_to
      ~stderr_to
      ~stdin_from
      (Lazy.force git)
      [ "clone"; uri t ]
  ;;
end

let duniverse =
  let pkg = Package.make in
  [ pkg "ocaml-dune" "dune-bench" ]
;;

let prepare_workspace () =
  Fiber.parallel_iter duniverse ~f:(fun (pkg : Package.t) ->
    Fpath.rm_rf pkg.name;
    Console.printf "cloning %s/%s" pkg.org pkg.name;
    Fiber.finalize
      (fun () -> Package.clone pkg)
      ~finally:(fun () ->
        Fiber.return @@ Console.printf "finished cloning %s/%s" pkg.org pkg.name))
;;

let dune_build ~name ~sandbox =
  let stdin_from = Process.(Io.null In) in
  let stdout_to = make_stdout () in
  let stderr_to = make_stderr () in
  let gc_dump = Temp.create File ~prefix:"gc_stat" ~suffix:name in
  let open Fiber.O in
  (* Build with timings and gc stats *)
  let+ times =
    Process.run_with_times
      Strict
      dune
      ~display:Quiet
      ~stdin_from
      ~stdout_to
      ~stderr_to
      ([ "build"
       ; "@install"
       ; "--release"
       ; "--cache" (* explicitly disable cache *)
       ; "disabled"
       ; "--dump-gc-stats"
       ; Path.to_string gc_dump
       ]
       @
       match sandbox with
       | `Yes -> [ "--sandbox"; "hardlink" ]
       | `No -> [])
  in
  (* Read the gc stats from the dump file *)
  Dune_lang.Parser.parse_string
    ~mode:Single
    ~fname:(Path.to_string gc_dump)
    (Io.read_file gc_dump)
  |> Dune_lang.Decoder.parse Dune_util.Gc.decode Univ_map.empty
  |> Metrics.make times
;;

let run_bench ~sandbox =
  let open Fiber.O in
  let* clean = dune_build ~name:"clean" ~sandbox in
  let+ zero =
    let rec zero acc n =
      if n = 0
      then Fiber.return (List.rev acc)
      else
        let* time = dune_build ~name:("zero" ^ string_of_int n) ~sandbox in
        zero (time :: acc) (pred n)
    in
    zero [] 5
  in
  clean, zero
;;

type ('float, 'int) bench_results =
  { size : int
  ; clean : ('float, 'int) Metrics.t
  ; zero : ('float, 'int) Metrics.t list
  }

let tag_results { size; clean; zero } =
  let tag data = Metrics.map ~f:(fun t -> `Float t) ~g:(fun t -> `Int t) data in
  let list_tag data =
    List.map data ~f:tag
    |> Metrics.unzip
    |> Metrics.map ~f:(fun x -> `List x) ~g:(fun x -> `List x)
  in
  `Int size, tag clean, list_tag zero
;;

(** Display all clean and null builds with a few exceptions:

    - fragments - not consistent between builds
    - stack_size - not very useful
    - forced_collections - only available in OCaml >= 4.12 *)
let display_clean_and_zero_with_sandboxing
  ({ elapsed_time
   ; user_cpu_time
   ; system_cpu_time
   ; minor_words
   ; promoted_words
   ; major_words
   ; minor_collections
   ; major_collections
   ; heap_words
   ; heap_chunks
   ; live_words
   ; live_blocks
   ; free_words
   ; free_blocks
   ; largest_free
   ; fragments = _
   ; compactions
   ; top_heap_words
   ; stack_size = _
   } :
    _ Metrics.t)
  (zero : _ Metrics.t)
  =
  let display what units clean zero =
    { Output.name = what
    ; metrics = [ "[Clean] " ^ what, clean, units; "[Null] " ^ what, zero, units ]
    }
  in
  [ display "Build Time" "Seconds" elapsed_time zero.elapsed_time
  ; display "Minor Words" "Approx. Words" minor_words zero.minor_words
  ; display "Promoted Words" "Approx. Words" promoted_words zero.promoted_words
  ; display "Major Words" "Approx. Words" major_words zero.major_words
  ; display "Minor Collections" "Collections" minor_collections zero.minor_collections
  ; display "Major Collections" "Collections" major_collections zero.major_collections
  ; display "Heap Words" "Words" heap_words zero.heap_words
  ; display "Heap Chunks" "Chunks" heap_chunks zero.heap_chunks
  ; display "Live Words" "Words" live_words zero.live_words
  ; display "Live Blocks" "Blocks" live_blocks zero.live_blocks
  ; display "Free Words" "Words" free_words zero.free_words
  ; display "Free Blocks" "Blocks" free_blocks zero.free_blocks
  ; display "Largest Free" "Words" largest_free zero.largest_free
  ; display "Compactions" "Compactions" compactions zero.compactions
  ; display "Top Heap Words" "Words" top_heap_words zero.top_heap_words
  ; display "User CPU Time" "Seconds" user_cpu_time zero.user_cpu_time
  ; display "System CPU Time" "Seconds" system_cpu_time zero.system_cpu_time
  ]
;;

let format_results bench_results =
  (* tagging data for json conversion *)
  let size, clean, zero = tag_results bench_results in
  (* bench results *)
  [ { Output.name = "Misc"; metrics = [ "Size of _boot/dune.exe", size, "Bytes" ] } ]
  @ display_clean_and_zero_with_sandboxing clean zero
;;

let () =
  Dune_util.Log.init ~file:No_log_file ();
  let dir = Temp.create Dir ~prefix:"dune" ~suffix:"bench" in
  Sys.chdir (Path.to_string dir);
  Path.as_external dir |> Option.value_exn |> Path.set_root;
  Path.Build.set_build_dir (Path.Outside_build_dir.of_string "_build");
  let module Scheduler = Dune_engine.Scheduler in
  let config =
    Dune_engine.Clflags.display := Quiet;
    { Scheduler.Config.concurrency = 10
    ; stats = None
    ; print_ctrl_c_warning = false
    ; watch_exclusions = []
    }
  in
  let size =
    let stat : Unix.stats = Path.stat_exn dune in
    stat.st_size
  in
  let results =
    Scheduler.Run.go config ~on_event:(fun _ _ -> ())
    @@ fun () ->
    let open Fiber.O in
    (* Prepare the workspace *)
    let* () = prepare_workspace () in
    (* Build the clean and null builds *)
    Console.printf "Building clean and null builds";
    let+ clean, zero = run_bench ~sandbox:`No in
    Console.printf "Finished building clean and null builds";
    (* Return the bench results *)
    format_results { size; clean; zero }
  in
  let version = 4 in
  let output = { Output.config = []; version; results } in
  print_string (Json.to_string (Output.to_json output));
  flush stdout
;;
