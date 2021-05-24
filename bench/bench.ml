open Stdune
module Process = Dune_engine.Process
module Config = Dune_util.Config

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
    ; metrics : (string * [ measurement | `List of measurement list ]) list
    }

  let json_of_bench { name; metrics } : Json.t =
    let metrics =
      List.map metrics ~f:(fun (name, value) ->
          let value =
            match value with
            | `Int i -> `Int i
            | `Float f -> `Float f
            | `List xs -> `List (xs :> Json.t list)
          in
          (name, value))
    in
    `Assoc [ ("name", `String name); ("metrics", `Assoc metrics) ]

  type t =
    { config : (string * Json.t) list
    ; results : bench list
    }

  let to_json { config; results } : Json.t =
    let assoc = [ ("results", `List (List.map results ~f:json_of_bench)) ] in
    let assoc =
      match config with
      | [] -> assoc
      | _ :: _ -> ("config", `Assoc config) :: assoc
    in
    `Assoc assoc
end

let git =
  lazy
    (let path =
       Env.get Env.initial "PATH" |> Option.value_exn |> Bin.parse_path
     in
     Bin.which ~path "git" |> Option.value_exn)

let dune = Path.of_string (Filename.concat Fpath.initial_cwd Sys.argv.(1))

module Package = struct
  type t =
    { org : string
    ; name : string
    }

  let uri { org; name } = sprintf "https://github.com/%s/%s" org name

  let make org name = { org; name }

  let clone t =
    let stdout_to = Process.Io.make_stdout Swallow in
    let stderr_to = Process.Io.make_stderr Swallow in
    let stdin_from = Process.Io.(null In) in
    Process.run Strict ~stdout_to ~stderr_to ~stdin_from (Lazy.force git)
      [ "clone"; uri t ]
end

let duniverse =
  let pkg = Package.make in
  [ pkg "ocaml-dune" "dune-bench" ]

let prepare_workspace () =
  Fiber.parallel_iter duniverse ~f:(fun (pkg : Package.t) ->
      Fpath.rm_rf pkg.name;
      Format.eprintf "cloning %s/%s@." pkg.org pkg.name;
      Package.clone pkg)

let dune_build () =
  let stdin_from = Process.(Io.null In) in
  let stdout_to = Process.Io.make_stdout Swallow in
  let stderr_to = Process.Io.make_stderr Swallow in
  let open Fiber.O in
  let+ times =
    Process.run_with_times dune ~stdin_from ~stdout_to ~stderr_to
      [ "build"; "@install"; "--root"; "." ]
  in
  times.elapsed_time

let run_bench () =
  let open Fiber.O in
  let* clean = dune_build () in
  let+ zero =
    let open Fiber.O in
    let rec zero acc n =
      if n = 0 then
        Fiber.return (List.rev acc)
      else
        let* time = dune_build () in
        zero (time :: acc) (pred n)
    in
    zero [] 5
  in
  (clean, zero)

let () =
  Dune_util.Log.init ~file:No_log_file ();
  let dir = Temp.create Dir ~prefix:"dune" ~suffix:"bench" in
  Sys.chdir (Path.to_string dir);
  let module Scheduler = Dune_engine.Scheduler in
  let config =
    { Scheduler.Config.concurrency = 10
    ; display = { verbosity = Quiet; status_line = false }
    ; rpc = None
    ; stats = None
    }
  in
  let clean, zero =
    Scheduler.Run.go config
      ~on_event:(fun _ _ -> ())
      (fun () ->
        let open Fiber.O in
        let* () = prepare_workspace () in
        run_bench ())
  in
  let zero = List.map zero ~f:(fun t -> `Float t) in
  let size =
    let stat : Unix.stats = Path.stat_exn dune in
    stat.st_size
  in
  let results =
    [ { Output.name = "clean_build"; metrics = [ ("time", `Float clean) ] }
    ; { Output.name = "zero_build"; metrics = [ ("time", `List zero) ] }
    ; { Output.name = "dune_size"; metrics = [ ("size", `Int size) ] }
    ]
  in
  let output = { Output.config = []; results } in
  print_string (Json.to_string (Output.to_json output));
  flush stdout
