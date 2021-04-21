open Stdune

module Json = struct
  include Chrome_trace.Json
  include Stats.Json
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

let dune =
  lazy
    (let path =
       Env.get Env.initial "PATH" |> Option.value_exn |> Bin.parse_path
     in
     Bin.which ~path "dune" |> Option.value_exn)

module Package = struct
  type t =
    { org : string
    ; name : string
    }

  let uri { org; name } = sprintf "https://github.com/%s/%s" org name

  let make org name = { org; name }

  let clone t =
    let module Process = Dune_engine.Process in
    let module Config = Dune_util.Config in
    let stdout_to = Process.Io.(file Config.dev_null Out) in
    let stderr_to = Process.Io.(file Config.dev_null Out) in
    let stdin_from = Process.Io.(null In) in
    Process.run Strict ~stdout_to ~stderr_to ~stdin_from (Lazy.force git)
      [ "clone"; uri t ]
end

let duniverse =
  let pkg = Package.make in
  [ pkg "ocaml-dune" "dune-bench" ]

let prepare_workspace () =
  let module Scheduler = Dune_engine.Scheduler in
  let config =
    { Scheduler.Config.concurrency = 10
    ; display = Quiet
    ; rpc = None
    ; stats = None
    }
  in
  Scheduler.Run.go config
    ~on_event:(fun _ _ -> ())
    (fun () ->
      Fiber.parallel_iter duniverse ~f:(fun (pkg : Package.t) ->
          Fpath.rm_rf pkg.name;
          Format.eprintf "cloning %s/%s@." pkg.org pkg.name;
          Package.clone pkg))

let with_timer f =
  let start = Unix.time () in
  let res = f () in
  let stop = Unix.time () in
  (stop -. start, res)

let () =
  Dune_util.Log.init ~file:No_log_file ();
  let dir = Temp.create Dir ~prefix:"dune." ~suffix:".bench" in
  Sys.chdir (Path.to_string dir);
  prepare_workspace ();
  let clean, _ =
    with_timer (fun () -> Sys.command "dune build @install --root . 1>&2")
  in
  let zeros =
    List.init 5 ~f:(fun _ ->
        let time, _ =
          with_timer (fun () -> Sys.command "dune build @install --root . 1>&2")
        in
        `Float time)
  in
  let size =
    let stat : Unix.stats = Path.stat_exn (Lazy.force dune) in
    stat.st_size
  in
  let results =
    [ { Output.name = "clean_build"; metrics = [ ("time", `Float clean) ] }
    ; { Output.name = "zero_build"; metrics = [ ("time", `List zeros) ] }
    ; { Output.name = "dune_size"; metrics = [ ("size", `Int size) ] }
    ]
  in
  let output = { Output.config = []; results } in
  print_string (Json.to_string (Output.to_json output));
  flush stdout
