open Dune_memory
open Stdune

let parse_metadata s =
  Dune_lang.Parser.parse_string s ~fname:"<command-line>" ~mode:Many
  |> List.map ~f:Dune_lang.Ast.remove_locs

let usage =
  Printf.sprintf "Usage: %s [OPTIONS] command [ARGUMENTS]" Sys.argv.(0)

let lift_result = function
  | Result.Ok r -> r
  | Result.Error e -> User_error.raise [ Pp.textf "%s" e ]

let fill_option name ref s =
  match !ref with
  | None -> ref := Some s
  | Some _ -> User_error.raise [ Pp.textf "duplicate option %s" name ]

let unwrap_option ?default o = function
  | None -> (
    match default with
    | None -> User_error.raise [ Pp.textf "missing required argument: %s" o ]
    | Some v -> v )
  | Some v -> v

let main () =
  let root = ref None
  and cmd = ref None
  and current = ref (Array.length Sys.argv) in
  Arg.parse_argv Sys.argv
    [ ("--root", Arg.String (fill_option "--root" root), "root directory") ]
    (fun a ->
      if !cmd = None then
        cmd := Some a
      else if !current = Array.length Sys.argv then
        current := !Arg.current - 1)
    usage;
  let root = Option.map ~f:Path.of_string !root
  and cmd = unwrap_option "command" !cmd in
  Console.init Quiet;
  Log.init ~file:(This (Path.of_string "/tmp/log")) ();
  let memory = lift_result (make ?root ()) in
  match cmd with
  | "promote" ->
    let usage = ""
    and metadata = ref None
    and key = ref None
    and files = ref (Array.make 0 "") in
    Arg.parse_argv ?current:(Some current) Sys.argv
      [ ( "--metadata"
        , Arg.String (fill_option "--metadata" metadata)
        , "metadata" )
      ; ("--key", Arg.String (fill_option "--key" key), "key")
      ]
      (fun f -> files := Array.append !files (Array.make 1 f))
      usage;
    let open Result.O in
    let produced =
      Array.to_list
        (Array.map
           ~f:(fun p ->
             let p = Path.of_string p in
             (p, Digest.file p))
           !files)
    and key = unwrap_option "key" !key in
    lift_result
      (let metadata =
         parse_metadata (unwrap_option ~default:"()" "--metadata" !metadata)
       in
       key_of_string key
       >>= fun key ->
       Memory.promote memory produced key metadata None
       >>| fun promotions ->
       List.iter
         ~f:(fun p -> Printf.printf "%s\n" (promotion_to_string p))
         promotions)
  | "search" ->
    lift_result
      (let open Result.O in
      let+ key = key_of_string Sys.argv.(3) in
      match Memory.search memory key with
      | Found (_, paths) ->
        List.iter
          ~f:
            (fun { Dune_memory.File.in_the_build_directory
                 ; in_the_memory
                 ; digest
                 } ->
            Printf.printf "%s: %s (%s)\n"
              (Path.to_string in_the_build_directory)
              (Path.to_string in_the_memory)
              (Digest.to_string digest))
          paths
      | Not_found ->
        User_error.raise [ Pp.text "Entry not found in the dune memory" ]
      | Cannot_read exn -> raise exn)
  | "trim" ->
    let freed, files = trim memory 1 in
    Printf.printf "freed %i bytes\n" freed;
    List.iter ~f:(fun p -> Printf.printf "%s\n" (Path.to_string p)) files
  | _ -> User_error.raise [ Pp.textf "unkown command: %s" cmd ]

let () =
  try main () with
  | User_error.E msg ->
    Printf.fprintf stderr "%s: user error: %s\n" Sys.argv.(0)
      (Format.asprintf "%a@?" Pp.render_ignore_tags (User_message.pp msg));
    exit 1
  | Sys_error msg ->
    Printf.fprintf stderr "%s: fatal error: %s\n" Sys.argv.(0) msg;
    exit 2
