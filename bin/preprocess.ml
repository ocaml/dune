open Stdune
open Import

let doc = "Print the preprocessed output"

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune preprocess FILE) build the given FILE and print the preprocessed output|}
  ; `Blocks Common.help_secs
  ]

let info = Term.info "preprocess" ~doc ~man

let change_ext f path =
  let base, ext = Path.of_string path |> Path.split_extension in
  let suffix = f ext in
  Path.extend_basename base ~suffix |> Path.to_string

let pp_with_ocamlc sctx pp_ml =
  let ocamlc = (Super_context.context sctx).ocamlc |> Path.to_string in
  let env = Super_context.context_env sctx |> Env.to_unix |> Array.of_list in
  let argv =
    Array.of_list [ocamlc; "-stop-after"; "parsing"; "-dsource"; pp_ml; "-dump-into-file"]
  in
  let pid =
    Unix.create_process_env ocamlc argv env Unix.stdin Unix.stdout Unix.stderr
  in
  match Unix.waitpid [] pid |> snd with
  | Unix.WEXITED 0 ->
    let dump_file =
      pp_ml |> change_ext (function
        | ".mli" -> ".cmi.dump"
        | ".ml"  -> ".cmo.dump"
        | _ -> failwith "unreachable")
    in
    if not (Path.of_string dump_file |> Path.is_file) then
      User_error.raise [ Pp.textf "cannot find a dump file: %s" dump_file ]
    else
      In_channel.with_open_text dump_file (fun ic -> Io.copy_channels ic stdout);
      Sys.remove dump_file;
      exit 0
  | Unix.WEXITED n -> exit n
  | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> exit 255

let term =
  let+ common = Common.term
  and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE")
  and+ ctx_name =
    Common.context_arg ~doc:{|Select context where to build.|} in
  let config = Common.init common in
  let file = Common.prefix_target common file in
  if String.is_empty file then
    User_error.raise [ Pp.textf "no file is given" ];
  let pp_file =
    file |> change_ext (function
      | ".mli" -> ".pp.mli"
      | ".ml"  -> ".pp.ml"
      | _ -> User_error.raise [ Pp.textf "unsupported file: %s" file ])
  in
  let sctx, path =
    Scheduler.go ~common ~config (fun () ->
      let open Fiber.O in
      let* setup = Import.Main.setup () in
      Build_system.run_exn (fun () ->
        let open Memo.O in
        let* setup = setup in
        let context = Import.Main.find_context_exn setup ~name:ctx_name in
        let sctx = Import.Main.find_scontext_exn setup ~name:ctx_name in
        let target = Path.build (Path.Build.relative context.build_dir pp_file) in
        Build_system.file_exists target >>= function
        | false ->
          User_error.raise
            [ Pp.textf "%s is not preprocessed" (String.maybe_quoted file) ]
        | true ->
          let+ _digest = Build_system.build_file target in
          (sctx, Path.to_string target)))
  in
  Hooks.End_of_build.run ();
  pp_with_ocamlc sctx path

let command = (term, info)