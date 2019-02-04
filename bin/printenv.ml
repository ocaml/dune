open Stdune
open Import
open Fiber.O

let doc = "Print the environment of a directory"

let man =
  [ `S "DESCRIPTION"
  ; `P {|$(b,dune printenv DIR) prints the environment of a directory|}
  ; `Blocks Common.help_secs
  ]

let info = Term.info "printenv" ~doc ~man

let dump sctx ~dir =
  let open Build.O in
  Super_context.dump_env sctx ~dir
  >>^ fun env ->
  ((Super_context.context sctx).name, env)

let term =
  let%map common = Common.term
  and dir = Arg.(value & pos 0 dir "" & info [] ~docv:"PATH")
  in
  Common.set_common common ~targets:[];
  let log = Log.create common in
  Scheduler.go ~log ~common (fun () ->
    Import.Main.setup ~log common >>= fun setup ->
    let dir = Path.of_string dir in
    Util.check_path setup.workspace.contexts dir;
    let request =
      Build.all (
        match Path.extract_build_context dir with
        | Some (ctx, _) ->
          let sctx = String.Map.find_exn setup.scontexts ctx in
          [dump sctx ~dir]
        | None ->
          String.Map.values setup.scontexts
          |> List.map ~f:(fun sctx ->
            let dir =
              Path.append (Super_context.context sctx).build_dir dir
            in
            dump sctx ~dir)
      )
    in
    Build_system.do_build ~request
    >>| fun l ->
    let pp ppf = Format.fprintf ppf "@[<v1>(@,@[<v>%a@]@]@,)"
                   (Format.pp_print_list (Dune_lang.pp Dune)) in
    match l with
    | [(_, env)] ->
      Format.printf "%a@." pp env
    | l ->
      List.iter l ~f:(fun (name, env) ->
        Format.printf "@[<v2>Environment for context %s:@,%a@]@." name pp env))

let command = term, info
