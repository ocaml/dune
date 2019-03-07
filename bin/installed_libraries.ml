open Stdune
open Import
open Fiber.O

let doc = "Print out libraries installed on the system."

let info = Term.info "installed-libraries" ~doc

let term =
  let+ common = Common.term
  and+ na =
    Arg.(value
         & flag
         & info ["na"; "not-available"]
             ~doc:"List libraries that are not available and explain why")
  in
  Common.set_common common ~targets:[];
  let env = Import.Main.setup_env ~capture_outputs:common.capture_outputs in
  Scheduler.go ~log:(Log.create common) ~common (fun () ->
    Context.create ~env
      { merlin_context = Some "default"
      ; contexts = [Default { loc = Loc.of_pos __POS__
                            ; targets   = [Native]
                            ; profile   = Config.default_build_profile
                            ; env       = None
                            ; toolchain = None
                            }]
      ; env = None
      }
    >>= fun ctxs ->
    let ctx = List.hd ctxs in
    let findlib = ctx.findlib in
    if na then begin
      let pkgs = Findlib.all_unavailable_packages findlib in
      let longest =
        String.longest_map pkgs ~f:(fun (n, _) -> Lib_name.to_string n) in
      let ppf = Format.std_formatter in
      List.iter pkgs ~f:(fun (n, r) ->
        Format.fprintf ppf "%-*s -> %a@\n" longest (Lib_name.to_string n)
          Findlib.Unavailable_reason.pp r);
      Format.pp_print_flush ppf ();
      Fiber.return ()
    end else begin
      let pkgs = Findlib.all_packages findlib in
      let max_len =
        String.longest_map pkgs ~f:(fun (n : _ Dune_package.Lib.t) ->
          Lib_name.to_string (Dune_package.Lib.name n)) in
      List.iter pkgs ~f:(fun (pkg : _ Dune_package.Lib.t) ->
        let ver =
          Option.value (Dune_package.Lib.version pkg) ~default:"n/a"
        in
        Printf.printf "%-*s (version: %s)\n" max_len
          (Lib_name.to_string (Dune_package.Lib.name pkg)) ver);
      Fiber.return ()
    end)

let command = term, info
