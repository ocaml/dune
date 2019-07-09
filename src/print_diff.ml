open! Stdune
open Import

open Fiber.O

let print ?(skip_trailing_cr=Sys.win32) path1 path2 =
  let dir, file1, file2 =
    match
      Path.extract_build_context_dir path1,
      Path.extract_build_context_dir path2
    with
    | Some (dir1, f1), Some (dir2, f2) when Path.equal dir1 dir2 ->
      (dir1, Path.source f1, Path.source f2)
    | _ ->
      (Path.root, path1, path2)
  in
  let loc = Loc.in_file file1 in
  let (file1, file2) = Path.(to_string file1, to_string file2) in
  let fallback () =
    User_error.raise ~loc
      [ Pp.textf "Files %s and %s differ."
          (Path.to_string_maybe_quoted path1)
          (Path.to_string_maybe_quoted path2)
      ]
  in
  let normal_diff () =
    let which bin = Option.map (Bin.which ~path:(Env.path Env.initial) bin) ~f:(fun path -> path, bin) in
    let (|||) a b = if a = None then b else a in
    match which "git" ||| which "diff" with
    | None -> fallback ()
    | Some (prog, bin) ->
      Format.eprintf "%a@?" Loc.print loc;
      let* () =
        Process.run ~dir ~env:Env.initial Strict prog
          (List.concat
             [ if bin = "git" then ["diff"; "--no-index"; "--color=always"] else []
             ; ["-u"]
             ; if skip_trailing_cr then ["--strip-trailing-cr"] else []
             ; [ file1; file2 ]
             ])
      in
      fallback ()
  in
  match !Clflags.diff_command with
  | Some "-" -> fallback ()
  | Some cmd ->
    let sh, arg = Utils.system_shell_exn ~needed_to:"print diffs" in
    let cmd =
      sprintf "%s %s %s" cmd (String.quote_for_shell file1) (String.quote_for_shell file2)
    in
    let* () = Process.run ~dir ~env:Env.initial Strict sh [arg; cmd] in
    User_error.raise
      [ Pp.textf "command reported no differences: %s"
          (if Path.is_root dir then
             cmd
           else
             sprintf "cd %s && %s" (String.quote_for_shell (Path.to_string dir)) cmd) ]
  | None ->
    if Config.inside_dune then
      fallback ()
    else
      match Bin.which ~path:(Env.path Env.initial) "patdiff" with
      | None -> normal_diff ()
      | Some prog ->
        let* () =
          Process.run ~dir ~env:Env.initial Strict prog
            [ "-keep-whitespace"
            ; "-location-style"; "omake"
            ; if Lazy.force Ansi_color.stderr_supports_color then
                "-unrefined"
              else
                "-ascii"
            ; file1
            ; file2
            ]
        in
        (* Use "diff" if "patdiff" reported no differences *)
        normal_diff ()
