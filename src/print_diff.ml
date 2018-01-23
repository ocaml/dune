open Import

open Fiber.O

let print path1 path2 =
  let dir, file1, file2 =
    match
      Path.extract_build_context_dir path1,
      Path.extract_build_context_dir path2
    with
    | Some (dir1, f1), Some (dir2, f2) when dir1 = dir2 ->
      (Path.to_string dir1, Path.to_string f1, Path.to_string f2)
    | _ ->
      (".", Path.to_string path1, Path.to_string path2)
  in
  let loc = Loc.in_file file1 in
  let fallback () =
    die "%aFiles %s and %s differ." Loc.print loc
      (Path.to_string_maybe_quoted path1)
      (Path.to_string_maybe_quoted path2)
  in
  let normal_diff () =
    match Bin.which "diff" with
    | None -> fallback ()
    | Some prog ->
      Format.eprintf "%a@?" Loc.print loc;
      Process.run ~dir Strict (Path.to_string prog)
        ["-u"; file1; file2]
      >>= fun () ->
      fallback ()
  in
  match !Clflags.diff_command with
  | Some cmd ->
    let sh, arg = Utils.system_shell_exn ~needed_to:"print diffs" in
    let cmd =
      sprintf "%s %s %s" cmd (quote_for_shell file1) (quote_for_shell file2)
    in
    Process.run ~dir Strict (Path.to_string sh) [arg; cmd]
    >>= fun () ->
    die "command reported no differences: %s"
      (if dir = "." then
         cmd
       else
         sprintf "cd %s && %s" (quote_for_shell dir) cmd)
  | None ->
    match Bin.which "patdiff" with
    | None -> normal_diff ()
    | Some prog ->
      Process.run ~dir Strict (Path.to_string prog)
        [ "-keep-whitespace"
        ; "-location-style"; "omake"
        ; "-unrefined"
        ; file1
        ; file2
        ]
      >>= fun () ->
      (* Use "diff" if "patdiff" reported no differences *)
      normal_diff ()
