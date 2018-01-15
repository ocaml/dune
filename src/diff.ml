open Import

let ( >>= ) = Future.( >>= )

let print file1 file2 =
  let loc = Loc.in_file (Path.to_string file1) in
  let fallback () =
    die "%aFiles \"%s\" and \"%s\" differ." Loc.print loc
      (Path.to_string file1) (Path.to_string file2)
  in
  let normal_diff () =
    match Bin.which "diff" with
    | None -> fallback ()
    | Some prog ->
      Format.eprintf "%a@?" Loc.print loc;
      Future.run Strict (Path.to_string prog)
        ["-u"; Path.to_string file1; Path.to_string file2]
      >>= fun () ->
      die "diff reported no differences on \"%s\" and \"%s\""
        (Path.to_string file1) (Path.to_string file2)
  in
  match !Clflags.diff_command with
  | Some cmd ->
    let sh, arg = Utils.system_shell_exn ~needed_to:"print diffs" in
    let q fn = Filename.quote (Path.to_string fn) in
    let cmd = sprintf "%s %s %s" cmd (q file1) (q file2) in
    Future.run Strict (Path.to_string sh) [arg; cmd]
    >>= fun () ->
    die "command reported no differences: %s" cmd
  | None ->
    match Bin.which "patdiff" with
    | None -> normal_diff ()
    | Some prog ->
      Future.run Strict (Path.to_string prog)
        [ "-keep-whitespace"
        ; "-location-style"; "omake"
        ; "-unrefined"
        ; Path.to_string file1
        ; Path.to_string file2
        ]
      >>= fun () ->
      (* Use "diff" if "patdiff" reported no differences *)
      normal_diff ()
