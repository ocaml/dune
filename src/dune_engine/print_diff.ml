open! Stdune
open Import
open Fiber.O

(* [git diff] doesn't like symlink arguments *)
let resolve_link_for_git path =
  match Path.follow_symlink path with
  | Ok p -> p
  | Error Not_a_symlink -> path
  | Error Max_depth_exceeded ->
    User_error.raise
      [ Pp.textf "Unable to resolve symlink %s. Max recursion depth exceeded"
          (Path.to_string path)
      ]
  | Error (Unix_error _) ->
    User_error.raise
      [ Pp.textf "Unable to resolve symlink %s" (Path.to_string path) ]

let print ?(skip_trailing_cr = Sys.win32) annots path1 path2 =
  let dir, file1, file2 =
    match
      ( Path.extract_build_context_dir_maybe_sandboxed path1
      , Path.extract_build_context_dir_maybe_sandboxed path2 )
    with
    | Some (dir1, f1), Some (dir2, f2) when Path.equal dir1 dir2 ->
      (dir1, Path.source f1, Path.source f2)
    | _ -> (Path.root, path1, path2)
  in
  let loc = Loc.in_file file1 in
  let run_process ?(dir = dir)
      ?(metadata =
        Process.create_metadata ~purpose:Internal_job ~loc ~annots ()) prog args
      =
    Process.run ~dir ~env:Env.initial Strict prog args ~metadata
  in
  let file1, file2 = Path.(to_string file1, to_string file2) in
  let fallback () =
    User_error.raise ~loc ~annots
      [ Pp.textf "Files %s and %s differ."
          (Path.to_string_maybe_quoted (Path.drop_optional_sandbox_root path1))
          (Path.to_string_maybe_quoted (Path.drop_optional_sandbox_root path2))
      ]
  in
  let normal_diff () =
    let dir, path, args, skip_trailing_cr_arg, files =
      let which prog = Bin.which ~path:(Env.path Env.initial) prog in
      match which "git" with
      | Some path ->
        let dir =
          (* We can't run [git] from [dir] as [dir] might be inside a sandbox
             and sandboxes have fake [.git] files to stop [git] from escaping
             the sandbox. If we did, the below git command would fail saying it
             can run this fake [.git] file. *)
          Path.root
        in
        ( dir
        , path
        , [ "--no-pager"; "diff"; "--no-index"; "--color=always"; "-u" ]
        , "--ignore-cr-at-eol"
        , List.map
            ~f:(fun path -> resolve_link_for_git path |> Path.reach ~from:dir)
            [ path1; path2 ] )
      | None -> (
        match which "diff" with
        | Some path ->
          (dir, path, [ "-u" ], "--strip-trailing-cr", [ file1; file2 ])
        | None -> fallback ())
    in
    let args =
      if skip_trailing_cr then args @ [ skip_trailing_cr_arg ] else args
    in
    let args = args @ files in
    let* () = run_process ~dir path args in
    fallback ()
  in
  match !Clflags.diff_command with
  | Some "-" -> fallback ()
  | Some cmd ->
    let sh, arg = Utils.system_shell_exn ~needed_to:"print diffs" in
    let cmd =
      sprintf "%s %s %s" cmd
        (String.quote_for_shell file1)
        (String.quote_for_shell file2)
    in
    let* () = run_process sh [ arg; cmd ] in
    User_error.raise ~loc ~annots
      [ Pp.textf "command reported no differences: %s"
          (if Path.is_root dir then cmd
          else
            sprintf "cd %s && %s"
              (String.quote_for_shell (Path.to_string dir))
              cmd)
      ]
  | None -> (
    if Config.inside_dune then fallback ()
    else
      match Bin.which ~path:(Env.path Env.initial) "patdiff" with
      | None -> normal_diff ()
      | Some prog ->
        let* () =
          run_process prog
            ([ "-keep-whitespace"; "-location-style"; "omake" ]
            @ (if Lazy.force Ansi_color.stderr_supports_color then []
              else [ "-ascii" ])
            @ [ file1; file2 ])
            ~metadata:
              ((* Because of the [-location-style omake], patdiff will print the
                  location of each hunk in a format that the editor should
                  understand. However, the location won't be the first line of
                  the output, so the [process] module won't recognise that the
                  output has a location.

                  For this reason, we manually pass the below annotation. *)
               Process.create_metadata ~purpose:Internal_job ~loc
                 ~annots:
                   (User_message.Annots.set annots
                      User_message.Annots.has_embedded_location ())
                 ())
        in
        (* Use "diff" if "patdiff" reported no differences *)
        normal_diff ())
