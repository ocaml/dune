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

module Diff = struct
  type t =
    { loc : Loc.t option
    ; output : string
    }

  let print { loc; output } =
    Option.iter loc ~f:(fun loc ->
        Loc.pp loc |> Pp.map_tags ~f:(fun Loc.Loc -> []) |> Ansi_color.print);
    print_string output
end

type command =
  { dir : Path.t
  ; metadata : Process.metadata
  ; prog : Path.t
  ; args : string list
  }

module With_fallback : sig
  type t

  val fail : User_message.t -> t

  val run : command -> fallback:t -> t

  val exec : t -> _ Fiber.t

  val capture : t -> (Diff.t, User_message.t) result Fiber.t
end = struct
  type t =
    { commands : command list
    ; error : User_message.t
    }

  let run command ~fallback:{ commands; error } =
    { commands = command :: commands; error }

  let fail error = { commands = []; error }

  let rec exec = function
    | { commands = []; error } -> raise (User_error.E error)
    | { commands = { dir; metadata; prog; args } :: commands; error } ->
      let* () =
        Process.run ~display:Quiet ~dir ~env:Env.initial Strict prog args
          ~metadata
      in
      exec { commands; error }

  let rec capture = function
    | { commands = []; error } -> Fiber.return (Error error)
    | { commands = { dir; metadata; prog; args } :: commands; error } -> (
      let* output, code =
        Process.run_capture ~display:Quiet ~dir ~env:Env.initial Return prog
          args ~metadata
      in
      match code with
      | 1 -> Fiber.return (Ok { Diff.output; loc = metadata.loc })
      | _ -> capture { commands; error })
end

let prepare ~skip_trailing_cr annots path1 path2 =
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
  let run ?(dir = dir)
      ?(metadata =
        Process.create_metadata ~purpose:Internal_job ~loc ~annots ()) prog args
      ~fallback =
    With_fallback.run { dir; prog; args; metadata } ~fallback
  in
  let file1, file2 = Path.(to_string file1, to_string file2) in
  let fallback =
    With_fallback.fail
      (User_error.make ~loc ~annots
         [ Pp.textf "Files %s and %s differ."
             (Path.to_string_maybe_quoted
                (Path.drop_optional_sandbox_root path1))
             (Path.to_string_maybe_quoted
                (Path.drop_optional_sandbox_root path2))
         ])
  in
  let normal_diff () =
    let diff =
      let which prog = Bin.which ~path:(Env_path.path Env.initial) prog in
      match which "git" with
      | Some path ->
        let dir =
          (* We can't run [git] from [dir] as [dir] might be inside a sandbox
             and sandboxes have fake [.git] files to stop [git] from escaping
             the sandbox. If we did, the below git command would fail saying it
             can run this fake [.git] file. *)
          Path.root
        in
        Some
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
          Some (dir, path, [ "-u" ], "--strip-trailing-cr", [ file1; file2 ])
        | None -> None)
    in
    match diff with
    | None -> fallback
    | Some (dir, path, args, skip_trailing_cr_arg, files) ->
      let args =
        if skip_trailing_cr then args @ [ skip_trailing_cr_arg ] else args
      in
      let args = args @ files in
      run ~dir path args ~fallback
  in
  match !Clflags.diff_command with
  | Some "-" -> fallback
  | Some cmd ->
    let sh, arg = Utils.system_shell_exn () ~needed_to:"print diffs" in
    let cmd =
      sprintf "%s %s %s" cmd
        (String.quote_for_shell file1)
        (String.quote_for_shell file2)
    in
    run sh [ arg; cmd ]
      ~fallback:
        (With_fallback.fail
           (User_error.make ~loc ~annots
              [ Pp.textf "command reported no differences: %s"
                  (if Path.is_root dir then cmd
                   else
                     sprintf "cd %s && %s"
                       (String.quote_for_shell (Path.to_string dir))
                       cmd)
              ]))
  | None -> (
    if Execution_env.inside_dune then fallback
    else
      match Bin.which ~path:(Env_path.path Env.initial) "patdiff" with
      | None -> normal_diff ()
      | Some prog ->
        run prog
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
          ~fallback:
            ((* Use "diff" if "patdiff" reported no differences *)
             normal_diff ()))

let print ?(skip_trailing_cr = Sys.win32) annots path1 path2 =
  let p = prepare ~skip_trailing_cr annots path1 path2 in
  With_fallback.exec p

let get ?(skip_trailing_cr = Sys.win32) annots path1 path2 =
  let p = prepare ~skip_trailing_cr annots path1 path2 in
  With_fallback.capture p
