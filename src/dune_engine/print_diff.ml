open Import
open Fiber.O
module Process = Process

(* [git diff] doesn't like symlink arguments *)
let resolve_link_for_git path =
  match Fpath.follow_symlink (Path.to_string path) with
  | Ok p -> Path.of_string p
  | Error Not_a_symlink -> path
  | Error Max_depth_exceeded ->
    User_error.raise
      [ Pp.textf
          "Unable to resolve symlink %s. Max recursion depth exceeded"
          (Path.to_string path)
      ]
  | Error (Unix_error _) ->
    User_error.raise [ Pp.textf "Unable to resolve symlink %s" (Path.to_string path) ]
;;

module Diff = struct
  type t =
    { loc : Loc.t option
    ; output : string
    }

  let print { loc; output } =
    Option.iter loc ~f:(fun loc ->
      Loc.pp loc |> Pp.map_tags ~f:(fun Loc.Loc -> []) |> Ansi_color.print);
    print_string output
  ;;
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
  ;;

  let fail error = { commands = []; error }

  let rec exec = function
    | { commands = []; error } -> raise (User_error.E error)
    | { commands = { dir; metadata; prog; args } :: commands; error } ->
      let* () =
        Process.run ~display:Quiet ~dir ~env:Env.initial Strict prog args ~metadata
      in
      exec { commands; error }
  ;;

  let rec capture = function
    | { commands = []; error } -> Fiber.return (Error error)
    | { commands = { dir; metadata; prog; args } :: commands; error } ->
      let* output, code =
        Process.run_capture
          ~display:Quiet
          ~dir
          ~env:Env.initial
          Return
          prog
          args
          ~metadata
      in
      (match code with
       | 1 -> Fiber.return (Ok { Diff.output; loc = metadata.loc })
       | _ -> capture { commands; error })
  ;;
end

let make_metadata ~has_embedded_location annots loc =
  Process.create_metadata
    ~categories:[ "diff" ]
    ~has_embedded_location
    ~purpose:Internal_job
    ~loc
    ~annots
    ()
;;

module External = struct
  let which prog = Bin.which ~path:(Env_path.path Env.initial) prog

  let diff ~skip_trailing_cr ~dir annots loc file1 file2 =
    which "diff"
    |> Option.map ~f:(fun prog ->
      let args =
        [ "-u"; "--label"; file1; "--label"; file2 ]
        @ (if skip_trailing_cr then [ "--strip-trailing-cr" ] else [])
        @ [ file1; file2 ]
      in
      { dir
      ; prog
      ; args
      ; metadata = make_metadata ~has_embedded_location:false annots loc
      })
  ;;

  let git ~skip_trailing_cr annots loc path1 path2 =
    which "git"
    |> Option.map ~f:(fun prog ->
      let dir =
        (* We can't run [git] from [dir] as [dir] might be inside a sandbox
          and sandboxes have fake [.git] files to stop [git] from escaping
          the sandbox. If we did, the below git command would fail saying it
          can run this fake [.git] file. *)
        Path.root
      in
      { dir
      ; prog
      ; metadata = make_metadata ~has_embedded_location:false annots loc
      ; args =
          [ "--no-pager"; "diff"; "--no-index"; "--color=always"; "-u" ]
          @ (if skip_trailing_cr then [ "--ignore-cr-at-eol" ] else [])
          @ List.map [ path1; path2 ] ~f:(fun path ->
            resolve_link_for_git path |> Path.reach ~from:dir)
      })
  ;;

  let patdiff ~dir annots loc file1 file2 =
    which "patdiff"
    |> Option.map ~f:(fun prog ->
      let metadata =
        (* Because of the [-location-style omake], patdiff will print the
           location of each hunk in a format that the editor should understand.
           However, the location won't be the first line of the output, so the
           [process] module won't recognise that the output has a location.

           For this reason, we manually pass the below annotation. *)
        make_metadata annots loc ~has_embedded_location:true
      in
      let args =
        [ "-keep-whitespace"; "-location-style"; "omake" ]
        @ (if Lazy.force Ansi_color.stderr_supports_color then [] else [ "-ascii" ])
        @ [ file1; file2 ]
      in
      { dir; prog; metadata; args })
  ;;
end

let prepare ~skip_trailing_cr annots path1 path2 =
  let dir, file1, file2 =
    match
      ( Path.extract_build_context_dir_maybe_sandboxed path1
      , Path.extract_build_context_dir_maybe_sandboxed path2 )
    with
    | Some (dir1, f1), Some (dir2, f2) when Path.equal dir1 dir2 ->
      dir1, Path.source f1, Path.source f2
    | _ -> Path.root, path1, path2
  in
  let loc = Loc.in_file file1 in
  let file1, file2 = Path.(to_string file1, to_string file2) in
  let fallback =
    With_fallback.fail
      (User_error.make
         ~loc
         ~annots
         [ Pp.textf
             "Files %s and %s differ."
             (Path.to_string_maybe_quoted (Path.drop_optional_sandbox_root path1))
             (Path.to_string_maybe_quoted (Path.drop_optional_sandbox_root path2))
         ])
  in
  match !Clflags.diff_command with
  | Some "-" -> fallback
  | Some cmd ->
    let sh, arg = Env_path.system_shell_exn ~needed_to:"print diffs" in
    let cmd =
      sprintf "%s %s %s" cmd (String.quote_for_shell file1) (String.quote_for_shell file2)
    in
    With_fallback.run
      { prog = sh
      ; args = [ arg; cmd ]
      ; metadata = make_metadata ~has_embedded_location:false annots loc
      ; dir
      }
      ~fallback:
        (With_fallback.fail
           (User_error.make
              ~loc
              ~annots
              [ Pp.textf
                  "command reported no differences: %s"
                  (if Path.is_root dir
                   then cmd
                   else
                     sprintf
                       "cd %s && %s"
                       (String.quote_for_shell (Path.to_string dir))
                       cmd)
              ]))
  | None ->
    let diff = External.diff ~skip_trailing_cr ~dir annots loc file1 file2 in
    if Execution_env.inside_dune
    then (
      match diff with
      | None -> fallback
      | Some diff -> With_fallback.run diff ~fallback)
    else (
      let git_or_diff =
        match
          Option.first_some (External.git ~skip_trailing_cr annots loc path1 path2) diff
        with
        | None -> fallback
        | Some command -> With_fallback.run command ~fallback
      in
      match External.patdiff ~dir annots loc file1 file2 with
      | None -> git_or_diff
      | Some command -> With_fallback.run command ~fallback:git_or_diff)
;;

let print ~skip_trailing_cr annots path1 path2 =
  let p = prepare ~skip_trailing_cr annots path1 path2 in
  With_fallback.exec p
;;

let get path1 path2 =
  let p = prepare ~skip_trailing_cr:Sys.win32 User_message.Annots.empty path1 path2 in
  With_fallback.capture p
;;
