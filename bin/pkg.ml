open Stdune
open Import
module Lock_dir = Dune_pkg.Lock_dir

(* Takes a string containing a possibly absolute path and returns a
   [Path.Source.t] referring to that path. In the case of an absolute path,
   [Error `Absolute_path_outside_source_directory] is returned in the case that
   the specified path is outside the source directory. *)
let source_path_from_possibly_absolute_path path_string =
  if Filename.is_relative path_string then
    Ok (Path.Source.of_string path_string)
  else
    let path = Path.External.of_string path_string |> Path.external_ in
    let source_dir_path = Path.external_ Path.External.initial_cwd in
    match Path.drop_prefix path ~prefix:source_dir_path with
    | None -> Error `Absolute_path_outside_source_directory
    | Some relative_path -> Ok (Path.Source.of_local relative_path)

let lock_dir_term =
  let+ lock_dir_opt =
    Arg.(
      value
      & opt (some string) None
      & info [ "lock-dir" ] ~docv:"PATH"
          ~doc:
            (sprintf "Path to lock directory (default: %s)"
               (Stdune.Path.Source.to_string Lock_dir.default_path)))
  in
  Option.map lock_dir_opt ~f:(fun path_string ->
      match source_path_from_possibly_absolute_path path_string with
      | Ok source_path -> source_path
      | Error `Absolute_path_outside_source_directory ->
        User_error.raise
          [ Pp.textf
              "Specified lock directory (%s) is not a descendant of the source \
               directory."
              (String.maybe_quoted path_string)
          ])
  |> Option.value ~default:Lock_dir.default_path

module Lock = struct
  module Repo = struct
    open Dune_pkg.Opam.Repo

    let term =
      let+ opam_repository_path =
        Arg.(
          required
          & opt (some string) None
          & info [ "opam-repository-path" ] ~docv:"PATH"
              ~doc:
                "Path to a local opam repository. This should be a directory \
                 containing a valid opam repository such as the one at \
                 https://github.com/ocaml/opam-repository.")
      in
      of_opam_repo_dir_path opam_repository_path
  end

  module Env = struct
    module Source = struct
      type t =
        | Global
        | Pure

      let to_string = function
        | Global -> "global"
        | Pure -> "pure"

      let default = Global

      let term =
        let all = [ Global; Pure ] in
        let all_with_strings = List.map all ~f:(fun t -> (to_string t, t)) in
        let all_strings = List.map all_with_strings ~f:fst in
        let doc =
          sprintf
            "How to initialize the opam environment. Possible values are %s. \
             '%s' will use the environment associated with the current opam \
             switch. '%s' will use an empty environment. The default is '%s'."
            (String.enumerate_and all_strings)
            (to_string Global) (to_string Pure) (to_string default)
        in
        Arg.(
          value
          & opt (some (enum all_with_strings)) None
          & info [ "opam-env" ] ~doc)
    end

    open Dune_pkg.Opam.Env

    let term =
      let+ source = Source.term in
      match Option.value source ~default:Source.default with
      | Global -> global ()
      | Pure -> empty
  end

  (* Converts the package table found inside a [Dune_project.t] into the
     package table expected by the dependency solver *)
  let opam_file_map_of_dune_package_map
      (dune_package_map : Package.t Package.Name.Map.t) :
      OpamFile.OPAM.t OpamTypes.name_map =
    Package.Name.Map.to_list_map dune_package_map
      ~f:(fun dune_package_name dune_package ->
        let opam_package_name =
          Package.Name.to_opam_package_name dune_package_name
        in
        let opam_file = Package.to_opam_file dune_package in
        (opam_package_name, opam_file))
    |> OpamPackage.Name.Map.of_list

  let term =
    let+ (common : Common.t) = Common.term
    and+ env = Env.term
    and+ repo = Repo.term
    and+ lock_dir_path = lock_dir_term in
    let config = Common.init common in
    Scheduler.go ~common ~config (fun () ->
        let open Fiber.O in
        let* source_dir = Memo.run (Source_tree.root ()) in
        let project = Source_tree.Dir.project source_dir in
        let dune_package_map = Dune_project.packages project in
        let opam_file_map =
          opam_file_map_of_dune_package_map dune_package_map
        in
        let summary, lock_dir =
          Dune_pkg.Opam.solve_lock_dir ~env ~repo ~lock_dir_path opam_file_map
        in
        Console.print_user_message
          (Dune_pkg.Opam.Summary.selected_packages_message summary);
        Lock_dir.write_disk ~lock_dir_path lock_dir;
        Fiber.return ())

  let info =
    let doc = "Create a lockfile" in
    Cmd.info "lock" ~doc

  let command = Cmd.v info term
end

let info =
  let doc = "Experimental package management" in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|Commands for doing package management with dune|}
    ; `Blocks Common.help_secs
    ]
  in
  Cmd.info "pkg" ~doc ~man

let group = Cmd.group info [ Lock.command ]
