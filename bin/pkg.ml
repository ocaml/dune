open Stdune
open Import
module Lock_dir = Dune_pkg.Lock_dir

module Lock = struct
  module Repo_selection = struct
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
              "How to initialize the opam environment when taking the opam \
               repository from a local directory (may only be used along with \
               the --opam-repository-path option). Possible values are %s. \
               '%s' will use the environment associated with the current opam \
               switch. '%s' will use an empty environment. The default is \
               '%s'."
              (String.enumerate_and all_strings)
              (to_string Global) (to_string Pure) (to_string default)
          in
          Arg.(
            value
            & opt (some (enum all_with_strings)) None
            & info [ "opam-env" ] ~doc)
      end

      let of_source =
        let open Dune_pkg.Opam.Env in
        function
        | Source.Global -> global ()
        | Pure -> empty
    end

    let term =
      let+ opam_repository_path =
        Arg.(
          value
          & opt (some string) None
          & info [ "opam-repository-path" ] ~docv:"PATH"
              ~doc:
                "Path to a local opam repository. This should be a directory \
                 containing a valid opam repository such as the one at \
                 https://github.com/ocaml/opam-repository. If this option is \
                 omitted the dependencies will be locked using the current \
                 switch instead.")
      and+ opam_switch_name =
        Arg.(
          value
          & opt (some string) None
          & info [ "opam-switch" ] ~docv:"SWITCH"
              ~doc:
                "Name or path of opam switch to use while solving \
                 dependencies. Local switches may be specified with relative \
                 paths (e.g. `--opam-switch=.`)")
      and+ env_source = Env.Source.term in
      let module Repo_selection = Dune_pkg.Opam.Repo_selection in
      match (opam_switch_name, opam_repository_path, env_source) with
      | None, None, _env_source | Some _, Some _, _env_source ->
        User_error.raise
          [ Pp.text
              "Exactly one of --opam-switch and --opam-repository-path must be \
               specified"
          ]
      | Some _opam_switch, None, Some _env_source ->
        User_error.raise
          [ Pp.text "--opam-env may only used with --opam-repository-path" ]
      | Some opam_switch_name, None, None ->
        Repo_selection.switch_with_name opam_switch_name
      | None, Some opam_repo_dir_path, env_source_opt ->
        let env =
          Option.value env_source_opt ~default:Env.Source.default
          |> Env.of_source
        in
        Repo_selection.local_repo_with_env ~opam_repo_dir_path ~env
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
    and+ repo_selection = Repo_selection.term in
    let config = Common.init common in
    Scheduler.go ~common ~config (fun () ->
        let open Fiber.O in
        let* source_dir = Memo.run (Source_tree.root ()) in
        let project = Source_tree.Dir.project source_dir in
        let dune_package_map = Dune_project.packages project in
        let opam_file_map =
          opam_file_map_of_dune_package_map dune_package_map
        in
        let lock_dir_path = Lock_dir.default_path in
        let summary, lock_dir =
          Dune_pkg.Opam.solve_lock_dir ~repo_selection opam_file_map
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
