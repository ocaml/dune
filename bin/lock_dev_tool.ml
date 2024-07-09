open Import

type cmd = Fmt

module Dev_tool = struct
  type t = Ocamlformat

  let cmd_dev_tool = function
    | Fmt ->
      let ocamlformat = "ocamlformat" in
      let ocamlformat_config = Path.Source.of_string ".ocamlformat" |> Path.source in
      (match Path.exists ocamlformat_config with
       | false -> ocamlformat, None
       | true ->
         (try
            ( ocamlformat
            , Io.lines_of_file ocamlformat_config
              |> List.find_map ~f:(fun line ->
                match String.split_on_char ~sep:'=' line |> List.map ~f:String.trim with
                | [ "version"; value ] -> Some value
                | _ -> None) )
          with
          | _ -> ocamlformat, None))
  ;;
end

let local_dev ~pkg_name ~version : Dune_pkg.Local_package.t =
  let dependency =
    let open Dune_lang in
    let open Package_dependency in
    let constraint_ =
      Option.map version ~f:(fun version ->
        Package_constraint.Uop (Relop.Eq, Package_constraint.Value.String_literal version))
    in
    { name = Package_name.of_string pkg_name; constraint_ }
  in
  { Dune_pkg.Local_package.name = Package_name.of_string (pkg_name ^ "_dev")
  ; version = None
  ; dependencies = [ dependency ]
  ; conflicts = []
  ; depopts = []
  ; pins = Package_name.Map.empty
  ; conflict_class = []
  ; loc = Loc.none
  }
;;

let solve ~local_packages ~lock_dirs =
  let open Fiber.O in
  let* solver_env_from_current_system =
    Dune_pkg.Sys_poll.make ~path:(Env_path.path Stdune.Env.initial)
    |> Dune_pkg.Sys_poll.solver_env_from_current_system
    >>| Option.some
  and* workspace =
    Memo.run
    @@
    let open Memo.O in
    let+ workspace = Workspace.workspace () in
    workspace
  in
  Lock.solve
    workspace
    ~local_packages
    ~project_sources:Dune_pkg.Pin_stanza.DB.empty
    ~solver_env_from_current_system
    ~version_preference:None
    ~lock_dirs
;;

let exists_path path = Path.exists @@ Path.source path
let default_lock_dir_active () = exists_path Dune_pkg.Lock_dir.default_path

let exists_pkg_in_default_lock_dir pkg_name =
  let filename =
    Dune_pkg.Lock_dir.Package_filename.of_package_name
      (Dune_lang.Package_name.of_string pkg_name)
  in
  Path.Source.relative Dune_pkg.Lock_dir.default_path filename |> exists_path
;;

let dev_tool_path pkg_name =
  Path.Source.relative Dune_pkg.Lock_dir.dev_tools_path pkg_name
;;

let lock cmd =
  match cmd with
  | Fmt ->
    let pkg_name, version = Dev_tool.cmd_dev_tool Fmt in
    let lock_dir = dev_tool_path pkg_name in
    if (not (exists_path lock_dir))
       && default_lock_dir_active ()
       && not (exists_pkg_in_default_lock_dir pkg_name)
    then (
      let local_pkg = local_dev ~pkg_name ~version in
      let local_packages = Package_name.Map.singleton local_pkg.name local_pkg in
      solve ~local_packages ~lock_dirs:[ lock_dir ])
    else Fiber.return ()
;;
