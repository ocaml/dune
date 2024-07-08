open Import

type cmd = Fmt

module Dev_tool = struct
  type t = Ocamlformat

  let cmd_dev_tool = function
    | Fmt -> "ocamlformat", "0.26.2"
  ;;
end

let local_dev ~pkg_name ~version : Dune_pkg.Local_package.t =
  let dependency =
    let open Dune_lang in
    let open Package_dependency in
    let _constraint =
      Package_constraint.Uop (Relop.Eq, Package_constraint.Value.String_literal version)
    in
    { name = Package_name.of_string pkg_name; constraint_ = Some _constraint }
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

let is_pkg_lock_dir_active () = Path.exists @@ Path.source Dune_pkg.Lock_dir.default_path

let lock cmd =
  match cmd with
  | Fmt ->
    let pkg_name, version = Dev_tool.cmd_dev_tool Fmt in
    let lock_dir = Path.Source.relative Dune_pkg.Lock_dir.dev_tools_path pkg_name in
    if (not (Path.exists @@ Path.source lock_dir)) && is_pkg_lock_dir_active ()
    then (
      let pkg = local_dev ~pkg_name ~version in
      let local_packages = Package_name.Map.singleton pkg.name pkg in
      solve ~local_packages ~lock_dirs:[ lock_dir ])
    else Fiber.return ()
;;
