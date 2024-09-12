open Dune_config
open Import

let enabled =
  Config.make_toggle ~name:"lock_dev_tool" ~default:Dune_rules.Setup.lock_dev_tool
;;

let is_enabled =
  lazy
    (match Config.get enabled with
     | `Enabled -> true
     | `Disabled -> false)
;;

(* The solver satisfies dependencies for local packages, but dev tools
   are not local packages. As a workaround, create an empty local package
   which depends on the dev tool package. *)
let make_local_package_wrapping_dev_tool ~dev_tool ~dev_tool_version
  : Dune_pkg.Local_package.t
  =
  let dev_tool_pkg_name = Dune_pkg.Dev_tool.package_name dev_tool in
  let dependency =
    let open Dune_lang in
    let open Package_dependency in
    let constraint_ =
      Option.map dev_tool_version ~f:(fun version ->
        Package_constraint.Uop
          ( Relop.Eq
          , Package_constraint.Value.String_literal (Package_version.to_string version) ))
    in
    { name = dev_tool_pkg_name; constraint_ }
  in
  let local_package_name =
    Package_name.of_string (Package_name.to_string dev_tool_pkg_name ^ "_dev_tool_wrapper")
  in
  { Dune_pkg.Local_package.name = local_package_name
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

let lock_ocamlformat () : unit Fiber.t =
  let version = Dune_pkg.Ocamlformat.version_of_current_project's_ocamlformat_config () in
  let ocamlformat_dev_tool_lock_dir =
    Dune_pkg.Lock_dir.dev_tool_lock_dir_path Ocamlformat
  in
  if not (Path.exists @@ Path.source ocamlformat_dev_tool_lock_dir)
  then (
    let local_pkg =
      make_local_package_wrapping_dev_tool ~dev_tool:Ocamlformat ~dev_tool_version:version
    in
    let local_packages = Package_name.Map.singleton local_pkg.name local_pkg in
    solve ~local_packages ~lock_dirs:[ ocamlformat_dev_tool_lock_dir ])
  else Fiber.return ()
;;
