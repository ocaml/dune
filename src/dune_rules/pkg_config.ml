open Import
open Memo.O

let pkg_config_binary sctx ~dir =
  let+ env = Super_context.env_node sctx ~dir >>= Env_node.external_env in
  match Env.get env "PKG_CONFIG" with
  | None -> "pkg-config"
  | Some s -> s
;;

module Query = struct
  type t =
    | Libs of string
    | Cflags of string

  let to_args t ~env : _ Command.Args.t list =
    let env_args : _ Command.Args.t =
      match Env.get env "PKG_CONFIG_ARGN" with
      | Some s -> As (String.split_on_char ~sep:' ' s)
      | None -> Command.Args.empty
    in
    Hidden_deps Dep.(Set.singleton universe)
    :: env_args
    ::
    (match t with
     | Libs lib -> [ A "--libs"; A lib ]
     | Cflags lib -> [ A "--cflags"; A lib ])
  ;;

  let default = function
    | Libs lib -> [ sprintf "-l%s" lib ]
    | Cflags _ -> [ "-I/usr/include" ]
  ;;

  let read ~loc t sctx ~dir =
    let open Action_builder.O in
    let* bin =
      let* pkg_config = Action_builder.of_memo @@ pkg_config_binary sctx ~dir in
      Super_context.resolve_program sctx ~loc:None ~dir pkg_config
    in
    match bin with
    | Error _ -> Action_builder.return (default t)
    | Ok _ as bin ->
      let external_env =
        let open Memo.O in
        Super_context.env_node sctx ~dir >>= Env_node.external_env
      in
      let action =
        let+ action =
          let* env =
            Action_builder.of_memo
              (let open Memo.O in
               let* dune_version =
                 Dune_load.find_project ~dir >>| Dune_project.dune_version
               in
               if dune_version >= (3, 8) then external_env else Memo.return Env.empty)
          in
          Command.run'
            ~dir:(Path.build dir)
            ~env:(Action_builder.of_memo external_env)
            bin
            (to_args t ~env)
        in
        { Rule.Anonymous_action.action; loc; dir; alias = None }
      in
      Build_system.execute_action_stdout action
      |> Memo.map ~f:(fun contents ->
        String.split_lines contents |> List.hd |> String.extract_blank_separated_words)
      |> Action_builder.of_memo
  ;;
end
