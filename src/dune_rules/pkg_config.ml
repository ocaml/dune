open Import

let pkg_config_binary sctx ~dir =
  let open Memo.O in
  let+ env = Super_context.env_node sctx ~dir >>= Env_node.external_env in
  match Env.get env "PKG_CONFIG" with
  | None -> "pkg-config"
  | Some s -> s
;;

module Query = struct
  type t =
    | Libs of string
    | Cflags of string

  let file t ~dir =
    let dir = Path.Build.relative dir ".pkg-config" in
    Path.Build.relative dir
    @@
    match t with
    | Libs s -> sprintf "%s.libs" s
    | Cflags s -> sprintf "%s.cflags" s
  ;;

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

  let read t sctx ~dir =
    let open Action_builder.O in
    let* bin =
      let open Action_builder.O in
      let* pkg_config = Action_builder.of_memo @@ pkg_config_binary sctx ~dir in
      Super_context.resolve_program sctx ~loc:None ~dir pkg_config
    in
    match bin with
    | Error _ -> Action_builder.return (default t)
    | Ok _ ->
      let file = file t ~dir in
      let+ contents = Action_builder.contents (Path.build file) in
      String.split_lines contents |> List.hd |> String.extract_blank_separated_words
  ;;
end

let gen_rule sctx ~loc ~dir query =
  let open Memo.O in
  let* bin =
    let open Memo.O in
    let* pkg_config = pkg_config_binary sctx ~dir in
    Super_context.resolve_program_memo sctx ~loc:(Some loc) ~dir pkg_config
  in
  match bin with
  | Error _ -> Memo.return @@ Error `Not_found
  | Ok _ as bin ->
    let* command =
      let+ env =
        let* dune_version = Dune_load.find_project ~dir >>| Dune_project.dune_version in
        let* env_node = Super_context.env_node sctx ~dir in
        if dune_version >= (3, 8)
        then Env_node.external_env env_node
        else Memo.return Env.empty
      in
      Command.run
        ~dir:(Path.build dir)
        ~stdout_to:(Query.file ~dir query)
        bin
        (Query.to_args ~env query)
    in
    let+ () = Super_context.add_rule sctx ~loc ~dir command in
    Ok ()
;;

let read_flags ~file =
  let open Action_builder.O in
  let+ contents = Action_builder.contents (Path.build file) in
  String.split_lines contents |> List.hd |> String.extract_blank_separated_words
;;
