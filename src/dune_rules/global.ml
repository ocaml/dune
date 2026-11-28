open Import

let env = Fdecl.create Env.to_dyn

let remove_build_dir_from_path =
  let build_dir =
    lazy
      (Path.to_absolute_filename Path.build_dir
       |> Path.External.of_string
       |> Path.External.normalize)
  and sanitized_paths = String.Table.create 1 in
  fun path ->
    String.Table.find_or_add sanitized_paths path ~f:(fun path ->
      let build_dir = Lazy.force build_dir in
      Bin.parse path
      |> List.filter ~f:(function
        | "" -> true
        | dir ->
          let dir =
            Path.External.of_filename_relative_to_initial_cwd dir
            |> Path.External.normalize
          in
          not
            (Path.External.equal dir build_dir
             || Path.External.is_descendant dir ~of_:build_dir))
      |> Bin.encode_strings)
;;

let remove_build_dir_from_env env =
  Env.update env ~var:Env_path.var ~f:(Option.map ~f:remove_build_dir_from_path)
;;

let init ~capture_outputs =
  Fdecl.set
    env
    (let env =
       if (not capture_outputs) || not (Lazy.force Ansi_color.stderr_supports_color)
       then Env.initial
       else Colors.setup_env_for_colors Env.initial
     in
     let env =
       let value = Execution_env.Inside_dune.value Yes in
       Env.add env ~var:Execution_env.Inside_dune.var ~value
     in
     (* To improve reproducibility, we don't let command executed by Dune
        observe whether Dune is run inside emacs or not. One such program that
        behave differently when run inside emacs is Dune itself and we sometimes
        run Dune from inside Dune, for instance in cram tests, so it is
        important to do this. *)
     Env.remove env ~var:Env.Var._INSIDE_EMACS)
;;

let env () = Fdecl.get env

let rule_env =
  let env = lazy (remove_build_dir_from_env (env ())) in
  fun () -> Lazy.force env
;;
