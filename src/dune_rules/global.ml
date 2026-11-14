open Import

let env = Fdecl.create Env.to_dyn

(** [remove_build_dir_from_path env] is [env] with the entries pointing
    inside the build directory removed from PATH. *)
let remove_build_dir_from_path env =
  match Env.get env Env_path.var with
  | None -> env
  | Some path ->
    let value =
      let build_dir =
        Path.External.of_filename_relative_to_initial_cwd
          (Path.to_absolute_filename Path.build_dir)
      in
      Bin.parse path
      |> List.filter ~f:(function
        | "" -> true
        | dir ->
          not
            (Path.External.is_descendant
               (Path.External.of_filename_relative_to_initial_cwd dir)
               ~of_:build_dir))
      |> String.concat ~sep:(String.make 1 Bin.path_sep)
    in
    Env.add env ~var:Env_path.var ~value
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
     Env.remove env ~var:Env.Var._INSIDE_EMACS |> remove_build_dir_from_path)
;;

let env () = Fdecl.get env
