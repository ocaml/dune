open Import

type t =
  { local_binaries : File_binding.Expanded.t list Memo.Lazy.t
  ; base_env : Env.t Memo.Lazy.t
  ; external_env : Env.t Memo.Lazy.t
  ; artifacts : Artifacts.t Memo.Lazy.t
  }

let local_binaries t = Memo.Lazy.force t.local_binaries
let base_env t = Memo.Lazy.force t.base_env
let external_env t = Memo.Lazy.force t.external_env
let artifacts t = Memo.Lazy.force t.artifacts

let expand_str_lazy expander sw =
  match String_with_vars.text_only sw with
  | Some s -> Memo.return s
  | None ->
    let open Memo.O in
    let* expander = expander in
    Expander.No_deps.expand_str expander sw
;;

let make
      ~dir
      ~inherit_from
      ~config_stanza
      ~profile
      ~expander
      ~default_env
      ~default_artifacts
      ~owning_package_deps
      ~lockdir_bin_env
  =
  let open Memo.O in
  let config = Dune_env.find config_stanza ~profile in
  let inherited ~field ~root extend =
    Memo.lazy_ (fun () ->
      (match inherit_from with
       | None -> root
       | Some t -> Memo.Lazy.force t >>= field)
      >>= extend)
  in
  let config_binaries = Option.value config.binaries ~default:[] in
  let base_env =
    inherited ~field:base_env ~root:default_env (fun env ->
      let env =
        let env = Env.extend_env env config.env_vars in
        match config_binaries with
        | [] -> env
        | _ :: _ ->
          let dir = Artifacts.local_bin dir |> Path.build in
          Env_path.cons env ~dir
      in
      Memo.return env)
  in
  let external_env =
    Memo.lazy_ (fun () ->
      let* env = Memo.Lazy.force base_env in
      let+ bin_env = lockdir_bin_env in
      Env_path.extend_env_concat_path env bin_env)
  in
  let artifacts =
    inherited ~field:artifacts ~root:default_artifacts (fun binaries ->
      let* owning_package_deps = owning_package_deps in
      Memo.parallel_map
        config_binaries
        ~f:(File_binding_expand.expand ~dir ~f:(expand_str_lazy expander))
      >>| Artifacts.add_binaries binaries ~dir
      >>| Artifacts.set_owning_package_deps ~owning_package_deps)
  in
  let local_binaries =
    Memo.lazy_ (fun () -> Memo.Lazy.force artifacts >>= Artifacts.local_binaries)
  in
  { base_env; external_env; artifacts; local_binaries }
;;
