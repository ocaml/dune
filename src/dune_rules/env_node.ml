open Import

type t =
  { local_binaries : File_binding.Expanded.t list Memo.Lazy.t
  ; base_env : Env.t Memo.Lazy.t
  ; local_bin_dirs : Path.t list Memo.Lazy.t
  ; external_env : Env.t Memo.Lazy.t
  ; artifacts : Artifacts.t Memo.Lazy.t
  }

let local_binaries t = Memo.Lazy.force t.local_binaries
let base_env t = Memo.Lazy.force t.base_env
let local_bin_dirs t = Memo.Lazy.force t.local_bin_dirs
let external_env t = Memo.Lazy.force t.external_env
let artifacts t = Memo.Lazy.force t.artifacts

let expand_str_lazy expander sw =
  Memo.Option.value (String_with_vars.text_only sw) ~default:(fun () ->
    let open Memo.O in
    let* expander = expander in
    Expander.No_deps.expand_str expander sw)
;;

let make
      ~dir
      ~inherit_from
      ~config_stanza
      ~profile
      ~expander
      ~default_env
      ~default_artifacts
      ~visible_packages
      ~lockdir_bin_env
  =
  let open Memo.O in
  let config = Dune_env.find config_stanza ~profile in
  let inherited ~field ~root extend =
    Memo.lazy_ ~name:"inherited-environment-field" (fun () ->
      (match inherit_from with
       | None -> root
       | Some t -> Memo.Lazy.force t >>= field)
      >>= extend)
  in
  let config_binaries = Option.value config.binaries ~default:[] in
  let local_bin_dirs =
    inherited ~field:local_bin_dirs ~root:(Memo.return []) (fun dirs ->
      Memo.return
        (match config_binaries with
         | [] -> dirs
         | _ :: _ -> (Artifacts.local_bin dir |> Path.build) :: dirs))
  in
  let base_env =
    inherited ~field:base_env ~root:default_env (fun env ->
      Memo.return (Env.extend_env env config.env_vars))
  in
  (* The lock directory [PATH] is added to each node's own [base_env] rather
     than inherited: the visible packages differ per directory, so inheriting
     it would widen a descendant's [PATH] back to its ancestor's.

     [local_bin_dirs] go ahead of it, so that a binary bound by
     [(env (binaries ...))] wins on [PATH] just as it does in [%{bin:...}],
     where [local_bins] is consulted before the lock directory. *)
  let external_env =
    Memo.lazy_ ~name:"external-env" (fun () ->
      let* env = Memo.Lazy.force base_env
      and* dirs = Memo.Lazy.force local_bin_dirs in
      let+ bin_env = lockdir_bin_env in
      let env = Env_path.extend_env_concat_path env bin_env in
      List.fold_right dirs ~init:env ~f:(fun dir env -> Env_path.cons env ~dir))
  in
  let artifacts =
    inherited ~field:artifacts ~root:default_artifacts (fun binaries ->
      let* visible_packages = visible_packages in
      Memo.parallel_map
        config_binaries
        ~f:(File_binding_expand.expand ~dir ~f:(expand_str_lazy expander))
      >>| Artifacts.add_binaries binaries ~dir
      >>| Artifacts.set_visible_packages ~visible_packages)
  in
  let local_binaries =
    Memo.lazy_ ~name:"local-binaries" (fun () ->
      Memo.Lazy.force artifacts >>= Artifacts.local_binaries)
  in
  { base_env; local_bin_dirs; external_env; artifacts; local_binaries }
;;
