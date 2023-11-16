open Import

module Odoc = struct
  type warnings = Dune_env.Stanza.Odoc.warnings =
    | Fatal
    | Nonfatal

  type t = { warnings : warnings }
end

module Coq = struct
  type t = string list
end

type t =
  { scope : Scope.t
  ; local_binaries : File_binding.Expanded.t list Memo.Lazy.t
  ; ocaml_flags : Ocaml_flags.t Memo.Lazy.t
  ; foreign_flags : string list Action_builder.t Foreign_language.Dict.t
  ; link_flags : Link_flags.t Memo.Lazy.t
  ; external_env : Env.t Memo.Lazy.t
  ; artifacts : Artifacts.t Memo.Lazy.t
  ; inline_tests : Dune_env.Stanza.Inline_tests.t Memo.Lazy.t
  ; menhir_flags : string list Action_builder.t Memo.Lazy.t
  ; odoc : Odoc.t Action_builder.t Memo.Lazy.t
  ; js_of_ocaml : string list Action_builder.t Js_of_ocaml.Env.t Memo.Lazy.t
  ; coq : Coq.t Action_builder.t Memo.Lazy.t
  ; format_config : Format_config.t Memo.Lazy.t
  ; bin_annot : bool Memo.Lazy.t
  }

let scope t = t.scope
let local_binaries t = Memo.Lazy.force t.local_binaries
let ocaml_flags t = Memo.Lazy.force t.ocaml_flags
let foreign_flags t = t.foreign_flags
let link_flags t = Memo.Lazy.force t.link_flags
let external_env t = Memo.Lazy.force t.external_env
let artifacts t = Memo.Lazy.force t.artifacts
let inline_tests t = Memo.Lazy.force t.inline_tests
let js_of_ocaml t = Memo.Lazy.force t.js_of_ocaml
let menhir_flags t = Memo.Lazy.force t.menhir_flags |> Action_builder.of_memo_join
let format_config t = Memo.Lazy.force t.format_config

let set_format_config t format_config =
  { t with format_config = Memo.Lazy.of_val format_config }
;;

let odoc t = Memo.Lazy.force t.odoc |> Action_builder.of_memo_join
let coq t = Memo.Lazy.force t.coq
let bin_annot t = Memo.Lazy.force t.bin_annot

let expand_str_lazy expander sw =
  match String_with_vars.text_only sw with
  | Some s -> Memo.return s
  | None ->
    let open Memo.O in
    let* expander = Memo.Lazy.force expander in
    Expander.No_deps.expand_str expander sw
;;

let make
  build_context
  ~dir
  ~inherit_from
  ~scope
  ~config_stanza
  ~profile
  ~expander
  ~expander_for_artifacts
  ~default_context_flags
  ~default_env
  ~default_artifacts
  ~default_bin_annot
  =
  let open Memo.O in
  let config = Dune_env.Stanza.find config_stanza ~profile in
  let inherited ~field ~root extend =
    Memo.lazy_ (fun () ->
      (match inherit_from with
       | None -> Memo.return root
       | Some t -> Memo.Lazy.force t >>= field)
      >>= extend)
  in
  let inherited_if_absent ~field ~root ~f_absent =
    Memo.lazy_ (fun () ->
      match root with
      | Some x -> Memo.return x
      | None ->
        (match inherit_from with
         | None -> f_absent None
         | Some t ->
           let* field = Memo.Lazy.force t >>= field in
           f_absent (Some field)))
  in
  let local_binaries =
    Memo.lazy_ (fun () ->
      Memo.parallel_map
        config.binaries
        ~f:
          (File_binding.Unexpanded.expand
             ~dir
             ~f:(expand_str_lazy expander_for_artifacts)))
  in
  let external_env =
    inherited ~field:external_env ~root:default_env (fun env ->
      let env =
        let env = Env.extend_env env config.env_vars in
        match config.binaries with
        | [] -> env
        | _ :: _ ->
          let dir = Artifacts.local_bin dir |> Path.build in
          Env_path.cons env ~dir
      in
      Memo.return env)
  in
  let artifacts =
    inherited ~field:artifacts ~root:default_artifacts (fun binaries ->
      let+ local_binaries = Memo.Lazy.force local_binaries in
      Artifacts.add_binaries binaries ~dir local_binaries)
  in
  let ocaml_flags =
    let default_ocaml_flags =
      let project = Scope.project scope in
      let dune_version = Dune_project.dune_version project in
      Ocaml_flags.default ~profile ~dune_version
    in
    inherited ~field:ocaml_flags ~root:default_ocaml_flags (fun flags ->
      let+ expander = Memo.Lazy.force expander in
      Ocaml_flags.make
        ~spec:config.flags
        ~default:flags
        ~eval:(Expander.expand_and_eval_set expander))
  in
  let inline_tests =
    match config with
    | { inline_tests = Some s; _ } -> Memo.Lazy.of_val s
    | { inline_tests = None; _ } ->
      inherited
        ~field:inline_tests
        Memo.return
        ~root:
          (if Profile.is_inline_test profile
           then Dune_env.Stanza.Inline_tests.Enabled
           else Disabled)
  in
  let js_of_ocaml =
    inherited
      ~field:(fun t -> js_of_ocaml t)
      ~root:Js_of_ocaml.Env.(map ~f:Action_builder.return (default ~profile))
      (fun (jsoo : _ Action_builder.t Js_of_ocaml.Env.t) ->
        let local = config.js_of_ocaml in
        let+ expander = Memo.Lazy.force expander in
        { Js_of_ocaml.Env.compilation_mode =
            Option.first_some local.compilation_mode jsoo.compilation_mode
        ; runtest_alias = Option.first_some local.runtest_alias jsoo.runtest_alias
        ; flags =
            Js_of_ocaml.Flags.make
              ~spec:local.flags
              ~default:jsoo.flags
              ~eval:(Expander.expand_and_eval_set expander)
        })
  in
  let foreign_flags lang =
    let field t = Memo.return (Foreign_language.Dict.get t.foreign_flags lang) in
    Action_builder.of_memo_join
      (Memo.Lazy.force
         (inherited
            ~field
            ~root:(Foreign_language.Dict.get default_context_flags lang)
            (fun flags ->
               let+ expander = Memo.Lazy.force expander in
               let f = Foreign_language.Dict.get config.foreign_flags lang in
               Expander.expand_and_eval_set expander f ~standard:flags)))
  in
  let foreign_flags =
    Foreign_language.Dict.make ~c:(foreign_flags C) ~cxx:(foreign_flags Cxx)
  in
  let link_flags =
    let default_link_flags =
      let default_cxx_link_flags = Cxx_flags.get_flags ~for_:Link build_context in
      Link_flags.default ~default_cxx_link_flags
    in
    inherited ~field:link_flags ~root:default_link_flags (fun link_flags ->
      let+ expander = Memo.Lazy.force expander in
      Link_flags.make
        ~spec:config.link_flags
        ~default:link_flags
        ~eval:(Expander.expand_and_eval_set expander))
  in
  let menhir_flags =
    inherited
      ~field:(fun t -> Memo.return (menhir_flags t))
      ~root:(Action_builder.return [])
      (fun flags ->
        match config.menhir_flags with
        | None -> Memo.return flags
        | Some menhir_flags ->
          let+ expander = Memo.Lazy.force expander in
          Expander.expand_and_eval_set expander menhir_flags ~standard:flags)
  in
  let odoc =
    let open Odoc in
    let root =
      (* DUNE4: Enable for dev profile in the future *)
      Action_builder.return { warnings = Nonfatal }
    in
    inherited
      ~field:(fun t -> Memo.return (odoc t))
      ~root
      (fun warnings ->
        Memo.return
        @@
        let open Action_builder.O in
        let+ { warnings } = warnings in
        { warnings = Option.value config.odoc.warnings ~default:warnings })
  in
  let default_coq_flags = Action_builder.return [ "-q" ] in
  let coq : Coq.t Action_builder.t Memo.Lazy.t =
    inherited ~field:coq ~root:default_coq_flags (fun flags ->
      let+ expander = Memo.Lazy.force expander in
      let standard = flags in
      Expander.expand_and_eval_set expander config.coq ~standard)
  in
  let format_config =
    inherited_if_absent
      ~field:format_config
      ~root:config.format_config
      ~f_absent:(function
      | Some x -> Memo.return x
      | None ->
        Code_error.raise
          "format config should always have a default value taken from the project root"
          [])
  in
  let bin_annot =
    inherited ~field:bin_annot ~root:default_bin_annot (fun default ->
      Memo.return (Option.value ~default config.bin_annot))
  in
  { scope
  ; ocaml_flags
  ; foreign_flags
  ; link_flags
  ; external_env
  ; artifacts
  ; local_binaries
  ; inline_tests
  ; js_of_ocaml
  ; menhir_flags
  ; odoc
  ; coq
  ; format_config
  ; bin_annot
  }
;;
