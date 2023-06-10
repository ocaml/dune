open Import
open Stdune

module Kind = struct
  type t =
    | Required
    | Optional

  let to_dyn : t -> Dyn.t = function
    | Required -> String "required"
    | Optional -> String "optional"
end

type external_lib_dep =
  { name : Lib_name.t
  ; kind : Kind.t
  }

let external_lib_dep_to_dyn t =
  let open Dyn in
  List [ String (Lib_name.to_string t.name); Kind.to_dyn t.kind ]

module Item = struct
  module Kind = struct
    type t =
      | Executables
      | Library
      | Tests

    let to_string = function
      | Executables -> "executables"
      | Library -> "library"
      | Tests -> "tests"
  end

  type t =
    { kind : Kind.t
    ; dir : Path.Source.t
    ; external_deps : external_lib_dep list
    ; names : string list
    ; package : Package.t option
    }

  let to_dyn t =
    let open Dyn in
    let record =
      record
        [ ("names", (list string) t.names)
        ; ( "package"
          , option Package.Name.to_dyn (Option.map ~f:Package.name t.package) )
        ; ("source_dir", String (Path.Source.to_string t.dir))
        ; ("external_deps", list external_lib_dep_to_dyn t.external_deps)
        ]
    in
    Variant (Kind.to_string t.kind, [ record ])
end

let is_external db name =
  let open Memo.O in
  let+ lib = Dune_rules.Lib.DB.find_even_when_hidden db name in
  match lib with
  | None -> true
  | Some t -> (
    match Dune_rules.Lib_info.status (Dune_rules.Lib.info t) with
    | Installed_private | Public _ | Private _ -> false
    | Installed -> true)

let external_lib_pps db preprocess =
  let open Memo.O in
  let* pps =
    Resolve.Memo.read_memo
      (Dune_rules.Preprocess.Per_module.with_instrumentation preprocess
         ~instrumentation_backend:(Dune_rules.Lib.DB.instrumentation_backend db))
    >>| Dune_rules.Preprocess.Per_module.pps
  in
  Memo.parallel_map
    ~f:(fun (_, name) ->
      let+ is_external = is_external db name in
      if is_external then Some { name; kind = Kind.Required } else None)
    pps
  >>| List.filter_opt

let external_resolve db name kind =
  let open Memo.O in
  let+ is_external = is_external db name in
  if is_external then Some { name; kind } else None

let external_lib_deps db lib_deps =
  let open Memo.O in
  Memo.parallel_map lib_deps ~f:(fun lib ->
      match lib with
      | Dune_rules.Lib_dep.Direct (_, name) | Re_export (_, name) -> (
        let+ v = external_resolve db name Kind.Required in
        match v with
        | Some x -> [ x ]
        | None -> [])
      | Select select ->
        Memo.parallel_map select.choices
          ~f:(fun (choice : Dune_rules.Lib_dep.Select.Choice.t) ->
            Memo.parallel_map
              (Lib_name.Set.to_string_list choice.required
              @ Lib_name.Set.to_string_list choice.forbidden)
              ~f:(fun name ->
                external_resolve db (Lib_name.of_string name) Kind.Optional)
            >>| List.filter_opt)
        >>| List.concat)
  >>| List.concat

let external_libs db dir libraries preprocess names package kind =
  let open Memo.O in
  let open Item in
  let* lib_deps = external_lib_deps db libraries in
  let+ lib_pps = external_lib_pps db preprocess in
  Some { kind; dir; names; package; external_deps = lib_deps @ lib_pps }

let libs db (context : Context.t) (build_system : Dune_rules.Main.build_system)
    =
  let { Dune_rules.Main.conf; contexts = _; _ } = build_system in
  let open Memo.O in
  let* dune_files =
    Dune_rules.Dune_load.Dune_files.eval conf.dune_files ~context
  in
  Memo.parallel_map dune_files ~f:(fun (dune_file : Dune_rules.Dune_file.t) ->
      Memo.parallel_map dune_file.stanzas ~f:(fun stanza ->
          let dir = dune_file.dir in
          match stanza with
          | Dune_rules.Dune_file.Executables exes ->
            external_libs db dir exes.buildable.libraries
              exes.buildable.preprocess
              (List.map exes.names ~f:snd)
              exes.package Item.Kind.Executables
          | Dune_rules.Dune_file.Library lib ->
            external_libs db dir lib.buildable.libraries
              lib.buildable.preprocess
              [ Dune_rules.Dune_file.Library.best_name lib |> Lib_name.to_string
              ]
              (Dune_rules.Dune_file.Library.package lib)
              Item.Kind.Library
          | Dune_rules.Dune_file.Tests tests ->
            external_libs db dir tests.exes.buildable.libraries
              tests.exes.buildable.preprocess
              (List.map tests.exes.names ~f:snd)
              tests.exes.package Item.Kind.Tests
          | _ -> Memo.return None)
      >>| List.filter_opt)
  >>| List.concat

let external_resolved_libs setup super_context =
  let open Memo.O in
  let context = Super_context.context super_context in
  let* scope = Dune_rules.Scope.DB.find_by_dir context.build_dir in
  let db = Dune_rules.Scope.libs scope in
  libs db context setup
  >>| List.filter ~f:(fun (x : Item.t) -> not (x.external_deps = []))

let to_dyn context_name external_resolved_libs =
  let open Dyn in
  Tuple [ String context_name; list Item.to_dyn external_resolved_libs ]

let term =
  let+ common = Common.term
  and+ context_name = Common.context_arg ~doc:"Build context to use."
  and+ _ = Describe_lang_compat.arg
  and+ format = Describe_format.arg in
  let config = Common.init common in
  Scheduler.go ~common ~config @@ fun () ->
  let open Fiber.O in
  let* setup = Import.Main.setup () in
  let* setup = Memo.run setup in
  let super_context = Import.Main.find_scontext_exn setup ~name:context_name in
  Build_system.run_exn @@ fun () ->
  let open Memo.O in
  let context_name =
    Super_context.context super_context
    |> Context.name |> Dune_engine.Context_name.to_string
  in
  external_resolved_libs setup super_context
  >>| to_dyn context_name
  >>| Describe_format.print_dyn format

let command =
  let doc =
    "Print out external libraries needed to build the project. It's an \
     approximated set of libraries."
  in
  let info = Cmd.info ~doc "external-lib-deps" in
  Cmd.v info term
