open Import
module Lib_dep = Dune_lang.Lib_dep

module Kind = struct
  type t =
    | Required
    | Optional

  let to_dyn : t -> Dyn.t = function
    | Required -> String "required"
    | Optional -> String "optional"
  ;;
end

type lib_dep =
  { name : Lib_name.t
  ; kind : Kind.t
  }

let lib_dep_to_dyn t =
  let open Dyn in
  List [ String (Lib_name.to_string t.name); Kind.to_dyn t.kind ]
;;

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
    ;;
  end

  type t =
    { kind : Kind.t
    ; dir : Path.Source.t
    ; external_deps : lib_dep list
    ; internal_deps : lib_dep list
    ; names : string list (* private names *)
    ; public_names : string option list option
      (* public names, if any are declared (one entry per private name, [None]
         for those without a public name) *)
    ; package : Package.t option
    ; extensions : Filename.Extension.t list
    }

  let to_dyn
        ~split_public_names
        { kind
        ; dir
        ; external_deps
        ; internal_deps
        ; names
        ; public_names
        ; package
        ; extensions
        }
    =
    let open Dyn in
    let name_fields =
      if split_public_names
      then (
        let public_names =
          match public_names with
          | Some public_names -> public_names
          | None -> List.map names ~f:(fun _ -> None)
        in
        [ "names", (list string) names
        ; "public_names", (list (option string)) public_names
        ])
      else (
        (* replace each private name by its public name when there is one *)
        let best_names =
          match public_names with
          | None -> names
          | Some public_names ->
            List.map2 names public_names ~f:(fun name public_name ->
              Option.value public_name ~default:name)
        in
        [ "names", (list string) best_names ])
    in
    let record =
      record
        (name_fields
         @ [ "extensions", (list Filename.Extension.to_dyn) extensions
           ; "package", option Package.Name.to_dyn (Option.map ~f:Package.name package)
           ; "source_dir", String (Path.Source.to_string dir)
           ; "external_deps", list lib_dep_to_dyn external_deps
           ; "internal_deps", list lib_dep_to_dyn internal_deps
           ])
    in
    Variant (Kind.to_string kind, [ record ])
  ;;
end

type dep =
  | Local of lib_dep
  | External of lib_dep

let is_external db name =
  let open Memo.O in
  let+ lib = Dune_rules.Lib.DB.find_even_when_hidden db name in
  match lib with
  | None -> true
  | Some t ->
    (match Dune_rules.Lib_info.status (Dune_rules.Lib.info t) with
     | Installed_private | Public _ | Private _ -> false
     | Installed -> true)
;;

let resolve_lib db name kind =
  let open Memo.O in
  let+ is_external = is_external db name in
  if is_external then External { name; kind } else Local { name; kind }
;;

let resolve_lib_pps db preprocess =
  let open Memo.O in
  Dune_rules.Instrumentation.with_instrumentation
    preprocess
    ~instrumentation_backend:(Dune_rules.Lib.DB.instrumentation_backend db)
  |> Resolve.Memo.read_memo
  >>| Dune_lang.Preprocess.Per_module.pps
  >>= Memo.parallel_map ~f:(fun (_, name) -> resolve_lib db name Kind.Required)
;;

let resolve_lib_deps db lib_deps =
  let open Memo.O in
  Memo.parallel_map lib_deps ~f:(fun (lib : Lib_dep.t) ->
    match lib with
    | Direct (_, name) | Re_export (_, name) | Instantiate { lib = name; _ } ->
      let+ v = resolve_lib db name Kind.Required in
      [ v ]
    | Select select ->
      select.choices
      |> Memo.parallel_map ~f:(fun (choice : Lib_dep.Select.Choice.t) ->
        Lib_name.Set.to_string_list choice.required
        @ Lib_name.Set.to_string_list choice.forbidden
        |> Memo.parallel_map ~f:(fun name ->
          let name = Lib_name.of_string name in
          resolve_lib db name Kind.Optional))
      >>| List.concat)
  >>| List.concat
;;

let resolve_libs db dir libraries preprocess names public_names package kind extensions =
  let open Memo.O in
  let open Item in
  let* lib_deps = resolve_lib_deps db libraries in
  let+ lib_pps = resolve_lib_pps db preprocess in
  let deps = lib_deps @ lib_pps in
  let internal_deps, external_deps =
    deps
    |> List.partition_map ~f:(function
      | Local lib -> Either.Left lib
      | External lib -> Either.Right lib)
  in
  { external_deps; internal_deps; kind; names; public_names; package; dir; extensions }
;;

let exes_extensions (lib_config : Dune_rules.Lib_config.t) modes =
  Dune_rules.Executables.Link_mode.Map.to_list modes
  |> List.map ~f:(fun (m, loc) ->
    Dune_rules.Executables.Link_mode.extension
      m
      ~loc
      ~ext_obj:lib_config.ext_obj
      ~ext_dll:lib_config.ext_dll)
;;

(* Splits the executables' names into their private names and, when declared,
   their public names *)
let exe_names (exes : Dune_rules.Executables.t) =
  ( Nonempty_list.to_list_map exes.names ~f:snd
  , Option.map exes.public_names ~f:(Nonempty_list.to_list_map ~f:snd) )
;;

let libs db (context : Context.t) =
  let open Memo.O in
  let* dune_files = Context.name context |> Dune_rules.Dune_load.dune_files in
  Memo.parallel_map dune_files ~f:(fun (dune_file : Dune_rules.Dune_file.t) ->
    Dune_file.stanzas dune_file
    >>= Memo.parallel_map ~f:(fun stanza ->
      let dir = Dune_file.dir dune_file in
      match Stanza.repr stanza with
      | Dune_rules.Executables.T exes ->
        let* ocaml = Context.ocaml context in
        let names, public_names = exe_names exes in
        resolve_libs
          db
          dir
          exes.buildable.libraries
          exes.buildable.preprocess.config
          names
          public_names
          exes.package
          Item.Kind.Executables
          (exes_extensions ocaml.lib_config exes.modes)
        >>| List.singleton
      | Dune_rules.Library.T lib ->
        let names = [ Lib_name.of_local lib.name |> Lib_name.to_string ] in
        let public_names =
          Option.map (Dune_rules.Library.public_name lib) ~f:(fun public_name ->
            [ Some (Lib_name.to_string public_name) ])
        in
        resolve_libs
          db
          dir
          lib.buildable.libraries
          lib.buildable.preprocess.config
          names
          public_names
          (Dune_rules.Library.package lib)
          Item.Kind.Library
          []
        >>| List.singleton
      | Dune_rules.Tests.T tests ->
        let* ocaml = Context.ocaml context in
        let names, public_names = exe_names tests.exes in
        resolve_libs
          db
          dir
          tests.exes.buildable.libraries
          tests.exes.buildable.preprocess.config
          names
          public_names
          (if Option.is_none tests.package then tests.exes.package else tests.package)
          Item.Kind.Tests
          (exes_extensions ocaml.lib_config tests.exes.modes)
        >>| List.singleton
      | _ -> Memo.return [])
    >>| List.concat)
  >>| List.concat
;;

let external_resolved_libs (context : Context.t) =
  let open Memo.O in
  let* scope = Dune_rules.Scope.DB.find_by_dir (Context.build_dir context) in
  let db = Dune_rules.Scope.libs scope in
  libs db context
  >>| List.filter ~f:(fun (x : Item.t) ->
    not (List.is_empty x.external_deps && List.is_empty x.internal_deps))
;;

let to_dyn ~split_public_names context_name external_resolved_libs =
  let open Dyn in
  Tuple
    [ String context_name; list (Item.to_dyn ~split_public_names) external_resolved_libs ]
;;

let arg_split_public_names =
  let open Arg in
  value
  & flag
  & info
      [ "split-public-names" ]
      ~doc:
        (Some
           "Report private names in the 'names' field and public names in a separate \
            'public_names' field, rather than reporting the public name (falling back to \
            the private name) in 'names'.")
;;

let term =
  let+ builder = Common.Builder.term
  and+ context_name = Common.context_arg ~doc:(Some "Build context to use.")
  and+ _ = Describe_lang_compat.arg
  and+ split_public_names = arg_split_public_names
  and+ format = Describe_format.arg in
  let common, config = Common.init builder in
  Scheduler_setup.go_with_rpc_server ~common ~config
  @@ fun () ->
  Build.build_memo_exn
  @@ fun () ->
  let open Memo.O in
  let* setup = Util.setup () in
  let super_context = Dune_rules.Main.find_scontext_exn setup ~name:context_name in
  let context_name =
    Super_context.context super_context
    |> Context.name
    |> Dune_engine.Context_name.to_string
  in
  external_resolved_libs (Super_context.context super_context)
  >>| to_dyn ~split_public_names context_name
  >>| Describe_format.print_dyn format
;;

let command =
  let doc =
    "Print out external libraries needed to build the project. It's an approximated set \
     of libraries."
  in
  let info = Cmd.info ~doc "external-lib-deps" in
  Cmd.v info term
;;
