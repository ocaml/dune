open Import
module Stanza = Dune_lang.Stanza
open Dune_lang.Decoder

module File_key = struct
  type t = string

  module Map = String.Map

  let of_string s = s
  let to_string s = s

  let make ~name ~root =
    let digest = Digest.generic (name, root) |> Digest.to_string in
    String.take digest 12
  ;;
end

type t =
  { name : Dune_project_name.t
  ; root : Path.Source.t
  ; version : Package_version.t option
  ; dune_version : Dune_lang.Syntax.Version.t
  ; info : Package_info.t
  ; packages : Package.t Package.Name.Map.t
  ; stanza_parser : Stanza.t list Dune_lang.Decoder.t
  ; project_file : Path.Source.t option
  ; extension_args : Univ_map.t
  ; parsing_context : Univ_map.t
  ; implicit_transitive_deps : bool
  ; wrapped_executables : bool
  ; map_workspace_root : bool
  ; executables_implicit_empty_intf : bool
  ; accept_alternative_dune_file_name : bool
  ; generate_opam_files : bool
  ; warnings : Warning.Settings.t
  ; use_standard_c_and_cxx_flags : bool option
  ; file_key : File_key.t
  ; dialects : Dialect.DB.t
  ; explicit_js_mode : bool
  ; format_config : Format_config.t option
  ; subst_config : (Loc.t * Subst_config.t) option
  ; strict_package_deps : bool
  ; allow_approximate_merlin : Loc.t option
  ; sources : Dune_pkg.Pin_stanza.DB.t
  ; cram : bool
  ; expand_aliases_in_sandbox : bool
  ; opam_file_location : [ `Relative_to_project | `Inside_opam_directory ]
  ; including_hidden_packages : Package.t Package.Name.Map.t
  }

let key = Univ_map.Key.create ~name:"dune-project" Dyn.opaque
let get () = Dune_lang.Decoder.get key

let get_exn () =
  get ()
  >>| function
  | Some t -> t
  | None -> Code_error.raise "Current project is unset" []
;;

let equal : t -> t -> bool = phys_equal
let hash = Poly.hash
let packages t = t.packages
let name t = t.name
let root t = t.root
let stanza_parser t = Dune_lang.Decoder.set key t t.stanza_parser
let file t = t.project_file
let file_key t = t.file_key
let implicit_transitive_deps t = t.implicit_transitive_deps
let generate_opam_files t = t.generate_opam_files
let warnings t = t.warnings
let sources t = Dune_pkg.Pin_stanza.DB.add_opam_pins t.sources t.packages
let set_generate_opam_files generate_opam_files t = { t with generate_opam_files }
let use_standard_c_and_cxx_flags t = t.use_standard_c_and_cxx_flags
let dialects t = t.dialects
let explicit_js_mode t = t.explicit_js_mode
let dune_version t = t.dune_version

let to_dyn
  { name
  ; root
  ; version
  ; dune_version
  ; info
  ; project_file
  ; parsing_context = _
  ; extension_args = _
  ; stanza_parser = _
  ; packages
  ; implicit_transitive_deps
  ; wrapped_executables
  ; map_workspace_root
  ; executables_implicit_empty_intf
  ; accept_alternative_dune_file_name
  ; generate_opam_files
  ; warnings
  ; use_standard_c_and_cxx_flags
  ; file_key
  ; dialects
  ; explicit_js_mode
  ; format_config
  ; subst_config
  ; strict_package_deps
  ; allow_approximate_merlin
  ; sources = _
  ; cram
  ; expand_aliases_in_sandbox
  ; opam_file_location
  ; including_hidden_packages = _
  }
  =
  let open Dyn in
  record
    [ "name", Dune_project_name.to_dyn name
    ; "root", Path.Source.to_dyn root
    ; "version", (option Package_version.to_dyn) version
    ; "dune_version", Dune_lang.Syntax.Version.to_dyn dune_version
    ; "info", Package_info.to_dyn info
    ; "project_file", Dyn.option Path.Source.to_dyn project_file
    ; ( "packages"
      , (list (pair Package.Name.to_dyn Package.to_dyn))
          (Package.Name.Map.to_list packages) )
    ; "implicit_transitive_deps", bool implicit_transitive_deps
    ; "wrapped_executables", bool wrapped_executables
    ; "map_workspace_root", bool map_workspace_root
    ; "executables_implicit_empty_intf", bool executables_implicit_empty_intf
    ; "accept_alternative_dune_file_name", bool accept_alternative_dune_file_name
    ; "generate_opam_files", bool generate_opam_files
    ; "warnings", Warning.Settings.to_dyn warnings
    ; "use_standard_c_and_cxx_flags", option bool use_standard_c_and_cxx_flags
    ; "file_key", string file_key
    ; "dialects", Dialect.DB.to_dyn dialects
    ; "explicit_js_mode", bool explicit_js_mode
    ; "format_config", option Format_config.to_dyn format_config
    ; "subst_config", option Toggle.to_dyn (Option.map ~f:snd subst_config)
    ; "strict_package_deps", bool strict_package_deps
    ; "cram", bool cram
    ; "allow_approximate_merlin", opaque allow_approximate_merlin
    ; "expand_aliases_in_sandbox", bool expand_aliases_in_sandbox
    ; ( "opam_file_location"
      , match opam_file_location with
        | `Relative_to_project -> variant "Relative_to_project" []
        | `Inside_opam_directory -> variant "Inside_opam_directory" [] )
    ]
;;

let find_extension_args t key = Univ_map.find t.extension_args key
let is_extension_set t key = Univ_map.mem t.extension_args key

include Dune_lang.Versioned_file.Make (struct
    type t = Stanza.Parser.t list
  end)

let default_dune_language_version =
  ref (Dune_lang.Syntax.greatest_supported_version_exn Stanza.syntax)
;;

let get_dune_lang () =
  { (Lang.get_exn "dune") with version = !default_dune_language_version }
;;

module Extension = struct
  type 'a t = 'a Univ_map.Key.t

  type 'a poly_info =
    { syntax : Dune_lang.Syntax.t
    ; stanzas : ('a * Stanza.Parser.t list) Dune_lang.Decoder.t
    ; key : 'a t
    }

  type packed_extension = Packed : 'a poly_info -> packed_extension

  type info =
    | Extension of packed_extension
    | Deleted_in of Dune_lang.Syntax.Version.t

  type instance =
    { extension : packed_extension
    ; version : Dune_lang.Syntax.Version.t
    ; loc : Loc.t
    ; parse_args :
        (Univ_map.t * Stanza.Parser.t list) Dune_lang.Decoder.t
        -> Univ_map.t * Stanza.Parser.t list
    }

  (* CR-someday amokhov: convert this mutable table to a memoized function,
     which depends on the contents of dune files that declare extensions. *)
  let extensions = Table.create (module String) 32

  let register syntax stanzas arg_to_dyn =
    let name = Dune_lang.Syntax.name syntax in
    if Table.mem extensions name
    then
      Code_error.raise
        "Dune_project.Extension.register: already registered"
        [ "name", Dyn.string name ];
    let key = Univ_map.Key.create ~name arg_to_dyn in
    let ext = { syntax; stanzas; key } in
    Table.add_exn extensions name (Extension (Packed ext));
    key
  ;;

  let register_deleted ~name ~deleted_in =
    Table.add_exn extensions name (Deleted_in deleted_in)
  ;;

  let register_unit syntax stanzas =
    let unit_stanzas =
      let+ r = stanzas in
      (), r
    in
    register syntax unit_stanzas Unit.to_dyn
  ;;

  let register_simple syntax stanzas =
    let (_ : unit t) = register_unit syntax stanzas in
    ()
  ;;

  let instantiate ~dune_lang_ver ~loc ~parse_args (name_loc, name) (ver_loc, ver) =
    match Table.find extensions name with
    | None ->
      User_error.raise
        ~loc:name_loc
        [ Pp.textf "Unknown extension %S." name ]
        ~hints:(User_message.did_you_mean name ~candidates:(Table.keys extensions))
    | Some (Deleted_in v) ->
      User_error.raise
        ~loc
        [ Pp.textf
            "Extension %s was deleted in the %s version of the dune language"
            name
            (Dune_lang.Syntax.Version.to_string v)
        ]
    | Some (Extension (Packed e)) ->
      Dune_lang.Syntax.check_supported ~dune_lang_ver e.syntax (ver_loc, ver);
      { extension = Packed e; version = ver; loc; parse_args }
  ;;

  type automatic =
    | Selected of instance
    | Not_selected of packed_extension

  let automatic ~explicitly_selected : automatic list =
    Table.foldi extensions ~init:[] ~f:(fun name extension acc ->
      match String.Map.find explicitly_selected name with
      | Some instance -> Selected instance :: acc
      | None ->
        (match extension with
         | Deleted_in _ -> acc
         | Extension e -> Not_selected e :: acc))
  ;;
end

module Melange_syntax = struct
  let name = "melange"
end

let explicit_extensions_map explicit_extensions =
  match
    String.Map.of_list
      (List.map explicit_extensions ~f:(fun (e : Extension.instance) ->
         let syntax =
           let (Packed e) = e.extension in
           e.syntax
         in
         Dune_lang.Syntax.name syntax, e))
  with
  | Error (name, _, ext) ->
    User_error.raise
      ~loc:ext.loc
      [ Pp.textf "Extension %S specified for the second time." name ]
  | Ok map -> map
;;

let interpret_lang_and_extensions ~(lang : Lang.Instance.t) ~explicit_extensions =
  let extensions = Extension.automatic ~explicitly_selected:explicit_extensions in
  let parsing_context =
    let init =
      let init =
        Univ_map.singleton (Dune_lang.Syntax.key lang.syntax) (Active lang.version)
      in
      Univ_map.set init String_with_vars.decoding_env_key (Pform.Env.initial lang.version)
    in
    List.fold_left extensions ~init ~f:(fun acc (ext : Extension.automatic) ->
      let syntax =
        let (Extension.Packed ext) =
          match ext with
          | Selected e -> e.extension
          | Not_selected e -> e
        in
        ext.syntax
      in
      let status : Dune_lang.Syntax.Key.t =
        match ext with
        | Selected ext -> Active ext.version
        | Not_selected (Packed e) ->
          Inactive { lang = e.syntax; dune_lang_ver = lang.version }
      in
      Univ_map.set acc (Dune_lang.Syntax.key syntax) status)
  in
  let extension_args, extension_stanzas =
    List.fold_left
      extensions
      ~init:(Univ_map.empty, [])
      ~f:(fun (args_acc, stanzas_acc) (ext : Extension.automatic) ->
        match ext with
        | Not_selected (Packed e) ->
          let stanzas =
            let open Dune_lang.Decoder in
            let stanzas =
              match Dune_lang.Syntax.greatest_supported_version e.syntax with
              | None -> []
              | Some greatest_supported_version ->
                let parsing_context =
                  (* Temporarily mark the extension as active so that we can
                     call the parser and extract the list of stanza names this
                     extension registers *)
                  Univ_map.set
                    parsing_context
                    (Dune_lang.Syntax.key e.syntax)
                    (Active greatest_supported_version)
                in
                parse (enter e.stanzas) parsing_context (List (Loc.of_pos __POS__, []))
                |> snd
            in
            List.map stanzas ~f:(fun (name, _) ->
              ( name
              , let+ _ = Dune_lang.Syntax.get_exn e.syntax in
                (* The above [get_exn] will raise because the extension is
                   inactive *)
                assert false ))
          in
          args_acc, stanzas :: stanzas_acc
        | Selected instance ->
          let (Packed e) = instance.extension in
          let args_acc, stanzas =
            let args =
              let+ arg, stanzas = Dune_lang.Decoder.set_many parsing_context e.stanzas in
              Univ_map.set args_acc e.key arg, stanzas
            in
            instance.parse_args args
          in
          args_acc, stanzas :: stanzas_acc)
  in
  let stanzas = List.concat (lang.data :: extension_stanzas) in
  let stanza_parser = Dune_lang.Decoder.(set_many parsing_context (sum stanzas)) in
  parsing_context, stanza_parser, extension_args
;;

let filename = "dune-project"
let opam_file_location_default ~lang:_ = `Relative_to_project
let implicit_transitive_deps_default ~lang:_ = true
let wrapped_executables_default ~(lang : Lang.Instance.t) = lang.version >= (2, 0)
let map_workspace_root_default ~(lang : Lang.Instance.t) = lang.version >= (3, 0)

let executables_implicit_empty_intf_default ~(lang : Lang.Instance.t) =
  lang.version >= (3, 0)
;;

let strict_package_deps_default ~lang:_ = false
let explicit_js_mode_default ~(lang : Lang.Instance.t) = lang.version >= (2, 0)

let accept_alternative_dune_file_name_default ~(lang : Lang.Instance.t) =
  lang.version >= (3, 0)
;;

let cram_default ~(lang : Lang.Instance.t) = lang.version >= (3, 0)
let expand_aliases_in_sandbox_default ~lang:_ = false

let use_standard_c_and_cxx_flags_default ~(lang : Lang.Instance.t) =
  if lang.version >= (3, 0) then Some true else None
;;

let format_extension_key =
  Extension.register Format_config.syntax Format_config.dparse_args Format_config.to_dyn
;;

let format_config t =
  let ext = find_extension_args t format_extension_key in
  let dune_lang = t.format_config in
  let version = dune_version t in
  Format_config.of_config ~ext ~dune_lang ~version
;;

let subst_config t =
  let loc, subst_config =
    match t.subst_config with
    | None -> Loc.none, None
    | Some (loc, config) -> loc, Some config
  in
  loc, Subst_config.of_config subst_config
;;

let default_name ~dir ~(packages : Package.t Package.Name.Map.t) =
  match
    (* CR-rgrinberg: why do we pick a name randomly? How about just making it
       anonymous here *)
    Package.Name.Map.min_binding packages
  with
  | None -> Dune_project_name.anonymous dir
  | Some (name, pkg) ->
    let loc = Package.loc pkg in
    (* TODO: This is a strange error: [name] comes from a package but is
       rejected as a valid Dune project name. It would be better to make the
       set of allowed package names and the set of project names coincide. *)
    Dune_project_name.named loc (Package.Name.to_string name)
;;

let infer ~dir info packages =
  let lang = get_dune_lang () in
  let name = default_name ~dir ~packages in
  let parsing_context, stanza_parser, extension_args =
    interpret_lang_and_extensions ~lang ~explicit_extensions:String.Map.empty
  in
  let implicit_transitive_deps = implicit_transitive_deps_default ~lang in
  let wrapped_executables = wrapped_executables_default ~lang in
  let map_workspace_root = map_workspace_root_default ~lang in
  let executables_implicit_empty_intf = executables_implicit_empty_intf_default ~lang in
  let explicit_js_mode = explicit_js_mode_default ~lang in
  let strict_package_deps = strict_package_deps_default ~lang in
  let cram = cram_default ~lang in
  let expand_aliases_in_sandbox = expand_aliases_in_sandbox_default ~lang in
  let root = dir in
  let file_key = File_key.make ~root ~name in
  let opam_file_location = opam_file_location_default ~lang in
  { name
  ; allow_approximate_merlin = None
  ; sources = Dune_pkg.Pin_stanza.DB.empty
  ; packages
  ; root
  ; info
  ; version = None
  ; dune_version = lang.version
  ; implicit_transitive_deps
  ; wrapped_executables
  ; map_workspace_root
  ; executables_implicit_empty_intf
  ; accept_alternative_dune_file_name = false
  ; stanza_parser
  ; project_file = None
  ; extension_args
  ; parsing_context
  ; generate_opam_files = false
  ; warnings = Warning.Settings.empty
  ; use_standard_c_and_cxx_flags = use_standard_c_and_cxx_flags_default ~lang
  ; file_key
  ; dialects = Dialect.DB.builtin
  ; explicit_js_mode
  ; format_config = None
  ; subst_config = None
  ; strict_package_deps
  ; cram
  ; expand_aliases_in_sandbox
  ; opam_file_location
  ; including_hidden_packages = packages
  }
;;

let anonymous ~dir info packages = infer ~dir info packages

let encode : t -> Dune_lang.t list =
  fun { name
      ; allow_approximate_merlin = _
      ; sources
      ; version
      ; dune_version
      ; info
      ; packages
      ; implicit_transitive_deps
      ; wrapped_executables
      ; map_workspace_root
      ; executables_implicit_empty_intf
      ; accept_alternative_dune_file_name
      ; generate_opam_files
      ; warnings = _
      ; use_standard_c_and_cxx_flags
      ; dialects
      ; explicit_js_mode
      ; format_config
      ; strict_package_deps
      ; cram
      ; subst_config
        (* The next three fields all get parsed out from the `using` stanza, but
           we don't need them for project initialization. They should be
           reconstructed if you ever want a full encoding of the project record
           tho. *)
      ; extension_args = _
      ; parsing_context = _
      ; stanza_parser =
          _
          (* The next three fields hold metadata that is about the dune-project
             file, but not represented in its content *)
      ; file_key = _
      ; project_file = _
      ; root = _
      ; expand_aliases_in_sandbox
      ; opam_file_location = _
      ; including_hidden_packages = _
      } ->
  let open Dune_lang.Encoder in
  let lang = Lang.get_exn "dune" in
  let flags =
    let flag name value default =
      if Bool.equal value (default ~lang) then None else Some (constr name bool value)
    in
    (* Flags that don't take a boolean for some reason *)
    let flag' name v default =
      if v && not (Bool.equal (default ~lang) v)
      then Some (list string [ name ])
      else None
    in
    List.filter_opt
      [ flag "generate_opam_files" generate_opam_files (fun ~lang:_ ->
          not generate_opam_files)
      ; flag
          "implicit_transitive_deps"
          implicit_transitive_deps
          implicit_transitive_deps_default
      ; flag "wrapped_executables" wrapped_executables wrapped_executables_default
      ; flag "map_workspace_root" map_workspace_root map_workspace_root_default
      ; flag
          "executables_implicit_empty_intf"
          executables_implicit_empty_intf
          executables_implicit_empty_intf_default
      ; flag "strict_package_deps" strict_package_deps strict_package_deps_default
      ; flag'
          "accept_alternative_dune_file_name"
          accept_alternative_dune_file_name
          accept_alternative_dune_file_name_default
      ; flag' "explicit_js_mode" explicit_js_mode explicit_js_mode_default
        (* Two other ways of dealing with flags *)
      ; (match use_standard_c_and_cxx_flags with
         | None -> None
         | Some b ->
           if not
                (Option.equal
                   Bool.equal
                   (Some b)
                   (use_standard_c_and_cxx_flags_default ~lang))
           then Some (constr "use_standard_c_and_cxx_flags" bool b)
           else None)
      ; (if Bool.equal cram (cram_default ~lang)
         then None
         else Some (constr "cram" Toggle.encode (Toggle.of_bool cram)))
      ; flag
          "expand_aliases_in_sandbox"
          expand_aliases_in_sandbox
          expand_aliases_in_sandbox_default
      ]
  in
  let lang_stanza =
    list
      sexp
      [ string "lang"; string "dune"; Dune_lang.Syntax.Version.encode dune_version ]
  in
  let dialects =
    if Dialect.DB.is_default dialects
    then []
    else Dialect.DB.fold ~f:(fun d ls -> Dialect.encode d :: ls) ~init:[] dialects
  in
  let formatting =
    Option.bind format_config ~f:Format_config.encode_opt |> Option.to_list
  in
  let packages =
    Package.Name.Map.to_list_map packages ~f:(fun name package ->
      Package.encode name package)
  in
  let subst_config =
    Option.map subst_config ~f:(fun (_loc, x) -> constr "subst" Subst_config.encode x)
    |> Option.to_list
  in
  let name = constr "name" Dune_project_name.encode name in
  let version =
    Option.map ~f:(constr "version" Package_version.encode) version |> Option.to_list
  in
  let sources = Dune_pkg.Pin_stanza.DB.encode sources in
  List.concat
    [ [ lang_stanza; name ]
    ; flags
    ; version
    ; Package_info.encode_fields info
    ; formatting
    ; dialects
    ; packages
    ; subst_config
    ; sources
    ]
;;

module Memo_package_name = Memo.Make_parallel_map (Package.Name.Map)

let forbid_opam_files_relative_to_project opam_file_location packages =
  match opam_file_location with
  | `Relative_to_project -> ()
  | `Inside_opam_directory ->
    if not (Package.Name.Map.is_empty packages)
    then
      User_error.raise
        [ Pp.text
            "When (opam_file_location inside_opam_directory) is set, all opam files must \
             live in the opam/ subdirecotry. The following opam files must be moved:"
        ; Pp.enumerate (Package.Name.Map.values packages) ~f:(fun ((loc : Loc.t), _) ->
            Pp.text (Loc.start loc).pos_fname)
        ]
;;

let parse_packages
  (name : Dune_project_name.t option)
  ~info
  ~dir
  ~version
  packages
  opam_file_location
  ~generate_opam_files
  opam_packages
  =
  let open Memo.O in
  let+ packages =
    forbid_opam_files_relative_to_project opam_file_location opam_packages;
    if List.is_empty packages
    then
      Package.Name.Map.to_list opam_packages
      |> Memo.parallel_map ~f:(fun (name, (_loc, pkg)) ->
        let open Memo.O in
        let+ pkg = pkg in
        name, pkg)
      |> Memo.map ~f:Package.Name.Map.of_list_exn
    else (
      (match packages, Option.bind ~f:Dune_project_name.name name with
       | [ p ], Some name ->
         if Package.Name.to_string (Package.name p) <> name
         then
           User_error.raise
             ~loc:(Package.loc p)
             [ Pp.textf
                 "when a single package is defined, it must have the same name as the \
                  project name: %s"
                 name
             ]
       | _, _ -> ());
      let package_defined_twice name loc1 loc2 =
        let main_message =
          [ Pp.textf "Package name %s is defined twice:" (Package.Name.to_string name) ]
        in
        let name = Package.Name.to_string name in
        let annots =
          let message loc = User_message.make ~loc [ Pp.textf "package named %s" name ] in
          let related = [ message loc1; message loc2 ] in
          User_message.Annots.singleton
            Compound_user_error.annot
            [ Compound_user_error.make ~main:(User_message.make main_message) ~related ]
        in
        User_error.raise
          ~annots
          (main_message
           @ [ Pp.textf "- %s" (Loc.to_file_colon_line loc1)
             ; Pp.textf "- %s" (Loc.to_file_colon_line loc2)
             ])
      in
      let deprecated_package_names =
        List.fold_left packages ~init:Package.Name.Map.empty ~f:(fun acc package ->
          let deprecated_package_names = Package.deprecated_package_names package in
          Package.Name.Map.union acc deprecated_package_names ~f:package_defined_twice)
      in
      List.iter packages ~f:(fun p ->
        let name = Package.name p in
        match Package.Name.Map.find deprecated_package_names name with
        | None -> ()
        | Some loc -> package_defined_twice name loc (Package.loc p));
      match Package.Name.Map.of_list_map packages ~f:(fun p -> Package.name p, p) with
      | Error (_, _, p) ->
        let name = Package.name p in
        User_error.raise
          ~loc:(Package.loc p)
          [ Pp.textf "package %s is already defined" (Package.Name.to_string name) ]
      | Ok packages ->
        Memo.return
        @@
        let generated_opam_file =
          if generate_opam_files
          then fun p -> Package.set_has_opam_file p Package.Generated
          else Fun.id
        in
        (match opam_file_location with
         | `Inside_opam_directory ->
           Package.Name.Map.map packages ~f:(fun p ->
             let dir = Path.Source.relative dir "opam" in
             let p = Package.set_inside_opam_dir p ~dir in
             generated_opam_file p)
         | `Relative_to_project ->
           Package.Name.Map.merge packages opam_packages ~f:(fun _name dune opam ->
             match dune, opam with
             | None, None -> assert false
             | Some p, None -> Some (generated_opam_file p)
             | Some p, Some _ ->
               let p = Package.set_has_opam_file p (Exists true) in
               Some (generated_opam_file p)
             | None, Some (loc, _) ->
               User_error.raise
                 ~loc
                 [ Pp.text
                     "This opam file doesn't have a corresponding (package ...) stanza \
                      in the dune-project file. Since you have at least one other \
                      (package ...) stanza in your dune-project file, you must a \
                      (package ...) stanza for each opam package in your project."
                 ])))
  in
  Package.Name.Map.map packages ~f:(fun p ->
    let info = Package_info.superpose info (Package.info p) in
    let version =
      match Package.version p with
      | Some _ as v -> v
      | None -> version
    in
    Package.set_version_and_info p ~version ~info)
;;

let parse ~dir ~(lang : Lang.Instance.t) ~file =
  String_with_vars.set_decoding_env (Pform.Env.initial lang.version)
  @@ fields
  @@ let+ name = field_o "name" Dune_project_name.decode
     and+ version = field_o "version" Package_version.decode
     and+ info = Package_info.decode ()
     and+ packages = multi_field "package" (Package.decode ~dir)
     and+ sources = Dune_pkg.Pin_stanza.DB.decode ~dir
     and+ explicit_extensions =
       multi_field
         "using"
         (let+ loc = loc
          and+ name = located string
          and+ ver = located Dune_lang.Syntax.Version.decode
          and+ parse_args = capture in
          (* We don't parse the arguments quite yet as we want to set the
             version of extensions before parsing them. *)
          Extension.instantiate ~dune_lang_ver:lang.version ~loc ~parse_args name ver)
     and+ implicit_transitive_deps =
       field_o_b
         "implicit_transitive_deps"
         ~check:(Dune_lang.Syntax.since Stanza.syntax (1, 7))
     and+ wrapped_executables =
       field_o_b
         "wrapped_executables"
         ~check:(Dune_lang.Syntax.since Stanza.syntax (1, 11))
     and+ map_workspace_root =
       field_o_b "map_workspace_root" ~check:(Dune_lang.Syntax.since Stanza.syntax (3, 7))
     and+ allow_approximate_merlin =
       (* TODO DUNE4 remove this field from parsing *)
       let+ loc = loc
       and+ field =
         field_b
           "allow_approximate_merlin"
           ~check:(Dune_lang.Syntax.since Stanza.syntax (1, 9))
       in
       Option.some_if field loc
     and+ executables_implicit_empty_intf =
       field_o_b
         "executables_implicit_empty_intf"
         ~check:(Dune_lang.Syntax.since Stanza.syntax (2, 9))
     and+ accept_alternative_dune_file_name =
       field_b
         "accept_alternative_dune_file_name"
         ~check:(Dune_lang.Syntax.since Stanza.syntax (3, 0))
     and+ () = Dune_lang.Versioned_file.no_more_lang
     and+ generate_opam_files =
       field_o_b
         "generate_opam_files"
         ~check:(Dune_lang.Syntax.since Stanza.syntax (1, 10))
     and+ use_standard_c_and_cxx_flags =
       field_o_b
         "use_standard_c_and_cxx_flags"
         ~check:(Dune_lang.Syntax.since Stanza.syntax (2, 8))
     and+ dialects =
       multi_field
         "dialect"
         (Dune_lang.Syntax.since Stanza.syntax (1, 11) >>> located Dialect.decode)
     and+ explicit_js_mode =
       field_o_b "explicit_js_mode" ~check:(Dune_lang.Syntax.since Stanza.syntax (1, 11))
     and+ format_config = Format_config.field ~since:(2, 0)
     and+ subst_config = Subst_config.field
     and+ strict_package_deps =
       field_o_b
         "strict_package_deps"
         ~check:(Dune_lang.Syntax.since Stanza.syntax (2, 3))
     and+ cram = Toggle.field "cram" ~check:(Dune_lang.Syntax.since Stanza.syntax (2, 7))
     and+ expand_aliases_in_sandbox =
       field_o_b
         "expand_aliases_in_sandbox"
         ~check:(Dune_lang.Syntax.since Stanza.syntax (3, 0))
     and+ opam_file_location =
       field_o
         "opam_file_location"
         (Dune_lang.Syntax.since Stanza.syntax (3, 8)
          >>> enum
                [ "relative_to_project", `Relative_to_project
                ; "inside_opam_directory", `Inside_opam_directory
                ])
     and+ warnings =
       field
         "warnings"
         ~default:Warning.Settings.empty
         (Dune_lang.Syntax.since Stanza.syntax (3, 11) >>> Warning.Settings.decode)
     in
     fun (opam_packages : (Loc.t * Package.t Memo.t) Package.Name.Map.t) ->
       let opam_file_location =
         Option.value opam_file_location ~default:(opam_file_location_default ~lang)
       in
       let generate_opam_files = Option.value ~default:false generate_opam_files in
       let open Memo.O in
       let+ packages =
         parse_packages
           name
           ~info
           ~dir
           ~version
           packages
           opam_file_location
           ~generate_opam_files
           opam_packages
       in
       let name =
         match name with
         | Some n -> n
         | None -> default_name ~dir ~packages
       in
       let explicit_extensions = explicit_extensions_map explicit_extensions in
       let parsing_context, stanza_parser, extension_args =
         interpret_lang_and_extensions ~lang ~explicit_extensions
       in
       let implicit_transitive_deps =
         Option.value
           implicit_transitive_deps
           ~default:(implicit_transitive_deps_default ~lang)
       in
       let wrapped_executables =
         Option.value wrapped_executables ~default:(wrapped_executables_default ~lang)
       in
       let map_workspace_root =
         Option.value map_workspace_root ~default:(map_workspace_root_default ~lang)
       in
       let executables_implicit_empty_intf =
         Option.value
           executables_implicit_empty_intf
           ~default:(executables_implicit_empty_intf_default ~lang)
       in
       let strict_package_deps =
         Option.value strict_package_deps ~default:(strict_package_deps_default ~lang)
       in
       let dune_version = lang.version in
       let explicit_js_mode =
         Option.value explicit_js_mode ~default:(explicit_js_mode_default ~lang)
       in
       let use_standard_c_and_cxx_flags =
         match use_standard_c_and_cxx_flags with
         | None -> use_standard_c_and_cxx_flags_default ~lang
         | some -> some
       in
       let cram =
         match cram with
         | None -> cram_default ~lang
         | Some t -> Toggle.enabled t
       in
       let expand_aliases_in_sandbox =
         Option.value
           expand_aliases_in_sandbox
           ~default:(expand_aliases_in_sandbox_default ~lang)
       in
       let root = dir in
       let file_key = File_key.make ~name ~root in
       let dialects =
         let dialects =
           match String.Map.find explicit_extensions Melange_syntax.name with
           | Some extension -> (extension.loc, Dialect.rescript) :: dialects
           | None -> dialects
         in
         List.fold_left
           dialects
           ~init:Dialect.DB.builtin
           ~f:(fun dialects (loc, dialect) -> Dialect.DB.add dialects ~loc dialect)
       in
       { name
       ; file_key
       ; root
       ; version
       ; dune_version
       ; info
       ; packages
       ; stanza_parser
       ; project_file = Some file
       ; extension_args
       ; parsing_context
       ; implicit_transitive_deps
       ; wrapped_executables
       ; map_workspace_root
       ; executables_implicit_empty_intf
       ; accept_alternative_dune_file_name
       ; generate_opam_files
       ; warnings
       ; use_standard_c_and_cxx_flags
       ; dialects
       ; explicit_js_mode
       ; format_config
       ; subst_config
       ; strict_package_deps
       ; allow_approximate_merlin
       ; sources
       ; cram
       ; expand_aliases_in_sandbox
       ; opam_file_location
       ; including_hidden_packages = packages
       }
;;

let load_dune_project ~read ~dir opam_packages : t Memo.t =
  let file = Path.Source.relative dir filename in
  let open Memo.O in
  let* lexbuf =
    let+ contents = read file in
    Lexbuf.from_string contents ~fname:(Path.Source.to_string file)
  in
  parse_contents lexbuf ~f:(fun lang -> parse ~dir ~lang ~file) opam_packages
;;

let gen_load ~read ~dir ~files ~infer_from_opam_files : t option Memo.t =
  let open Memo.O in
  let opam_packages =
    Filename.Set.fold files ~init:[] ~f:(fun fn acc ->
      match Package.Name.of_opam_file_basename fn with
      | None -> acc
      | Some name ->
        let opam_file = Path.Source.relative dir fn in
        let loc = Loc.in_file (Path.source opam_file) in
        let pkg =
          let+ contents = read opam_file in
          Package.load_opam_file_with_contents ~contents opam_file name
        in
        (name, (loc, pkg)) :: acc)
    |> Package.Name.Map.of_list_exn
  in
  if Filename.Set.mem files filename
  then load_dune_project ~read ~dir opam_packages >>| Option.some
  else if infer_from_opam_files && not (Package.Name.Map.is_empty opam_packages)
  then
    let+ opam_packages =
      Memo_package_name.parallel_map opam_packages ~f:(fun _ (_loc, pkg) -> pkg)
    in
    Some (infer Package_info.empty ~dir opam_packages)
  else Memo.return None
;;

let load =
  let read source = Fs_memo.file_contents (Path.Outside_build_dir.In_source_dir source) in
  gen_load ~read
;;

let set_parsing_context t parser =
  let parsing_context = Univ_map.set t.parsing_context key t in
  Dune_lang.Decoder.set_many parsing_context parser
;;

let wrapped_executables t = t.wrapped_executables
let map_workspace_root t = t.map_workspace_root
let executables_implicit_empty_intf t = t.executables_implicit_empty_intf
let accept_alternative_dune_file_name t = t.accept_alternative_dune_file_name
let () = Extension.register_simple Dune_lang.Action.Action_plugin.syntax (return [])
let dune_site_extension = Extension.register_unit Site.dune_site_syntax (return [])
let strict_package_deps t = t.strict_package_deps
let allow_approximate_merlin t = t.allow_approximate_merlin
let cram t = t.cram
let info t = t.info

let update_execution_parameters t ep =
  ep
  |> Execution_parameters.set_expand_aliases_in_sandbox t.expand_aliases_in_sandbox
  |> Execution_parameters.set_workspace_root_to_build_path_prefix_map
       (if t.map_workspace_root then Set "/workspace_root" else Unset)
  |> Execution_parameters.set_should_remove_write_permissions_on_generated_files
       (t.dune_version >= (2, 4))
;;

let opam_file_location t = t.opam_file_location

let filter_packages t ~f =
  let packages = Package.Name.Map.filter t.packages ~f:(fun p -> f (Package.name p)) in
  { t with packages }
;;

let including_hidden_packages t = t.including_hidden_packages
