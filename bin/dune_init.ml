open Import

(** Because the dune_init utility deals with the addition of stanzas and fields
    to dune projects and files, we need to inspect and manipulate the concrete
    syntax tree (CST) a good deal. *)
module Cst = Dune_lang.Cst

(** Abstractions around the kinds of files handled during initialization *)
module File = struct
  type dune =
    { path : Path.t
    ; name : string
    ; content : Cst.t list
    }

  type text =
    { path : Path.t
    ; name : string
    ; content : string
    }

  type t =
    | Dune of dune
    | Text of text

  let make_text path name content = Text { path; name; content }

  let full_path = function
    | Dune { path; name; _ } | Text { path; name; _ } -> Path.relative path name
  ;;

  (** Inspection and manipulation of stanzas in a file *)
  module Stanza = struct
    let pp s =
      match Cst.to_sexp s with
      | None -> Pp.nop
      | Some s -> Dune_lang.pp s
    ;;

    let libraries_conflict (a : Library.t) (b : Library.t) = a.name = b.name

    let executables_conflict (a : Dune_rules.Executables.t) (b : Dune_rules.Executables.t)
      =
      let a_names = String.Set.of_list_map ~f:snd (Nonempty_list.to_list a.names) in
      let b_names = String.Set.of_list_map ~f:snd (Nonempty_list.to_list b.names) in
      String.Set.inter a_names b_names |> String.Set.is_empty |> not
    ;;

    let tests_conflict (a : Dune_rules.Tests.t) (b : Dune_rules.Tests.t) =
      executables_conflict a.exes b.exes
    ;;

    let stanzas_conflict (a : Stanza.t) (b : Stanza.t) =
      match Stanza.repr a, Stanza.repr b with
      | Dune_rules.Executables.T a, Dune_rules.Executables.T b -> executables_conflict a b
      | Library.T a, Library.T b -> libraries_conflict a b
      | Dune_rules.Tests.T a, Dune_rules.Tests.T b -> tests_conflict a b
      (* NOTE No other stanza types currently supported *)
      | _ -> false
    ;;

    let csts_conflict project (a : Cst.t) (b : Cst.t) =
      let of_ast = Dune_rules.Stanzas.of_ast project in
      (let open Option.O in
       let* a_ast = Cst.abstract a in
       let+ b_ast = Cst.abstract b in
       let a_asts = of_ast a_ast in
       let b_asts = of_ast b_ast in
       List.exists ~f:(fun x -> List.exists ~f:(stanzas_conflict x) a_asts) b_asts)
      |> Option.value ~default:false
    ;;

    (* TODO(shonfeder): replace with stanza merging *)
    let find_conflicting project new_stanzas existing_stanzas =
      let conflicting_stanza stanza =
        match List.find ~f:(csts_conflict project stanza) existing_stanzas with
        | Some conflict -> Some (stanza, conflict)
        | None -> None
      in
      List.find_map ~f:conflicting_stanza new_stanzas
    ;;

    let add (project : Dune_project.t) stanzas = function
      | Text f -> Text f (* Adding a stanza to a text file isn't meaningful *)
      | Dune f ->
        (match find_conflicting project stanzas f.content with
         | None -> Dune { f with content = f.content @ stanzas }
         | Some (a, b) ->
           User_error.raise
             [ Pp.text "Updating existing stanzas is not yet supported."
             ; Pp.text "A preexisting dune stanza conflicts with a generated stanza:"
             ; Pp.nop
             ; Pp.text "Generated stanza:"
             ; pp a
             ; Pp.nop
             ; Pp.text "Pre-existing stanza:"
             ; pp b
             ])
    ;;
  end

  (* Stanza *)

  let create_dir path =
    try Path.mkdir_p path with
    | Unix.Unix_error (EACCES, _, _) ->
      User_error.raise
        [ Pp.textf
            "A project directory cannot be created or accessed: Lacking permissions \
             needed to create directory %s"
            (Path.to_string_maybe_quoted path)
        ]
  ;;

  let load_dune_file ~path =
    let name = "dune" in
    let full_path = Path.relative path name in
    let content =
      if not (Path.exists full_path)
      then []
      else if Path.is_directory full_path
      then
        User_error.raise
          [ Pp.textf
              "\"%s\" already exists and is a directory"
              (Path.to_absolute_filename full_path)
          ]
      else (
        match Io.with_lexbuf_from_file ~f:Dune_lang.Format.parse full_path with
        | Dune_lang.Format.Sexps content -> content
        | Dune_lang.Format.OCaml_syntax _ ->
          User_error.raise
            [ Pp.textf
                "Cannot load dune file %s because it uses OCaml syntax"
                (Path.to_string_maybe_quoted full_path)
            ])
    in
    Dune { path; name; content }
  ;;

  let write_dune_file (dune_file : dune) =
    let path = Path.relative dune_file.path dune_file.name in
    let version =
      Dune_lang.Syntax.greatest_supported_version_exn Dune_lang.Stanza.syntax
    in
    Io.with_file_out
      ~binary:true
      (* Why do we pass [~binary:true] but not anywhere else when formatting? *)
      path
      ~f:(fun oc ->
        let fmt = Format.formatter_of_out_channel oc in
        Format.fprintf
          fmt
          "%a%!"
          Pp.to_fmt
          (Dune_lang.Format.pp_top_sexps ~version dune_file.content))
  ;;

  let write f =
    let path = full_path f in
    match f with
    | Dune f -> Ok (write_dune_file f)
    | Text f ->
      if Path.exists path
      then Error path
      else Ok (Io.write_file ~binary:false path f.content)
  ;;
end

(** The context in which the initialization is executed *)
module Init_context = struct
  open Dune_config_file

  type t =
    { dir : Path.t
    ; project : Dune_project.t
    ; defaults : Dune_config.Project_defaults.t
    }

  let make path defaults =
    let open Memo.O in
    let+ project =
      (* CR-rgrinberg: why not get the project from the source tree? *)
      Dune_project.load
        ~dir:Path.Source.root
        ~files:Filename.Set.empty
        ~infer_from_opam_files:true
      >>| function
      | Some p -> p
      | None ->
        Dune_project.anonymous
          ~dir:Path.Source.root
          Package_info.empty
          Package.Name.Map.empty
    in
    let dir =
      match path with
      | None -> Path.root
      | Some p -> Path.of_string p
    in
    File.create_dir dir;
    { dir; project; defaults }
  ;;
end

let check_module_name name =
  let s = Dune_lang.Atom.to_string name in
  let (_ : Dune_rules.Module_name.t) =
    Dune_rules.Module_name.of_string_user_error (Loc.none, s) |> User_error.ok_exn
  in
  ()
;;

module Public_name = struct
  include Lib_name
  module Pkg = Dune_lang.Package_name.Opam_compatible

  let is_opam_compatible l =
    Lib_name.package_name l |> Dune_lang.Package_name.is_opam_compatible
  ;;

  let of_string_user_error (loc, s) =
    let open Result.O in
    let* l = of_string_user_error (loc, s) in
    if is_opam_compatible l
    then Ok l
    else
      Error
        (User_error.make
           [ Pp.text
               "Public names are composed of an opam package name and optional \
                dot-separated string suffixes."
           ; Pkg.description_of_valid_string
           ])
  ;;

  let of_name_exn name =
    let s = Dune_lang.Atom.to_string name in
    of_string_user_error (Loc.none, s) |> User_error.ok_exn
  ;;
end

module Component = struct
  module Options = struct
    module Common = struct
      type t =
        { name : Dune_lang.Atom.t
        ; libraries : Dune_lang.Atom.t list
        ; pps : Dune_lang.Atom.t list
        }
    end

    module Executable = struct
      type t = { public : Public_name.t option }
    end

    module Library = struct
      type t =
        { public : Public_name.t option
        ; inline_tests : bool
        }
    end

    module Project = struct
      module Template = struct
        type t =
          | Exec
          | Lib

        let of_string = function
          | "executable" -> Some Exec
          | "library" -> Some Lib
          | _ -> None
        ;;

        let commands = [ "executable", Exec; "library", Lib ]
      end

      module Pkg = struct
        type t =
          | Opam
          | Esy

        let commands = [ "opam", Opam; "esy", Esy ]
      end

      type t =
        { template : Template.t
        ; inline_tests : bool
        ; pkg : Pkg.t
        }
    end

    module Test = struct
      type t = unit
    end

    type 'options t =
      { context : Init_context.t
      ; common : Common.t
      ; options : 'options
      }
  end

  (* Options *)

  type 'options t =
    | Executable : Options.Executable.t Options.t -> Options.Executable.t t
    | Library : Options.Library.t Options.t -> Options.Library.t t
    | Project : Options.Project.t Options.t -> Options.Project.t t
    | Test : Options.Test.t Options.t -> Options.Test.t t

  (** Internal representation of the files comprising a component *)
  type target =
    { dir : Path.t
    ; files : File.t list
    }

  (** Creates Dune language CST stanzas describing components *)
  module Stanza_cst = struct
    open Dune_lang

    module Field = struct
      let inline_tests = Encoder.field_b "inline_tests"
      let pps_encoder pps = Encoder.list Encoder.string ("pps" :: pps)

      let preprocess_field = function
        | [] -> []
        | pps -> [ Encoder.field "preprocess" pps_encoder pps ]
      ;;

      let common (options : Options.Common.t) =
        [ Encoder.field "name" Encoder.string (Atom.to_string options.name)
        ; Encoder.field_l
            "libraries"
            Encoder.string
            (List.map ~f:Atom.to_string options.libraries)
        ]
        @ preprocess_field (List.map ~f:Atom.to_string options.pps)
      ;;
    end

    (* Make CST representation of a stanza for the given `kind` *)
    let make kind common_options fields =
      Encoder.named_record_fields kind (fields @ Field.common common_options)
      (* Convert to a CST *)
      |> Dune_lang.Ast.add_loc ~loc:Loc.none
      |> Cst.concrete
      (* Package as a list CSTs *) |> List.singleton
    ;;

    let add_to_list_set elem set =
      if List.mem ~equal:Dune_lang.Atom.equal set elem then set else elem :: set
    ;;

    let public_name_field = Encoder.field_o "public_name" Public_name.encode

    let executable (common : Options.Common.t) (options : Options.Executable.t) =
      make "executable" common [ public_name_field options.public ]
    ;;

    let library (common : Options.Common.t) { Options.Library.inline_tests; public } =
      check_module_name common.name;
      let common =
        if inline_tests
        then (
          let pps =
            add_to_list_set (Dune_lang.Atom.of_string "ppx_inline_test") common.pps
          in
          { common with pps })
        else common
      in
      make "library" common [ public_name_field public; Field.inline_tests inline_tests ]
    ;;

    let test common (() : Options.Test.t) = make "test" common []

    (* A list of CSTs for dune-project file content *)
    let dune_project
      ~opam_file_gen
      ~(defaults : Dune_config_file.Dune_config.Project_defaults.t)
      dir
      (common : Options.Common.t)
      =
      let cst =
        let package =
          Package.create
            ~name:(Package.Name.of_string (Atom.to_string common.name))
            ~loc:Loc.none
            ~version:None
            ~conflicts:[]
            ~depopts:[]
            ~info:Package_info.empty
            ~sites:Site.Map.empty
            ~allow_empty:false
            ~deprecated_package_names:Package.Name.Map.empty
            ~has_opam_file:(Exists false)
            ~original_opam_file:None
            ~dir
            ~synopsis:(Some "A short synopsis")
            ~description:(Some "A longer description")
            ~tags:[ "add topics"; "to describe"; "your"; "project" ]
            ~depends:
              [ { Package_dependency.name = Package.Name.of_string "ocaml"
                ; constraint_ = None
                }
              ]
        in
        let packages = Package.Name.Map.singleton (Package.name package) package in
        let info =
          Package_info.example
            ~authors:defaults.authors
            ~maintainers:defaults.maintainers
            ~license:defaults.license
        in
        Dune_project.anonymous ~dir info packages
        |> Dune_project.set_generate_opam_files opam_file_gen
        |> Dune_project.encode
        |> List.map ~f:(fun exp ->
          exp |> Dune_lang.Ast.add_loc ~loc:Loc.none |> Cst.concrete)
      in
      List.append
        cst
        [ Cst.Comment
            ( Loc.none
            , [ " See the complete stanza docs at \
                 https://dune.readthedocs.io/en/stable/reference/dune-project/index.html"
              ] )
        ]
    ;;
  end

  (* TODO Support for merging in changes to an existing stanza *)
  let add_stanza_to_dune_file ~(project : Dune_project.t) ~dir stanza =
    File.load_dune_file ~path:dir |> File.Stanza.add project stanza
  ;;

  (* Functions to make the various components, represented as lists of files *)
  module Make = struct
    let bin ({ context; common; options } : Options.Executable.t Options.t) =
      let dir = context.dir in
      let bin_dune =
        Stanza_cst.executable common options
        |> add_stanza_to_dune_file ~project:context.project ~dir
      in
      let bin_ml =
        let name = sprintf "%s.ml" (Dune_lang.Atom.to_string common.name) in
        let content = sprintf "let () = print_endline \"Hello, World!\"\n" in
        File.make_text dir name content
      in
      let files = [ bin_dune; bin_ml ] in
      [ { dir; files } ]
    ;;

    let src ({ context; common; options } : Options.Library.t Options.t) =
      let dir = context.dir in
      let lib_dune =
        Stanza_cst.library common options
        |> add_stanza_to_dune_file ~project:context.project ~dir
      in
      let files = [ lib_dune ] in
      [ { dir; files } ]
    ;;

    let test ({ context; common; options } : Options.Test.t Options.t) =
      (* Marking the current absence of test-specific options *)
      let dir = context.dir in
      let test_dune =
        Stanza_cst.test common options
        |> add_stanza_to_dune_file ~project:context.project ~dir
      in
      let test_ml =
        let name = sprintf "%s.ml" (Dune_lang.Atom.to_string common.name) in
        let content = "" in
        File.make_text dir name content
      in
      let files = [ test_dune; test_ml ] in
      [ { dir; files } ]
    ;;

    let dune_project_file dir ({ context; common; options } : Options.Project.t Options.t)
      =
      let opam_file_gen =
        match options.pkg with
        | Opam -> true
        | Esy -> false
      in
      let content =
        Stanza_cst.dune_project
          ~opam_file_gen
          ~defaults:context.defaults
          Path.(as_in_source_tree_exn context.dir)
          common
      in
      File.Dune { path = dir; content; name = "dune-project" }
    ;;

    let proj_exec dir ({ context; common; options } : Options.Project.t Options.t) =
      let lib_target =
        src
          { context = { context with dir = Path.relative dir "lib" }
          ; options = { public = None; inline_tests = options.inline_tests }
          ; common
          }
      in
      let test_target =
        let test_name = "test_" ^ Dune_lang.Atom.to_string common.name in
        test
          { context = { context with dir = Path.relative dir "test" }
          ; options = ()
          ; common = { common with name = Dune_lang.Atom.of_string test_name }
          }
      in
      let bin_target =
        (* Add the lib_target as a library to the executable*)
        let libraries = Stanza_cst.add_to_list_set common.name common.libraries in
        bin
          { context = { context with dir = Path.relative dir "bin" }
          ; options = { public = Some (Public_name.of_name_exn common.name) }
          ; common = { common with libraries; name = Dune_lang.Atom.of_string "main" }
          }
      in
      bin_target @ lib_target @ test_target
    ;;

    let proj_lib dir ({ context; common; options } : Options.Project.t Options.t) =
      let lib_target =
        src
          { context = { context with dir = Path.relative dir "lib" }
          ; options =
              { public = Some (Public_name.of_name_exn common.name)
              ; inline_tests = options.inline_tests
              }
          ; common
          }
      in
      let test_target =
        let test_name = "test_" ^ Dune_lang.Atom.to_string common.name in
        test
          { context = { context with dir = Path.relative dir "test" }
          ; options = ()
          ; common = { common with name = Dune_lang.Atom.of_string test_name }
          }
      in
      lib_target @ test_target
    ;;

    let proj ({ common; options; _ } as opts : Options.Project.t Options.t) =
      let ({ template; pkg; _ } : Options.Project.t) = options in
      let dir = Path.Source.root in
      let name =
        Package.Name.parse_string_exn (Loc.none, Dune_lang.Atom.to_string common.name)
      in
      let proj_target =
        let package_files =
          match (pkg : Options.Project.Pkg.t) with
          | Opam ->
            let opam_file = Path.source @@ Package_name.file name ~dir in
            [ File.make_text (Path.parent_exn opam_file) (Path.basename opam_file) "" ]
          | Esy -> [ File.make_text (Path.source dir) "package.json" "" ]
        in
        let dir = Path.source dir in
        { dir; files = dune_project_file dir opts :: package_files }
      in
      let component_targets =
        (match (template : Options.Project.Template.t) with
         | Exec -> proj_exec
         | Lib -> proj_lib)
          (Path.source dir)
          opts
      in
      proj_target :: component_targets
    ;;
  end

  let report_uncreated_file = function
    | Ok _ -> ()
    | Error path ->
      let open Pp.O in
      User_warning.emit
        [ Pp.textf "File "
          ++ Pp.tag
               User_message.Style.Kwd
               (Pp.verbatim (Path.to_string_maybe_quoted path))
          ++ Pp.text " was not created because it already exists"
        ]
  ;;

  (** Creates a component, writing the files to disk *)
  let create target =
    File.create_dir target.dir;
    List.map ~f:File.write target.files
  ;;

  let init (type options) (t : options t) =
    let target =
      match t with
      | Executable params -> Make.bin params
      | Library params -> Make.src params
      | Project params -> Make.proj params
      | Test params -> Make.test params
    in
    List.concat_map ~f:create target |> List.iter ~f:report_uncreated_file
  ;;
end
