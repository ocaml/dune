open! Stdune
open! Import

(** Because the dune_init utility deals with the addition of stanzas and
    fields to dune projects and files, we need to inspect and manipulate the
    concrete syntax tree (CST) a good deal. *)
module Cst = Dune_lang.Cst

module Kind = struct
  type t =
    | Executable
    | Library
    | Test

  let to_string = function
    | Executable -> "executable"
    | Library -> "library"
    | Test -> "test"

  let pp ppf t = Format.pp_print_string ppf (to_string t)

  let commands =
    [ "exe", Executable
    ; "lib", Library
    ; "test", Test
    ]
end

(** Abstractions around the kinds of files handled during initialization *)
module File = struct

  type dune =
    { path: Path.t
    ; name: string
    ; content: Cst.t list
    }

  type text =
    { path: Path.t
    ; name: string
    ; content: string
    }

  type t =
    | Dune of dune
    | Text of text

  let make_text path name content =
    Text {path; name; content}

  let full_path = function
    | Dune {path; name; _} | Text {path; name; _} ->
      Path.relative_exn path name

  (** Inspection and manipulation of stanzas in a file *)
  module Stanza = struct

    let pp ppf s =
      Option.iter (Cst.to_sexp s) ~f:(Dune_lang.pp Dune ppf)

    let libraries_conflict (a: Dune_file.Library.t) (b: Dune_file.Library.t) =
      a.name = b.name

    let executables_conflict (a : Dune_file.Executables.t) (b : Dune_file.Executables.t) =
      let a_names = List.map ~f:snd a.names |> String.Set.of_list in
      let b_names = List.map ~f:snd b.names |> String.Set.of_list in
      String.Set.inter a_names b_names |> String.Set.is_empty |> not

    let tests_conflict (a : Dune_file.Tests.t) (b : Dune_file.Tests.t) =
      executables_conflict a.exes b.exes

    let stanzas_conflict (a : Stanza.t) (b : Stanza.t) =
      let open Dune_file in
      match a, b with
      | Executables a, Executables b -> executables_conflict a b
      | Library a, Library b -> libraries_conflict a b
      | Tests a, Tests b -> tests_conflict a b
      (* NOTE No other stanza types currently supported *)
      | _ -> false

    let csts_conflict project (a : Cst.t) (b : Cst.t) =
      let of_ast =
        Dune_file.Stanzas.of_ast ~kind:Dune_lang.File_syntax.Dune project
      in
      begin
        let open Option.O in
        let* a_ast = Cst.abstract a in
        let+ b_ast = Cst.abstract b in
        let a_asts = of_ast a_ast in
        let b_asts = of_ast b_ast in
        List.exists ~f:(fun x -> List.exists ~f:(stanzas_conflict x) a_asts) b_asts
      end
      |> Option.value ~default:false

    (* TODO(shonfeder): replace with stanza merging *)
    let find_conflicting project new_stanzas existing_stanzas =
      let conflicting_stanza stanza =
        match List.find ~f:(csts_conflict project stanza) existing_stanzas with
        | Some conflict -> Some (stanza, conflict)
        | None -> None
      in
      List.find_map ~f:conflicting_stanza new_stanzas

    let add (project : Dune_project.t) stanzas = function
      | Text f -> Text f (* Adding a stanza to a text file isn't meaningful *)
      | Dune f ->
        match find_conflicting project stanzas f.content with
        | None -> Dune {f with content = f.content @ stanzas}
        | Some (a, b) ->
          die "Updating existing stanzas is not yet supported.@\n\
               A preexisting dune stanza conflicts with a generated stanza:\
               @\n@\nGenerated stanza:@.%a@.@.Pre-existing stanza:@.%a"
            pp a pp b
  end (* Stanza *)

  let create_dir path =
    try Path.mkdir_p path with
    | Unix.Unix_error (EACCES, _, _) ->
      die "A project directory cannot be created or accessed: \
           Lacking permissions needed to create directory %a"
        Path.pp path

  let load_dune_file ~path =
    let name = "dune" in
    let full_path = Path.relative_exn path name in
    let content =
      if not (Path.exists full_path) then
        []
      else
        match Format_dune_lang.parse_file (Some full_path) with
        | Format_dune_lang.Sexps content -> content
        | Format_dune_lang.OCaml_syntax _ ->
          die "Cannot load dune file %a because it uses OCaml syntax"
            Path.pp full_path
    in
    Dune {path; name; content}

  let write_dune_file (dune_file : dune) =
    let path = Path.relative_exn dune_file.path dune_file.name in
    Format_dune_lang.write_file ~path dune_file.content

  let write f =
    let path = full_path f in
    match f with
    | Dune f -> Ok (write_dune_file f)
    | Text f ->
      if Path.exists path then
        Error path
      else
        Ok (Io.write_file ~binary:false path f.content)
end

(** The context in which the initialization is executed *)
module Init_context = struct
  type t =
    { dir : Path.t
    ; project : Dune_project.t
    }

  let make path =
    let project =
      match Dune_project.load ~dir:Path.Source.root ~files:String.Set.empty with
      | Some p -> p
      | None   -> Lazy.force Dune_project.anonymous
    in
    let dir =
      match path with
      | None -> Path.root
      | Some p -> Path.of_string_exn p
    in
    File.create_dir dir;
    { dir; project }
end

module Component = struct

  module Options = struct
    type common =
      { name : string
      ; libraries : string list
      ; pps : string list
      }

    type executable =
      { public: string option
      }

    type library =
      { public: string option
      ; inline_tests: bool
      }

    (* NOTE: no options supported yet *)
    type test = ()

    type 'options t =
      { context : Init_context.t
      ; common : common
      ; options : 'options
      }
  end

  type 'options t =
    | Executable : Options.executable Options.t -> Options.executable t
    | Library : Options.library Options.t -> Options.library t
    | Test : Options.test Options.t -> Options.test t

  type target =
    { dir : Path.t
    ; files : File.t list
    }

  (** Creates Dune language CST stanzas describing components *)
  module Stanza_cst = struct
    open Dune_lang

    module Field = struct
      let atoms = List.map ~f:atom
      let public_name name = List [atom "public_name"; atom name]
      let name name = List [atom "name"; atom name]
      let inline_tests = List [atom "inline_tests"]
      let libraries libs = List (atom "libraries" :: atoms libs)
      let pps pps = List [atom "preprocess"; List (atom "pps" :: atoms pps)]

      let optional_field ~f = function
        | [] -> []
        | args -> [f args]

      let common (options : Options.common) =
        let optional_fields =
          optional_field ~f:libraries options.libraries
          @ optional_field ~f:pps options.pps
        in
        name options.name :: optional_fields
    end

    let make kind common_options fields  =
      (* Form the AST *)
      List (atom kind
            :: fields
            @ Field.common common_options)
      (* Convert to a CST *)
      |> Dune_lang.add_loc ~loc:Loc.none
      |> Cst.concrete
      (* Package as a list CSTs *)
      |> List.singleton

    let add_to_list_set elem set =
      if List.mem elem ~set then set else elem :: set

    let public_name_field ~default = function
      | None -> []
      | Some "" -> [Field.public_name default]
      | Some n  -> [Field.public_name n]

    let executable (common : Options.common) (options : Options.executable) =
      let public_name =
        public_name_field ~default:common.name options.public
      in
      make "executable" {common with name = "main"} public_name

    let library (common : Options.common) (options: Options.library) =
      let (common, inline_tests) =
        if not options.inline_tests then
          (common, [])
        else
          let pps =
            add_to_list_set "ppx_inline_tests" common.pps
          in
          ({common with pps}, [Field.inline_tests])
      in
      let public_name =
        public_name_field ~default:common.name options.public
      in
      make "library" common (public_name @ inline_tests)

    let test common ((): Options.test) =
      make "test" common []
  end

  (* TODO Support for merging in changes to an existing stanza *)
  let add_stanza_to_dune_file ~(project : Dune_project.t) ~dir stanza =
    File.load_dune_file ~path:dir
    |> File.Stanza.add project stanza

  let bin ({context; common; options} : Options.executable Options.t) =
    let dir = context.dir in
    let bin_dune =
      Stanza_cst.executable common options
      |> add_stanza_to_dune_file ~project:context.project ~dir
    in
    let bin_ml =
      let name = "main.ml" in
      let content = sprintf "let () = print_endline \"Hello, World!\"\n" in
      File.make_text dir name content
    in
    let files = [bin_dune; bin_ml] in
    {dir; files}

  let src ({context; common; options} : Options.library Options.t) =
    let dir = context.dir in
    let lib_dune =
      Stanza_cst.library common options
      |> add_stanza_to_dune_file ~project:context.project ~dir
    in
    let files = [lib_dune] in
    {dir; files}

  let test ({context; common; options}: Options.test Options.t) =
    (* Marking the current absence of test-specific options *)
    let dir = context.dir in
    let test_dune =
      Stanza_cst.test common options
      |> add_stanza_to_dune_file ~project:context.project ~dir
    in
    let test_ml =
      let name = sprintf "%s.ml" common.name in
      let content = "" in
      File.make_text dir name content
    in
    let files = [test_dune; test_ml] in
    {dir; files}

  let report_uncreated_file = function
    | Ok _ -> ()
    | Error path ->
      Errors.kerrf ~f:print_to_console
        "@{<warning>Warning@}: file @{<kwd>%a@} was not created \
         because it already exists\n"
        Path.pp path

  let create target =
    File.create_dir target.dir;
    List.map ~f:File.write target.files

  let init (type options) (t : options t) =
    let target =
      match t with
      | Executable params -> bin params
      | Library params    -> src params
      | Test params       -> test params
    in
    create target
    |> List.iter ~f:report_uncreated_file
end

let validate_component_name name =
  match Lib_name.Local.of_string name with
  | Ok _ -> ()
  | _    ->
    die "A component named '%s' cannot be created because it is an %s"
      name Lib_name.Local.invalid_message

let print_completion kind name =
  Errors.kerrf ~f:print_to_console
    "@{<ok>Success@}: initialized %a component named @{<kwd>%s@}\n"
    Kind.pp kind name
