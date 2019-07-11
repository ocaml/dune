open! Stdune
open Import
open Dune_lang.Decoder

module Kind = struct
  type t =
    | Dune
    | Jbuilder
end

module Name : sig
  type t = private
    | Named     of string
    | Anonymous of Path.Source.t

  val to_dyn : t -> Dyn.t

  val equal : t -> t -> bool
  val compare : t -> t -> Ordering.t

  val to_string_hum : t -> string

  val decode : t Dune_lang.Decoder.t

  val to_encoded_string : t -> string
  val of_encoded_string : string -> t

  val anonymous : Path.Source.t -> t
  val named : string -> t option

  val anonymous_root : t

  module Infix : Comparator.OPS with type t = t

  module Map : Map.S with type key = t
end = struct
  module T = struct
    type t =
      | Named     of string
      | Anonymous of Path.Source.t

    let compare a b =
      match a, b with
      | Named     x, Named     y -> String.compare x y
      | Anonymous x, Anonymous y -> Path.Source.compare x y
      | Named     _, Anonymous _ -> Lt
      | Anonymous _, Named     _ -> Gt

    let equal a b = Ordering.is_eq (compare a b)

    let to_dyn =
      let open Dyn.Encoder in
      function
      | Named n -> constr "Named" [string n]
      | Anonymous p -> constr "Anonymous" [Path.Source.to_dyn p]
  end

  include T

  module Map = Map.Make(T)

  module Infix = Comparator.Operators(T)

  let anonymous_root = Anonymous Path.Source.root

  let to_string_hum = function
    | Named s -> s
    | Anonymous p ->
      sprintf "<anonymous %s>" (Path.Source.to_string_maybe_quoted p)

  let validate name =
    let len = String.length name in
    len > 0 &&
    String.for_all name ~f:(function
      | '.' | '/' -> false
      | _         -> true)

  let named name =
    if validate name then
      Some (Named name)
    else
      None

  let anonymous path = Anonymous path

  let decode =
    Dune_lang.Decoder.plain_string (fun ~loc s ->
      if validate s then
        Named s
      else
        User_error.raise ~loc
          [ Pp.text "Invalid project name" ])

  let to_encoded_string = function
    | Named     s -> s
    | Anonymous p ->
      if Path.Source.is_root p then
        "."
      else
        "." ^ String.map (Path.Source.to_string p)
                ~f:(function
                  | '/' -> '.'
                  | c   -> c)

  let of_encoded_string =
    let invalid s =
      (* Users would see this error if they did "dune build
         _build/default/.ppx/..." *)
      User_error.raise [ Pp.textf "Invalid encoded project name: %S" s ]
    in
    fun s ->
      match s with
      | "" -> invalid s
      | "." -> anonymous_root
      | _ when s.[0] = '.' -> begin
          match
            String.split s ~on:'.'
            |> List.tl
            |> String.concat ~sep:"/"
            |> Path.of_string
            |> Path.as_in_source_tree
          with
          | Some p -> Anonymous p
          | None -> invalid s
        end
      | _ when validate s -> Named s
      | _ -> invalid s
end

module Project_file = struct
  type t =
    { file           : Path.Source.t
    ; mutable exists : bool
    ; project_name   : Name.t
    }

  let to_dyn { file; exists; project_name } =
    let open Dyn.Encoder  in
    record
      [ "file", Path.Source.to_dyn file
      ; "exists", bool exists
      ; "project_name", Name.to_dyn project_name
      ]
end

module Source_kind = struct
  type t =
    | Github of string * string
    | Url of string

  let to_dyn =
    let open Dyn.Encoder in
    function
    | Github (user,repo) ->
      constr "Github" [string user; string repo]
    | Url url ->
      constr "Url" [string url]

  let pp fmt = function
    | Github (user,repo) ->
      Format.fprintf fmt "git+https://github.com/%s/%s.git" user repo
    | Url u -> Format.pp_print_string fmt u

  let decode =
    let open Stanza.Decoder in
    sum
      ["github", plain_string (fun ~loc s ->
         match String.split ~on:'/' s with
         | [user; repo] -> Github (user,repo)
         | _ ->
           User_error.raise ~loc [ Pp.textf "GitHub repository must be of form user/repo" ])
      ; "uri", string >>| fun s -> Url s
      ]
end

module File_key = struct
  type t = string

  module Map = String.Map

  let of_string s = s
  let to_string s = s

  let make ~name ~root =
    let digest = Digest.generic (name, root) |> Digest.to_string in
    String.take digest 12
end

type t =
  { name            : Name.t
  ; root            : Path.Source.t
  ; version         : string option
  ; source          : Source_kind.t option
  ; license         : string option
  ; authors         : string list
  ; homepage        : string option
  ; bug_reports     : string option
  ; documentation   : string option
  ; maintainers     : string list
  ; packages        : Package.t Package.Name.Map.t
  ; stanza_parser   : Stanza.t list Dune_lang.Decoder.t
  ; project_file    : Project_file.t
  ; extension_args  : Univ_map.t
  ; parsing_context : Univ_map.t
  ; implicit_transitive_deps : bool
  ; wrapped_executables : bool
  ; dune_version    : Syntax.Version.t
  ; allow_approx_merlin : bool
  ; generate_opam_files : bool
  ; file_key : File_key.t
  ; dialects        : Dialect.S.t
  }

let equal = (==)
let hash = Hashtbl.hash

let packages t = t.packages
let version t = t.version
let source t = t.source
let license t = t.license
let homepage t = t.homepage
let documentation t = t.documentation
let bug_reports t = t.bug_reports
let maintainers t = t.maintainers
let authors t = t.authors
let name t = t.name
let root t = t.root
let stanza_parser t = t.stanza_parser
let file t = t.project_file.file
let file_key t = t.file_key
let implicit_transitive_deps t = t.implicit_transitive_deps
let allow_approx_merlin t = t.allow_approx_merlin
let generate_opam_files t = t.generate_opam_files
let dialects t = t.dialects

let to_dyn
      { name ; root ; version ; source; license; authors
      ; homepage ; documentation ; project_file ; parsing_context = _
      ; bug_reports ; maintainers
      ; extension_args = _; stanza_parser = _ ; packages
      ; implicit_transitive_deps ; wrapped_executables ; dune_version
      ; allow_approx_merlin ; generate_opam_files
      ; file_key ; dialects } =
  let open Dyn.Encoder in
  record
    [ "name", Name.to_dyn name
    ; "root", Path.Source.to_dyn root
    ; "version", (option string) version
    ; "source", (option Source_kind.to_dyn) source
    ; "license", (option string) license
    ; "homepage", (option string) homepage
    ; "documentation", (option string) documentation
    ; "bug_reports", (option string) bug_reports
    ; "maintainers", (list string) maintainers
    ; "authors", (list string) authors
    ; "project_file", Project_file.to_dyn project_file
    ; "packages",
      (list (pair Package.Name.to_dyn Package.to_dyn))
        (Package.Name.Map.to_list packages)
    ; "implicit_transitive_deps",
      bool implicit_transitive_deps
    ; "wrapped_executables", bool wrapped_executables
    ; "dune_version", Syntax.Version.to_dyn dune_version
    ; "allow_approx_merlin", bool allow_approx_merlin
    ; "generate_opam_files", bool generate_opam_files
    ; "file_key", string file_key
    ; "dialects", Dialect.S.to_dyn dialects
    ]

let find_extension_args t key =
  Univ_map.find t.extension_args key

include Versioned_file.Make(struct
    type t = Stanza.Parser.t list
  end)

let default_dune_language_version =
  ref (Syntax.greatest_supported_version Stanza.syntax)

let get_dune_lang () =
  { (Lang.get_exn "dune" ) with version = !default_dune_language_version }

type created_or_already_exist = Created | Already_exist

module Project_file_edit = struct
  open Project_file

  let notify_user paragraphs =
    Console.print_user_message
      (User_message.make paragraphs
         ~prefix:(Pp.seq
                    (Pp.tag (Pp.verbatim "Info")
                       ~tag:User_message.Style.Warning)
                    (Pp.char ':')))

  let lang_stanza () =
    let ver = (Lang.get_exn "dune").version in
    sprintf "(lang dune %s)" (Syntax.Version.to_string ver)

  let ensure_exists t =
    if t.exists then
      Already_exist
    else begin
      let ver = !default_dune_language_version in
      let lines =
        [sprintf "(lang dune %s)" (Syntax.Version.to_string ver)]
      in
      let lines =
        match t.project_name with
        | Anonymous _ -> lines
        | Named s ->
          lines @ [Dune_lang.to_string ~syntax:Dune
                     (List [ Dune_lang.atom "name"
                           ; Dune_lang.atom_or_quoted_string s
                           ])]
      in
      notify_user
        [ Pp.textf "Creating file %s with this contents:"
            (Path.Source.to_string_maybe_quoted t.file)
        ; Pp.vbox
            (Pp.concat_map lines ~sep:Pp.cut
               ~f:(fun line -> Pp.seq (Pp.verbatim "| ") (Pp.verbatim line)))
        ];
      Io.write_lines (Path.source t.file) lines ~binary:false;
      t.exists <- true;
      Created
    end

  let append t str =
    let what = ensure_exists t in
    let prev = Io.read_file (Path.source t.file) ~binary:false in
    notify_user
      [ Pp.textf "Appending this line to %s: %s"
          (Path.Source.to_string_maybe_quoted t.file) str
      ];
    Io.with_file_out (Path.source t.file) ~binary:false ~f:(fun oc ->
      List.iter [prev; str] ~f:(fun s ->
        output_string oc s;
        let len = String.length s in
        if len > 0 && s.[len - 1] <> '\n' then output_char oc '\n'));
    what
end

let lang_stanza = Project_file_edit.lang_stanza

let ensure_project_file_exists t =
  Project_file_edit.ensure_exists t.project_file

let append_to_project_file t str =
  Project_file_edit.append t.project_file str

module Extension = struct
  type 'a t = 'a Univ_map.Key.t

  type 'a poly_info =
    { syntax       : Syntax.t
    ; stanzas      : ('a * Stanza.Parser.t list) Dune_lang.Decoder.t
    ; experimental : bool
    ; key          : 'a t
    }

  type info = Extension : 'a poly_info -> info

  let syntax (Extension e) = e.syntax
  let is_experimental (Extension e) = e.experimental

  type instance =
    { extension  : info
    ; version    : Syntax.Version.t
    ; loc        : Loc.t
    ; parse_args : (Univ_map.t * Stanza.Parser.t list) Dune_lang.Decoder.t ->
        Univ_map.t * Stanza.Parser.t list
    }

  let extensions = Hashtbl.create 32

  let register ?(experimental=false) syntax stanzas arg_to_dyn =
    let name = Syntax.name syntax in
    if Hashtbl.mem extensions name then
      Code_error.raise "Dune_project.Extension.register: already registered"
        [ "name", Dyn.Encoder.string name ];
    let key = Univ_map.Key.create ~name arg_to_dyn in
    let ext = { syntax; stanzas; experimental; key } in
    Hashtbl.add_exn extensions name (Extension ext);
    key

  let register_simple ?experimental syntax stanzas =
    let unit_stanzas =
      let+ r = stanzas in
      ((), r)
    in
    let _ : unit t =
      register ?experimental syntax unit_stanzas Unit.to_dyn
    in
    ()

  let instantiate ~loc ~parse_args (name_loc, name) (ver_loc, ver) =
    match Hashtbl.find extensions name with
    | None ->
      User_error.raise ~loc:name_loc
        [ Pp.textf "Unknown extension %S." name ]
        ~hints:(User_message.did_you_mean name
                  ~candidates:(Hashtbl.keys extensions))
    | Some t ->
      Syntax.check_supported (syntax t) (ver_loc, ver);
      { extension = t
      ; version = ver
      ; loc
      ; parse_args
      }

  (* Extensions that are not selected in the dune-project file are
     automatically available at their latest version.  When used, dune
     will automatically edit the dune-project file. *)
  let automatic ~project_file ~f =
    Hashtbl.foldi extensions ~init:[] ~f:(fun name extension acc ->
      if f name then
        let version =
          if is_experimental extension then
            (0, 0)
          else
            Syntax.greatest_supported_version (syntax extension)
        in
        let parse_args p =
          let open Dune_lang.Decoder in
          let dune_project_edited = ref false in
          let arg, stanzas =
            parse (enter p) Univ_map.empty (List (Loc.of_pos __POS__, []))
          in
          let result_stanzas =
            List.map stanzas ~f:(fun (name, p) ->
              (name,
               let* () = return () in
               if not !dune_project_edited then begin
                 dune_project_edited := true;
                 ignore (
                   Project_file_edit.append project_file
                     (Dune_lang.to_string ~syntax:Dune
                        (List [ Dune_lang.atom "using"
                              ; Dune_lang.atom name
                              ; Dune_lang.atom
                                  (Syntax.Version.to_string version)
                              ]))
                   : created_or_already_exist)
               end;
               p))
          in
          (arg, result_stanzas)
        in
        { extension
        ; version
        ; loc = Loc.none
        ; parse_args
        } :: acc
      else
        acc)
end

let interpret_lang_and_extensions ~(lang : Lang.Instance.t)
      ~explicit_extensions ~project_file =
  match
    String.Map.of_list
      (List.map explicit_extensions ~f:(fun (e : Extension.instance) ->
         (Syntax.name (Extension.syntax e.extension), e.loc)))
  with
  | Error (name, _, loc) ->
    User_error.raise ~loc
      [ Pp.textf "Extension %S specified for the second time." name ]
  | Ok map ->
    let implicit_extensions =
      Extension.automatic ~project_file
        ~f:(fun name -> not (String.Map.mem map name))
    in
    let extensions =
      List.map ~f:(fun e -> (e, true)) explicit_extensions @
      List.map ~f:(fun e -> (e, false)) implicit_extensions
    in
    let acc = Univ_map.singleton (Syntax.key lang.syntax) lang.version in
    let parsing_context =
      List.fold_left extensions ~init:acc
        ~f:(fun acc ((ext : Extension.instance), _) ->
          Univ_map.add acc (Syntax.key (Extension.syntax ext.extension))
            ext.version)
    in
    let extension_args, extension_stanzas =
      List.fold_left
        extensions
        ~init:(Univ_map.empty, [])
        ~f:(fun (args_acc, stanzas_acc)
             ((instance : Extension.instance), is_explicit) ->
             let extension = instance.extension in
             let Extension.Extension e = extension in
             let args =
               let+ (arg, stanzas) =
                 Dune_lang.Decoder.set_many parsing_context e.stanzas
               in
               let new_args_acc =
                 if is_explicit then
                   Univ_map.add args_acc e.key arg
                 else
                   args_acc
               in
               (new_args_acc, stanzas)
             in
             let (new_args_acc, stanzas) = instance.parse_args args in
             (new_args_acc, stanzas::stanzas_acc))
    in
    let stanzas = List.concat (lang.data :: extension_stanzas) in
    let stanza_parser =
      Dune_lang.Decoder.(set_many parsing_context (sum stanzas))
    in
    (parsing_context, stanza_parser, extension_args)

let key =
  Univ_map.Key.create ~name:"dune-project" to_dyn

let set t = Dune_lang.Decoder.set key t
let get_exn () =
  let open Dune_lang.Decoder in
  get key >>| function
  | Some t -> t
  | None ->
    Code_error.raise "Current project is unset" []

let filename = "dune-project"

let implicit_transitive_deps_default ~(lang : Lang.Instance.t) =
  lang.version < (2, 0)

let wrapped_executables_default ~(lang : Lang.Instance.t) =
  lang.version >= (2, 0)

let anonymous = lazy (
  let lang = get_dune_lang () in
  let name = Name.anonymous_root in
  let project_file =
    { Project_file.
      file = Path.Source.relative Path.Source.root filename
    ; exists = false
    ; project_name = name
    }
  in
  let parsing_context, stanza_parser, extension_args =
    interpret_lang_and_extensions ~lang ~explicit_extensions:[] ~project_file
  in
  let implicit_transitive_deps = implicit_transitive_deps_default ~lang in
  let wrapped_executables = wrapped_executables_default ~lang in
  let root = Path.Source.root in
  let file_key = File_key.make ~root ~name in
  { name
  ; packages      = Package.Name.Map.empty
  ; root
  ; source        = None
  ; license       = None
  ; homepage      = None
  ; bug_reports   = None
  ; documentation = None
  ; maintainers   = []
  ; authors       = []
  ; version       = None
  ; implicit_transitive_deps
  ; wrapped_executables
  ; stanza_parser
  ; project_file
  ; extension_args
  ; parsing_context
  ; dune_version = lang.version
  ; allow_approx_merlin = true
  ; generate_opam_files = false
  ; file_key
  ; dialects = Dialect.S.empty
  })

let default_name ~dir ~packages =
  match Package.Name.Map.choose packages with
  | None -> Name.anonymous dir
  | Some (_, pkg) ->
    let pkg =
      let open Package.Name.Infix in
      Package.Name.Map.fold packages ~init:pkg ~f:(fun pkg acc ->
        if acc.Package.name <= pkg.Package.name then
          acc
        else
          pkg)
    in
    let name = Package.Name.to_string pkg.name in
    match Name.named name with
    | Some x -> x
    | None ->
      User_error.raise ~loc:pkg.loc
        [ Pp.textf "%S is not a valid opam package name."
            name
        ]

let parse ~dir ~lang ~opam_packages ~file =
  fields
    (let+ name = field_o "name" Name.decode
     and+ version = field_o "version" string
     and+ source = field_o "source" (Syntax.since Stanza.syntax (1, 7)
                                     >>> Source_kind.decode)
     and+ packages =
       multi_field "package" (Package.decode ~dir)
     and+ authors = field ~default:[] "authors"
                      (Syntax.since Stanza.syntax (1, 9) >>> repeat string)
     and+ license = field_o "license"
                      (Syntax.since Stanza.syntax (1, 9) >>> string)
     and+ homepage = field_o "homepage"
                       (Syntax.since Stanza.syntax (1, 10) >>> string)
     and+ documentation = field_o "documentation"
                       (Syntax.since Stanza.syntax (1, 10) >>> string)
     and+ bug_reports = field_o "bug_reports"
                          (Syntax.since Stanza.syntax (1, 10) >>> string)
     and+ maintainers = field "maintainers" ~default:[]
                         (Syntax.since Stanza.syntax (1, 10) >>> repeat string)
     and+ explicit_extensions =
       multi_field "using"
         (let+ loc = loc
          and+ name = located string
          and+ ver = located Syntax.Version.decode
          and+ parse_args = capture
          in
          (* We don't parse the arguments quite yet as we want to set
             the version of extensions before parsing them. *)
          Extension.instantiate ~loc ~parse_args name ver)
     and+ implicit_transitive_deps =
       field_o_b "implicit_transitive_deps"
         ~check:(Syntax.since Stanza.syntax (1, 7))
     and+ wrapped_executables =
       field_o_b "wrapped_executables"
         ~check:(Syntax.since Stanza.syntax (1, 11))
     and+ allow_approx_merlin =
       field_o_b "allow_approximate_merlin"
         ~check:(Syntax.since Stanza.syntax (1, 9))
     and+ () = Versioned_file.no_more_lang
     and+ generate_opam_files = field_o_b "generate_opam_files"
                                  ~check:(Syntax.since Stanza.syntax (1, 10))
     and+ dialects = multi_field "dialect" Dialect.decode
     in
     let homepage =
       match homepage, source with
       | None, Some (Github (user, repo)) ->
         Some (sprintf "https://github.com/%s/%s" user repo)
       | s, _ -> s
     in
     let bug_reports =
       match bug_reports, source with
       | None, Some (Github (user, repo)) ->
         Some (sprintf "https://github.com/%s/%s/issues" user repo)
       | s, _ -> s
     in
     let packages =
       if List.is_empty packages then
         Package.Name.Map.map opam_packages ~f:(fun (_loc, p) -> Lazy.force p)
       else begin
         begin match packages, name with
         | [p], Some (Named name) ->
           if Package.Name.to_string p.name <> name then
             User_error.raise ~loc:p.loc
               [ Pp.textf "when a single package is defined, it must have the same \
                name as the project name: %s" name ];
         | _, _ -> ()
         end;
         match
           Package.Name.Map.of_list_map packages ~f:(fun p -> p.name, p)
         with
         | Error (_, _, p) ->
           User_error.raise ~loc:p.loc [ Pp.textf "package %s is already defined"
             (Package.Name.to_string p.name) ]
         | Ok packages ->
           Package.Name.Map.merge packages opam_packages
             ~f:(fun _name dune opam ->
               match dune, opam with
               | _, None -> dune
               | Some p, _ -> Some { p with kind = Dune (Option.is_some opam) }
               | None, Some (loc, _) ->
                 User_error.raise ~loc
                   [ Pp.text
                       "This opam file doesn't have a corresponding \
                        (package ...) stanza in the dune-project_file. \
                        Since you have at least one other (package \
                        ...) stanza in your dune-project file, you \
                        must a (package ...) stanza for each opam \
                        package in your project."
                   ])
       end
     in
     let packages =
       match version with
       | None -> packages
       | Some version ->
         let version = Some (version, Package.Version_source.Project) in
         Package.Name.Map.map packages ~f:(fun p ->
           match p.version with
           | Some _ -> p
           | None -> { p with version })
     in
     let name =
       match name with
       | Some n -> n
       | None -> default_name ~dir ~packages
     in
     let project_file : Project_file.t =
       { file
       ; exists = true
       ; project_name = name
       }
     in
     let parsing_context, stanza_parser, extension_args =
       interpret_lang_and_extensions ~lang ~explicit_extensions ~project_file
     in
     let implicit_transitive_deps =
       Option.value implicit_transitive_deps
         ~default:(implicit_transitive_deps_default ~lang)
     in
     let wrapped_executables =
       Option.value wrapped_executables
         ~default:(wrapped_executables_default ~lang) in
     let allow_approx_merlin =
       Option.value ~default:false allow_approx_merlin in
     let generate_opam_files =
       Option.value ~default:false generate_opam_files in
     let root = dir in
     let file_key = File_key.make ~name ~root in
     let dialects = List.fold_left ~f:Dialect.S.add ~init:Dialect.S.builtin dialects in
     { name
     ; file_key
     ; root
     ; version
     ; source
     ; license
     ; authors
     ; homepage
     ; documentation
     ; bug_reports
     ; maintainers
     ; packages
     ; stanza_parser
     ; project_file
     ; extension_args
     ; parsing_context
     ; implicit_transitive_deps
     ; wrapped_executables
     ; dune_version = lang.version
     ; allow_approx_merlin
     ; generate_opam_files
     ; dialects
     })

let load_dune_project ~dir opam_packages =
  let file = Path.Source.relative dir filename in
  load (Path.source file) ~f:(fun lang ->
    parse ~dir ~lang ~opam_packages ~file)

let make_jbuilder_project ~dir opam_packages =
  let lang = get_dune_lang () in
  let packages =
    Package.Name.Map.map opam_packages ~f:(fun (_loc, p) -> Lazy.force p)
  in
  let name = default_name ~dir ~packages in
  let project_file =
    { Project_file.
      file = Path.Source.relative dir filename
    ; exists = false
    ; project_name = name
    }
  in
  let parsing_context, stanza_parser, extension_args =
    interpret_lang_and_extensions ~lang ~explicit_extensions:[] ~project_file in
  let root = dir in
  let file_key = File_key.make ~root ~name
  in
  let dialects = Dialect.S.builtin in
  { name
  ; root
  ; file_key
  ; version = None
  ; source = None
  ; license = None
  ; homepage = None
  ; bug_reports = None
  ; documentation = None
  ; maintainers = []
  ; authors = []
  ; packages
  ; stanza_parser
  ; project_file
  ; extension_args
  ; parsing_context
  ; implicit_transitive_deps = true
  ; dune_version = lang.version
  ; allow_approx_merlin = true
  ; generate_opam_files = false
  ; wrapped_executables = false
  ; dialects;
  }

let load ~dir ~files =
  let opam_packages =
    String.Set.fold files ~init:[] ~f:(fun fn acc ->
      match Filename.split_extension fn with
      | (pkg, ".opam") when pkg <> "" ->
        let name = Package.Name.of_string pkg in
        let opam_file = Path.Source.relative dir fn in
        let loc = Loc.in_file (Path.source opam_file) in
        let pkg = lazy (
          let version =
            let open Option.O in
            let* opam =
              match Opam_file.load (Path.source opam_file) with
              | s -> Some s
              | exception exn ->
                User_warning.emit ~loc:(Loc.in_file (Path.source opam_file))
                  [ Pp.text "Unable to read opam file. This package's \
                             version field will be ignored."
                  ; Pp.textf "Reason: %s" (Printexc.to_string exn)
                  ];
                None
            in
            let* version = Opam_file.get_field opam "version" in
            match version with
            | String (_, s) ->
              Some (s, Package.Version_source.Package)
            | _ -> None
          in
          { Package.
            name
          ; loc
          ; path = dir
          ; version
          ; conflicts = []
          ; depends = []
          ; depopts = []
          ; synopsis = None
          ; description = None
          ; kind = Opam
          ; tags = []
          })
        in
        (name, (loc, pkg)) :: acc
      | _ -> acc)
    |> Package.Name.Map.of_list_exn
  in
  if String.Set.mem files filename then
    Some (load_dune_project ~dir opam_packages)
  else if not (Package.Name.Map.is_empty opam_packages) then
    Some (make_jbuilder_project ~dir opam_packages)
  else
    None

let dune_version t = t.dune_version

let set_parsing_context t parser =
  Dune_lang.Decoder.set_many t.parsing_context parser

let wrapped_executables t = t.wrapped_executables
