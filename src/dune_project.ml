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
    | Anonymous of Path.t

  val to_dyn : t -> Dyn.t

  val compare : t -> t -> Ordering.t

  val to_string_hum : t -> string

  val decode : t Dune_lang.Decoder.t

  val to_encoded_string : t -> string
  val of_encoded_string : string -> t

  val anonymous : Path.t -> t option
  val named : string -> t option

  val anonymous_root : t

  module Infix : Comparable.OPS with type t = t

  module Map : Map.S with type key = t
end = struct
  module T = struct
    type t =
      | Named     of string
      | Anonymous of Path.t

    let compare a b =
      match a, b with
      | Named     x, Named     y -> String.compare x y
      | Anonymous x, Anonymous y -> Path.compare   x y
      | Named     _, Anonymous _ -> Lt
      | Anonymous _, Named     _ -> Gt
  end

  include T

  module Map = Map.Make(T)

  module Infix = Comparable.Operators(T)

  let anonymous_root = Anonymous Path.root

  let to_dyn =
    let open Dyn.Encoder in
    function
    | Named n -> constr "Named" [string n]
    | Anonymous p -> constr "Anonymous" [Path.to_dyn p]

  let to_string_hum = function
    | Named     s -> s
    | Anonymous p -> sprintf "<anonymous %s>" (Path.to_string_maybe_quoted p)

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

  let anonymous path =
    if Path.is_managed path then
      Some (Anonymous path)
    else
      None

  let decode =
    Dune_lang.Decoder.plain_string (fun ~loc s ->
      if validate s then
        Named s
      else
        Dune_lang.Decoder.of_sexp_errorf loc "invalid project name")

  let to_encoded_string = function
    | Named     s -> s
    | Anonymous p ->
      if Path.is_root p then
        "."
      else
        "." ^ String.map (Path.to_string p)
                ~f:(function
                  | '/' -> '.'
                  | c   -> c)

  let of_encoded_string =
    let invalid s =
      (* Users would see this error if they did "dune build
         _build/default/.ppx/..." *)
      die "Invalid encoded project name: %S" s
    in
    fun s ->
      match s with
      | "" -> invalid s
      | "." -> anonymous_root
      | _ when s.[0] = '.' ->
        let p =
          Path.of_string
            (String.split s ~on:'.'
             |> List.tl
             |> String.concat ~sep:"/")
        in
        if not (Path.is_managed p) then invalid s;
        Anonymous p
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
           of_sexp_errorf loc "GitHub repository must be of form user/repo")
      ; "uri", string >>| fun s -> Url s
      ]
end

module Opam = struct

  module Dependency = struct
    module Op = struct
      type t =
        | Eq
        | Gte
        | Lte
        | Gt
        | Lt
        | Neq

      let map =
        [ "=", Eq
        ; ">=", Gte
        ; "<=", Lte
        ; ">", Gt
        ; "<", Lt
        ; "<>", Neq
        ]

      let to_dyn =
        let open Dyn.Encoder in
        function
        | Eq -> string "Eq"
        | Gt -> string "Gt"
        | Gte -> string "Gte"
        | Lte -> string "Lte"
        | Lt -> string "Lt"
        | Neq -> string "Neq"

      let to_relop : t -> OpamParserTypes.relop = function
        | Eq -> `Eq
        | Gte -> `Geq
        | Lte -> `Leq
        | Gt -> `Gt
        | Lt -> `Lt
        | Neq -> `Neq
    end

    module Constraint = struct
      module Var = struct
        type t =
          | QVar of string
          | Var of string

        let decode =
          let open Stanza.Decoder in
          let+ s = string in
          if String.is_prefix s ~prefix:":" then
            Var (String.drop s 1)
          else
            QVar s

        let to_opam : t -> OpamParserTypes.value =
          let nopos = Opam_file.nopos in
          function
          | QVar x -> String (nopos, x)
          | Var x -> Ident (nopos, x)
      end

      type t =
        | Bvar of Var.t
        | Uop of Op.t * Var.t
        | And of t list
        | Or of t list

      let decode =
        let open Stanza.Decoder in
        let ops =
          List.map Op.map ~f:(fun (name, op) ->
            name, (let+ x = Var.decode in Uop (op, x)))
        in
        let ops =
          ("!=", let+ loc = loc in of_sexp_error loc "Use <> instead of !=")
          :: ops
        in
        fix begin fun t ->
          let logops =
            [ "and", (let+ x = repeat t in And x)
            ; "or", (let+ x = repeat t in Or x)
            ]
          in
          peek_exn >>= function
          | Atom (_loc, A s) when String.is_prefix s ~prefix:":" ->
            let+ () = junk in
            Bvar (Var (String.drop s 1))
          | _ ->
            sum (ops @ logops)
        end

      let rec to_dyn =
        let open Dyn.Encoder in
        function
        | Bvar (QVar v) -> constr "Bvar" [Dyn.String v]
        | Bvar (Var v) -> constr "Bvar" [Dyn.String (":" ^ v)]
        | Uop (b, QVar v) -> constr "Uop" [Op.to_dyn b; Dyn.String v]
        | Uop (b, Var v) -> constr "Uop" [Op.to_dyn b; Dyn.String (":" ^ v)]
        | And t -> constr "And" (List.map ~f:to_dyn t)
        | Or t -> constr "Or" (List.map ~f:to_dyn t)
    end

    type t =
      { name : Package.Name.t
      ; constraint_ : Constraint.t option
      }

    let decode =
      let open Stanza.Decoder in
      let constrained =
        let+ name = Package.Name.decode
        and+ expr = Constraint.decode
        in
        { name
        ; constraint_ = Some expr
        }
      in
      if_list
        ~then_:(enter constrained)
        ~else_:(
          let+ name = Package.Name.decode in
          { name
          ; constraint_ = None
          })

    let rec opam_constraint : Constraint.t -> OpamParserTypes.value =
      let nopos = Opam_file.nopos in
      function
      | Bvar v -> Constraint.Var.to_opam v
      | Uop (op, v) ->
        Prefix_relop (nopos, Op.to_relop op, Constraint.Var.to_opam v)
      | And [c] -> opam_constraint c
      | And (c :: cs) ->
        Logop (nopos, `And, opam_constraint c, opam_constraint (And cs))
      | Or [c] -> opam_constraint c
      | Or (c :: cs) ->
        Logop (nopos, `Or, opam_constraint c, opam_constraint (And cs))
      | And []
      | Or [] -> Exn.code_error "opam_constraint" []

    let opam_depend : t -> OpamParserTypes.value =
      let nopos = Opam_file.nopos in
      fun { name; constraint_ } ->
        let constraint_ = Option.map ~f:opam_constraint constraint_ in
        let pkg : OpamParserTypes.value =
          String (nopos, Package.Name.to_string name) in
        match constraint_ with
        | None -> pkg
        | Some c -> Option (nopos, pkg, [c])

    let to_dyn { name; constraint_ } =
      let open Dyn.Encoder in
      record
        [ "name", Package.Name.to_dyn name
        ; "constr", Dyn.Option (Option.map ~f:Constraint.to_dyn constraint_)
        ]
  end

  module Package = struct
    module Name = Package.Name

    type t =
      { name : Package.Name.t
      ; synopsis : string
      ; description : string
      ; depends : Dependency.t list
      ; conflicts: Dependency.t list
      }

    let decode =
      let open Stanza.Decoder in
      Syntax.since Stanza.syntax (1, 7) >>>
      fields (
        let+ name = field "name" Package.Name.decode
        and+ synopsis = field "synopsis" string
        and+ description = field "description" string
        and+ depends =
          field ~default:[] "depends" (repeat Dependency.decode)
        and+ conflicts =
          field ~default:[] "conflicts" (repeat Dependency.decode)
        in
        { name
        ; synopsis
        ; description
        ; depends
        ; conflicts
        })

    let to_dyn { name; synopsis; depends; conflicts; description } =
      let open Dyn.Encoder in
      record
        [ "name", Package.Name.to_dyn name
        ; "synopsis", string synopsis
        ; "description", string description
        ; "depends", list Dependency.to_dyn depends
        ; "conflicts", list Dependency.to_dyn conflicts
        ]
  end

  type t =
    { tags : string list
    ; depends : Dependency.t list
    ; conflicts : Dependency.t list
    ; packages : Package.t list
    }

  let to_dyn { tags; depends ; packages ; conflicts } =
    let open Dyn.Encoder in
    record
      [ "tags", list string tags
      ; "depends", list Dependency.to_dyn depends
      ; "conflicts", list Dependency.to_dyn conflicts
      ; "packages", list Package.to_dyn packages
      ]

  let decode =
    let open Stanza.Decoder in
    Syntax.since Stanza.syntax (1, 9) >>>
    fields (
      let+ tags = field ~default:[] "tags" (repeat string)
      and+ depends =
        field ~default:[] "depends" (repeat Dependency.decode)
      and+ conflicts =
        field ~default:[] "conflicts" (repeat Dependency.decode)
      and+ packages = multi_field "package" Package.decode in
      { tags
      ; depends
      ; conflicts
      ; packages
      }
    )

  let find t name =
    List.find t.packages ~f:(fun p -> Package.Name.equal p.name name)
end

type t =
  { name            : Name.t
  ; root            : Path.Source.t
  ; version         : string option
  ; source          : Source_kind.t option
  ; license         : string option
  ; authors         : string list
  ; opam            : Opam.t option
  ; packages        : Package.t Package.Name.Map.t
  ; stanza_parser   : Stanza.t list Dune_lang.Decoder.t
  ; project_file    : Project_file.t
  ; extension_args  : Univ_map.t
  ; parsing_context : Univ_map.t
  ; implicit_transitive_deps : bool
  ; dune_version    : Syntax.Version.t
  ; allow_approx_merlin : bool
  }

let equal = (==)
let hash = Hashtbl.hash

let packages t = t.packages
let version t = t.version
let source t = t.source
let license t = t.license
let authors t = t.authors
let opam t = t.opam
let name t = t.name
let root t = t.root
let stanza_parser t = t.stanza_parser
let file t = t.project_file.file
let implicit_transitive_deps t = t.implicit_transitive_deps
let allow_approx_merlin t = t.allow_approx_merlin

let to_dyn
      { name ; root ; version ; source; license; authors
      ; opam; project_file ; parsing_context = _
      ; extension_args = _; stanza_parser = _ ; packages
      ; implicit_transitive_deps ; dune_version
      ; allow_approx_merlin } =
  let open Dyn.Encoder in
  record
    [ "name", Name.to_dyn name
    ; "root", via_sexp Path.Source.to_sexp root
    ; "version", (option string) version
    ; "source", (option Source_kind.to_dyn) source
    ; "license", (option string) license
    ; "authors", (list string) authors
    ; "opam", (option Opam.to_dyn) opam
    ; "project_file", Project_file.to_dyn project_file
    ; "packages",
      (list (pair Package.Name.to_dyn Package.to_dyn))
        (Package.Name.Map.to_list packages)
    ; "implicit_transitive_deps",
      bool implicit_transitive_deps
    ; "dune_version", Syntax.Version.to_dyn dune_version
    ; "allow_approx_merlin", bool allow_approx_merlin
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

  let notify_user s =
    kerrf ~f:print_to_console "@{<warning>Info@}: %s\n" s

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
        (sprintf "creating file %s with this contents:\n%s\n"
           (Path.Source.to_string_maybe_quoted t.file)
           (List.map lines ~f:((^) "| ") |> String.concat ~sep:"\n"));
      Io.write_lines (Path.source t.file) lines ~binary:false;
      t.exists <- true;
      Created
    end

  let append t str =
    let what = ensure_exists t in
    let prev = Io.read_file (Path.source t.file) ~binary:false in
    notify_user
      (sprintf "appending this line to %s: %s"
         (Path.Source.to_string_maybe_quoted t.file) str);
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

  let register ?(experimental=false) syntax stanzas arg_to_sexp =
    let name = Syntax.name syntax in
    if Hashtbl.mem extensions name then
      Exn.code_error "Dune_project.Extension.register: already registered"
        [ "name", Sexp.Encoder.string name ];
    let key = Univ_map.Key.create ~name arg_to_sexp in
    let ext = { syntax; stanzas; experimental; key } in
    Hashtbl.add extensions name (Extension ext);
    key

  let register_simple ?experimental syntax stanzas =
    let unit_stanzas =
      let+ r = stanzas in
      ((), r)
    in
    let unit_to_sexp () = Sexp.List [] in
    let _ : unit t =
      register ?experimental syntax unit_stanzas unit_to_sexp
    in
    ()

  let instantiate ~loc ~parse_args (name_loc, name) (ver_loc, ver) =
    match Hashtbl.find extensions name with
    | None ->
      Errors.fail name_loc "Unknown extension %S.%s" name
        (hint name (Hashtbl.keys extensions))
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
    Errors.fail loc "Extension %S specified for the second time." name
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
  Univ_map.Key.create ~name:"dune-project"
    (fun { name; root; version; project_file; source
         ; license; authors; opam
         ; stanza_parser = _; packages = _ ; extension_args = _
         ; parsing_context ; implicit_transitive_deps ; dune_version
         ; allow_approx_merlin } ->
      Sexp.Encoder.record
        [ "name", Dyn.to_sexp (Name.to_dyn name)
        ; "root", Path.Source.to_sexp root
        ; "license", Sexp.Encoder.(option string) license
        ; "authors", Sexp.Encoder.(list string) authors
        ; "source", Dyn.to_sexp (Dyn.Encoder.(option Source_kind.to_dyn) source)
        ; "version", Sexp.Encoder.(option string) version
        ; "opam", Dyn.to_sexp (Dyn.Encoder.(option Opam.to_dyn) opam)
        ; "project_file", Dyn.to_sexp (Project_file.to_dyn project_file)
        ; "parsing_context", Univ_map.to_sexp parsing_context
        ; "implicit_transitive_deps", Sexp.Encoder.bool implicit_transitive_deps
        ; "dune_version", Syntax.Version.to_sexp dune_version
        ; "allow_approx_merlin"
        , Sexp.Encoder.bool allow_approx_merlin
        ])

let set t = Dune_lang.Decoder.set key t
let get_exn () =
  let open Dune_lang.Decoder in
  get key >>| function
  | Some t -> t
  | None ->
    Exn.code_error "Current project is unset" []

let filename = "dune-project"

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
  { name          = name
  ; packages      = Package.Name.Map.empty
  ; root          = Path.Source.root
  ; source        = None
  ; license       = None
  ; authors       = []
  ; version       = None
  ; implicit_transitive_deps = false
  ; opam          = None
  ; stanza_parser
  ; project_file
  ; extension_args
  ; parsing_context
  ; dune_version = lang.version
  ; allow_approx_merlin = true
  })

let default_name ~dir ~packages =
  match Package.Name.Map.choose packages with
  | None -> Option.value_exn (Name.anonymous dir)
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
      Errors.fail (Loc.in_file (Path.source (Package.opam_file pkg)))
        "%S is not a valid opam package name."
        name

let name_field ~dir ~packages =
  let+ name = field_o "name" Name.decode in
  match name with
  | Some x -> x
  | None   -> default_name ~dir ~packages

let parse ~dir ~lang ~packages ~file =
  fields
    (let+ name = name_field ~dir:(Path.source dir) ~packages
     and+ version = field_o "version" string
     and+ source = field_o "source" (Syntax.since Stanza.syntax (1, 7)
                                     >>> Source_kind.decode)
     and+ opam = field_o "opam" Opam.decode
     and+ authors = field ~default:[] "authors"
                      (Syntax.since Stanza.syntax (1, 9) >>> repeat string)
     and+ license = field_o "license"
                      (Syntax.since Stanza.syntax (1, 9) >>> string)
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
     and+ allow_approx_merlin =
       field_o_b "allow_approximate_merlin"
         ~check:(Syntax.since Stanza.syntax (1, 9))
     and+ () = Versioned_file.no_more_lang
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
       Option.value implicit_transitive_deps ~default:true
     in
     let allow_approx_merlin =
       Option.value ~default:false allow_approx_merlin in
     { name
     ; root = dir
     ; version
     ; source
     ; license
     ; authors
     ; packages
     ; opam
     ; stanza_parser
     ; project_file
     ; extension_args
     ; parsing_context
     ; implicit_transitive_deps
     ; dune_version = lang.version
     ; allow_approx_merlin
     })

let load_dune_project ~dir packages =
  let file = Path.Source.relative dir filename in
  load (Path.source file) ~f:(fun lang -> parse ~dir ~lang ~packages ~file)

let make_jbuilder_project ~dir packages =
  let lang = get_dune_lang () in
  let name = default_name ~dir:(Path.source dir) ~packages in
  let project_file =
    { Project_file.
      file = Path.Source.relative dir filename
    ; exists = false
    ; project_name = name
    }
  in
  let parsing_context, stanza_parser, extension_args =
    interpret_lang_and_extensions ~lang ~explicit_extensions:[] ~project_file
  in
  { name
  ; root = dir
  ; version = None
  ; source = None
  ; license = None
  ; authors = []
  ; opam = None
  ; packages
  ; stanza_parser
  ; project_file
  ; extension_args
  ; parsing_context
  ; implicit_transitive_deps = true
  ; dune_version = lang.version
  ; allow_approx_merlin = true
  }

let read_name file =
  load file ~f:(fun _lang ->
    fields
      (let+ name = field_o "name" (located string)
       and+ () = junk_everything
       in
       name))

let load ~dir ~files =
  let packages =
    String.Set.fold files ~init:[] ~f:(fun fn acc ->
      match Filename.split_extension fn with
      | (pkg, ".opam") when pkg <> "" ->
        let version_from_opam_file =
          let open Option.O in
          let* opam =
            let opam_file = Path.Source.relative dir fn in
            match Opam_file.load (Path.source opam_file) with
            | s -> Some s
            | exception exn ->
              Errors.warn (Loc.in_file (Path.source opam_file))
                "Unable to read opam file. This package's version field will\
                 be ignored.@.Reason: %a@."
                Exn.pp exn;
              None
          in
          let* version = Opam_file.get_field opam "version" in
          match version with
          | String (_, s) -> Some s
          | _ -> None
        in
        let name = Package.Name.of_string pkg in
        (name,
         { Package.
           name
         ; path = dir
         ; version_from_opam_file
         }) :: acc
      | _ -> acc)
    |> Package.Name.Map.of_list_exn
  in
  if String.Set.mem files filename then
    Some (load_dune_project ~dir packages)
  else if not (Package.Name.Map.is_empty packages) then
    Some (make_jbuilder_project ~dir packages)
  else
    None

let dune_version t = t.dune_version

let set_parsing_context t parser =
  Dune_lang.Decoder.set_many t.parsing_context parser
