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

  val pp : t Fmt.t

  val compare : t -> t -> Ordering.t

  val to_string_hum : t -> string

  val decode : t Dune_lang.Decoder.t
  val to_sexp : t Sexp.Encoder.t

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

  let pp fmt = function
    | Named n ->
      Format.fprintf fmt "Named %S" n
    | Anonymous p ->
      Format.fprintf fmt "Anonymous %s" (Path.to_string_maybe_quoted p)

  let to_string_hum = function
    | Named     s -> s
    | Anonymous p -> sprintf "<anonymous %s>" (Path.to_string_maybe_quoted p)

  let to_sexp = function
    | Named s -> Sexp.Encoder.string s
    | Anonymous p ->
      List [ Atom "anonymous"
           ; Path.to_sexp p
           ]

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
    { file           : Path.t
    ; mutable exists : bool
    ; project_name   : Name.t
    }

  let pp fmt { file ; exists; project_name } =
    Fmt.record fmt
      [ "file", Fmt.const Path.pp file
      ; "exists", Fmt.const Format.pp_print_bool exists
      ; "project_name", (fun fmt () -> Name.pp fmt project_name)
      ]

  let to_sexp { file; exists; project_name } =
    Sexp.Encoder.(
      record
        [ "file", Path.to_sexp file
        ; "exists", bool exists
        ; "project_name", Name.to_sexp project_name
        ])
end

type t =
  { name            : Name.t
  ; root            : Path.Local.t
  ; version         : string option
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
let name t = t.name
let root t = t.root
let stanza_parser t = t.stanza_parser
let file t = t.project_file.file
let implicit_transitive_deps t = t.implicit_transitive_deps
let allow_approx_merlin t = t.allow_approx_merlin

let pp fmt { name ; root ; version ; project_file ; parsing_context = _
           ; extension_args = _; stanza_parser = _ ; packages
           ; implicit_transitive_deps ; dune_version
           ; allow_approx_merlin } =
  Fmt.record fmt
    [ "name", Fmt.const Name.pp name
    ; "root", Fmt.const Path.Local.pp root
    ; "version", Fmt.const (Fmt.optional Format.pp_print_string) version
    ; "project_file", Fmt.const Project_file.pp project_file
    ; "packages",
      Fmt.const
        (Fmt.ocaml_list (Fmt.tuple Package.Name.pp Package.pp))
        (Package.Name.Map.to_list packages)
    ; "implicit_transitive_deps",
      Fmt.const Format.pp_print_bool implicit_transitive_deps
    ; "dune_version", Fmt.const Syntax.Version.pp dune_version
    ; "allow_approx_merlin"
    , Fmt.const Format.pp_print_bool allow_approx_merlin
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
           (Path.to_string_maybe_quoted t.file)
           (List.map lines ~f:((^) "| ") |> String.concat ~sep:"\n"));
      Io.write_lines t.file lines ~binary:false;
      t.exists <- true;
      Created
    end

  let append t str =
    let what = ensure_exists t in
    let prev = Io.read_file t.file ~binary:false in
    notify_user
      (sprintf "appending this line to %s: %s"
         (Path.to_string_maybe_quoted t.file) str);
    Io.with_file_out t.file ~binary:false ~f:(fun oc ->
      List.iter [prev; str] ~f:(fun s ->
        output_string oc s;
        let len = String.length s in
        if len > 0 && s.[len - 1] <> '\n' then output_char oc '\n'));
    what
end

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
               return () >>= fun () ->
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
    (fun { name; root; version; project_file
         ; stanza_parser = _; packages = _ ; extension_args = _
         ; parsing_context ; implicit_transitive_deps ; dune_version
         ; allow_approx_merlin } ->
      Sexp.Encoder.record
        [ "name", Name.to_sexp name
        ; "root", Path.Local.to_sexp root
        ; "version", Sexp.Encoder.(option string) version
        ; "project_file", Project_file.to_sexp project_file
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

let get_local_path p =
  match Path.kind p with
  | External _ -> assert false
  | Local    p -> p

let anonymous = lazy (
  let lang = get_dune_lang () in
  let name = Name.anonymous_root in
  let project_file =
    { Project_file.
      file = Path.relative Path.root filename
    ; exists = false
    ; project_name = name
    }
  in
  let parsing_context, stanza_parser, extension_args =
    interpret_lang_and_extensions ~lang ~explicit_extensions:[] ~project_file
  in
  { name          = name
  ; packages      = Package.Name.Map.empty
  ; root          = get_local_path Path.root
  ; version       = None
  ; implicit_transitive_deps = false
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
      Errors.fail (Loc.in_file (Package.opam_file pkg))
        "%S is not a valid opam package name."
        name

let name_field ~dir ~packages =
    let+ name = field_o "name" Name.decode in
    match name with
    | Some x -> x
    | None   -> default_name ~dir ~packages

let parse ~dir ~lang ~packages ~file =
  fields
    (let+ name = name_field ~dir ~packages
     and+ version = field_o "version" string
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
     ; root = get_local_path dir
     ; version
     ; packages
     ; stanza_parser
     ; project_file
     ; extension_args
     ; parsing_context
     ; implicit_transitive_deps
     ; dune_version = lang.version
     ; allow_approx_merlin
     })

let load_dune_project ~dir packages =
  let file = Path.relative dir filename in
  load file ~f:(fun lang -> parse ~dir ~lang ~packages ~file)

let make_jbuilder_project ~dir packages =
  let lang = get_dune_lang () in
  let name = default_name ~dir ~packages in
  let project_file =
    { Project_file.
      file = Path.relative dir filename
    ; exists = false
    ; project_name = name
    }
  in
  let parsing_context, stanza_parser, extension_args =
    interpret_lang_and_extensions ~lang ~explicit_extensions:[] ~project_file
  in
  { name
  ; root = get_local_path dir
  ; version = None
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
          let opam = Opam_file.load (Path.relative dir fn) in
          match Opam_file.get_field opam "version" with
          | Some (String (_, s)) -> Some s
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
