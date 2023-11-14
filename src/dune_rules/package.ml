open! Import
module Stanza = Dune_lang.Stanza
module Opam_file = Dune_pkg.Opam_file
module Dependency = Dune_pkg.Package_dependency

let opam_ext = ".opam"
let is_opam_file path = String.is_suffix (Path.to_string path) ~suffix:opam_ext

module Name = struct
  include Dune_lang.Package_name

  let of_opam_file_basename basename =
    let open Option.O in
    let* name = String.drop_suffix basename ~suffix:opam_ext in
    of_string_opt name
  ;;

  let opam_fn (t : t) = to_string t ^ opam_ext
  let meta_fn (t : t) = "META." ^ to_string t
  let version_fn (t : t) = to_string t ^ ".version"

  let of_opam_package_name opam_package_name =
    OpamPackage.Name.to_string opam_package_name |> of_string
  ;;

  let to_opam_package_name t = to_string t |> OpamPackage.Name.of_string

  module Infix = Comparator.Operators (String)
  module Map_traversals = Memo.Make_map_traversals (Map)

  let decode_opam_compatible =
    Dune_lang.Decoder.map ~f:Opam_compatible.to_package_name Opam_compatible.decode
  ;;
end

module Id = struct
  module T = struct
    type t =
      { name : Name.t
      ; dir : Path.Source.t
      }

    let compare { name; dir } pkg =
      let open Ordering.O in
      let= () = Name.compare name pkg.name in
      Path.Source.compare dir pkg.dir
    ;;

    let to_dyn { dir; name } =
      let open Dyn in
      record [ "name", Name.to_dyn name; "dir", Path.Source.to_dyn dir ]
    ;;
  end

  include T

  let hash { name; dir } = Tuple.T2.hash Name.hash Path.Source.hash (name, dir)
  let name t = t.name
  let default_opam_file { name; dir } = Path.Source.relative dir (Name.opam_fn name)

  module C = Comparable.Make (T)
  module Set = C.Set
  module Map = C.Map
end

module Source_kind = struct
  module Host = struct
    type kind =
      | Github
      | Bitbucket
      | Gitlab
      | Sourcehut

    let to_string = function
      | Github -> "github"
      | Bitbucket -> "bitbucket"
      | Gitlab -> "gitlab"
      | Sourcehut -> "sourcehut"
    ;;

    type t =
      { user : string
      ; repo : string
      ; kind : kind
      }

    let dyn_of_kind kind = kind |> to_string |> Dyn.string

    let to_dyn { user; repo; kind } =
      let open Dyn in
      record [ "kind", dyn_of_kind kind; "user", string user; "repo", string repo ]
    ;;

    let host_of_kind = function
      | Github -> "github.com"
      | Bitbucket -> "bitbucket.org"
      | Gitlab -> "gitlab.com"
      | Sourcehut -> "sr.ht"
    ;;

    let base_uri { kind; user; repo } =
      let host = host_of_kind kind in
      sprintf
        "%s/%s/%s"
        host
        (match kind with
         | Sourcehut -> "~" ^ user
         | _ -> user)
        repo
    ;;

    let add_https s = "https://" ^ s
    let homepage t = add_https (base_uri t)

    let bug_reports t =
      match t.kind with
      | Sourcehut -> add_https ("todo." ^ base_uri t)
      | _ ->
        homepage t
        ^
          (match t.kind with
          | Sourcehut -> assert false
          | Bitbucket | Github -> "/issues"
          | Gitlab -> "/-/issues")
    ;;

    let enum k =
      [ "GitHub", Github, None
      ; "Bitbucket", Bitbucket, Some (2, 8)
      ; "Gitlab", Gitlab, Some (2, 8)
      ; "Sourcehut", Sourcehut, Some (3, 1)
      ]
      |> List.map ~f:(fun (name, kind, since) ->
        let decode =
          let of_string ~loc s =
            match String.split ~on:'/' s with
            | [ user; repo ] -> k { kind; user; repo }
            | _ ->
              User_error.raise
                ~loc
                [ Pp.textf "%s repository must be of form user/repo" name ]
          in
          let open Dune_lang.Decoder in
          (match since with
           | None -> return ()
           | Some v -> Dune_lang.Syntax.since Stanza.syntax v)
          >>> plain_string of_string
        in
        let constr = to_string kind in
        constr, decode)
    ;;

    let encode { user; repo; kind } =
      let forge = to_string kind in
      let path = user ^ "/" ^ repo in
      let open Dune_lang.Encoder in
      pair string string (forge, path)
    ;;

    let to_string t =
      let base_uri =
        let base = base_uri t in
        match t.kind with
        | Sourcehut -> "git." ^ base
        | _ -> base ^ ".git"
      in
      "git+https://" ^ base_uri
    ;;
  end

  type t =
    | Host of Host.t
    | Url of string

  let to_dyn =
    let open Dyn in
    function
    | Host h -> variant "Host" [ Host.to_dyn h ]
    | Url url -> variant "Url" [ string url ]
  ;;

  let to_string = function
    | Host h -> Host.to_string h
    | Url u -> u
  ;;

  let encode =
    let open Dune_lang.Encoder in
    function
    | Url url -> pair string string ("uri", url)
    | Host host -> Host.encode host
  ;;

  let decode =
    let open Dune_lang.Decoder in
    sum (("uri", string >>| fun s -> Url s) :: Host.enum (fun x -> Host x))
  ;;
end

module Info = struct
  type t =
    { source : Source_kind.t option
    ; license : string list option
    ; authors : string list option
    ; homepage : string option
    ; bug_reports : string option
    ; documentation : string option
    ; maintainers : string list option
    }

  let source t = t.source
  let license t = t.license
  let authors t = t.authors

  let homepage t =
    match t.homepage, t.source with
    | None, Some (Host h) -> Some (Source_kind.Host.homepage h)
    | s, _ -> s
  ;;

  let bug_reports t =
    match t.bug_reports, t.source with
    | None, Some (Host h) -> Some (Source_kind.Host.bug_reports h)
    | s, _ -> s
  ;;

  let documentation t = t.documentation
  let maintainers t = t.maintainers

  let empty =
    { source = None
    ; license = None
    ; authors = None
    ; homepage = None
    ; bug_reports = None
    ; documentation = None
    ; maintainers = None
    }
  ;;

  let example =
    { source =
        Some
          (Host { kind = Source_kind.Host.Github; user = "username"; repo = "reponame" })
    ; license = Some [ "LICENSE" ]
    ; authors = Some [ "Author Name" ]
    ; maintainers = Some [ "Maintainer Name" ]
    ; documentation =
        Some "https://url/to/documentation"
        (* homepage and bug_reports are inferred from the source *)
    ; homepage = None
    ; bug_reports = None
    }
  ;;

  let to_dyn
    { source; license; authors; homepage; bug_reports; documentation; maintainers }
    =
    let open Dyn in
    record
      [ "source", (option Source_kind.to_dyn) source
      ; "license", (option (list string)) license
      ; "homepage", (option string) homepage
      ; "documentation", (option string) documentation
      ; "bug_reports", (option string) bug_reports
      ; "maintainers", option (list string) maintainers
      ; "authors", option (list string) authors
      ]
  ;;

  let encode_fields
    { source; authors; license; homepage; documentation; bug_reports; maintainers }
    =
    let open Dune_lang.Encoder in
    record_fields
      [ field_o "source" Source_kind.encode source
      ; field_l "authors" string (Option.value ~default:[] authors)
      ; field_l "maintainers" string (Option.value ~default:[] maintainers)
      ; field_l "license" string (Option.value ~default:[] license)
      ; field_o "homepage" string homepage
      ; field_o "documentation" string documentation
      ; field_o "bug_reports" string bug_reports
      ]
  ;;

  let decode ?since () =
    let open Dune_lang.Decoder in
    let v default = Option.value since ~default in
    let+ source =
      field_o
        "source"
        (Dune_lang.Syntax.since Stanza.syntax (v (1, 7)) >>> Source_kind.decode)
    and+ authors =
      field_o "authors" (Dune_lang.Syntax.since Stanza.syntax (v (1, 9)) >>> repeat string)
    and+ license =
      field_o
        "license"
        (Dune_lang.Syntax.since Stanza.syntax (v (1, 9))
         >>> let* l = repeat1 string in
             (if List.length l > 1
              then
                Dune_lang.Syntax.since
                  ~what:"Parsing several licenses"
                  Stanza.syntax
                  (v (3, 2))
              else return ())
             >>> return l)
    and+ homepage =
      field_o "homepage" (Dune_lang.Syntax.since Stanza.syntax (v (1, 10)) >>> string)
    and+ documentation =
      field_o "documentation" (Dune_lang.Syntax.since Stanza.syntax (v (1, 10)) >>> string)
    and+ bug_reports =
      field_o "bug_reports" (Dune_lang.Syntax.since Stanza.syntax (v (1, 10)) >>> string)
    and+ maintainers =
      field_o
        "maintainers"
        (Dune_lang.Syntax.since Stanza.syntax (v (1, 10)) >>> repeat string)
    in
    { source; authors; license; homepage; documentation; bug_reports; maintainers }
  ;;

  let superpose t1 t2 =
    let f o1 o2 =
      match o2 with
      | Some _ as x -> x
      | None -> o1
    in
    { source = f t1.source t2.source
    ; authors = f t1.authors t2.authors
    ; license = f t1.license t2.license
    ; homepage = f t1.homepage t2.homepage
    ; documentation = f t1.documentation t2.documentation
    ; bug_reports = f t1.bug_reports t2.bug_reports
    ; maintainers = f t1.maintainers t2.maintainers
    }
  ;;
end

type opam_file =
  | Exists of bool
  | Generated

type t =
  { id : Id.t
  ; opam_file : Path.Source.t
  ; loc : Loc.t
  ; synopsis : string option
  ; description : string option
  ; depends : Dependency.t list
  ; conflicts : Dependency.t list
  ; depopts : Dependency.t list
  ; info : Info.t
  ; version : Package_version.t option
  ; has_opam_file : opam_file
  ; tags : string list
  ; deprecated_package_names : Loc.t Name.Map.t
  ; sites : Section.t Site.Map.t
  ; allow_empty : bool
  }

(* Package name are globally unique, so we can reasonably expect that there will
   always be only a single value of type [t] with a given name in memory. That's
   why we only hash the name. *)
let hash t = Id.hash t.id
let name t = t.id.name
let dir t = t.id.dir

let set_inside_opam_dir t ~dir =
  { t with opam_file = Path.Source.relative dir (Name.opam_fn t.id.name) }
;;

let encode
  (name : Name.t)
  { id = _
  ; loc = _
  ; has_opam_file = _
  ; synopsis
  ; description
  ; depends
  ; conflicts
  ; depopts
  ; info
  ; version
  ; tags
  ; deprecated_package_names
  ; sites
  ; allow_empty
  ; opam_file = _
  }
  =
  let open Dune_lang.Encoder in
  let fields =
    Info.encode_fields info
    @ record_fields
        [ field "name" Name.encode name
        ; field_o "synopsis" string synopsis
        ; field_o "description" string description
        ; field_l "depends" Dependency.encode depends
        ; field_l "conflicts" Dependency.encode conflicts
        ; field_l "depopts" Dependency.encode depopts
        ; field_o "version" Package_version.encode version
        ; field "tags" (list string) ~default:[] tags
        ; field_l
            "deprecated_package_names"
            Name.encode
            (Name.Map.keys deprecated_package_names)
        ; field_l "sits" (pair Site.encode Section.encode) (Site.Map.to_list sites)
        ; field_b "allow_empty" allow_empty
        ]
  in
  list sexp (string "package" :: fields)
;;

let decode_name ~version =
  if version >= (3, 11) then Name.decode_opam_compatible else Name.decode
;;

let decode =
  let open Dune_lang.Decoder in
  let name_map syntax of_list_map to_string name decode print_value error_msg =
    let+ names = field ~default:[] name (syntax >>> repeat decode) in
    match of_list_map names ~f:(fun (loc, s) -> s, loc) with
    | Ok x -> x
    | Error (name, (loc1, _), (loc2, _)) ->
      User_error.raise
        [ Pp.textf "%s %s is declared twice:" error_msg (to_string name)
        ; Pp.textf "- %s" (print_value loc1)
        ; Pp.textf "- %s" (print_value loc2)
        ]
  in
  fun ~dir ->
    fields
    @@ let* version = Syntax.get_exn Stanza.syntax in
       let+ loc = loc
       and+ name = field "name" (decode_name ~version)
       and+ synopsis = field_o "synopsis" string
       and+ description = field_o "description" string
       and+ version =
         field_o
           "version"
           (Dune_lang.Syntax.since Stanza.syntax (2, 5) >>> Package_version.decode)
       and+ depends = field ~default:[] "depends" (repeat Dependency.decode)
       and+ conflicts = field ~default:[] "conflicts" (repeat Dependency.decode)
       and+ depopts = field ~default:[] "depopts" (repeat Dependency.decode)
       and+ info = Info.decode ~since:(2, 0) ()
       and+ tags = field "tags" (enter (repeat string)) ~default:[]
       and+ deprecated_package_names =
         name_map
           (Dune_lang.Syntax.since Stanza.syntax (2, 0))
           Name.Map.of_list_map
           Name.to_string
           "deprecated_package_names"
           (located Name.decode)
           Loc.to_file_colon_line
           "Deprecated package name"
       and+ sites =
         name_map
           (Dune_lang.Syntax.since Stanza.syntax (2, 8))
           Site.Map.of_list_map
           Site.to_string
           "sites"
           (pair Section.decode Site.decode)
           Section.to_string
           "Site location name"
       and+ allow_empty =
         field_b "allow_empty" ~check:(Dune_lang.Syntax.since Stanza.syntax (3, 0))
       and+ lang_version = Dune_lang.Syntax.get_exn Stanza.syntax in
       let allow_empty = lang_version < (3, 0) || allow_empty in
       let id = { Id.name; dir } in
       let opam_file = Id.default_opam_file id in
       { id
       ; loc
       ; synopsis
       ; description
       ; depends
       ; conflicts
       ; depopts
       ; info
       ; version
       ; has_opam_file = Exists false
       ; tags
       ; deprecated_package_names
       ; sites
       ; allow_empty
       ; opam_file
       }
;;

let dyn_of_opam_file =
  let open Dyn in
  function
  | Exists b -> variant "Exists" [ bool b ]
  | Generated -> variant "Generated" []
;;

let to_dyn
  { id
  ; version
  ; synopsis
  ; description
  ; depends
  ; conflicts
  ; depopts
  ; info
  ; has_opam_file
  ; tags
  ; loc = _
  ; deprecated_package_names
  ; sites
  ; allow_empty
  ; opam_file = _
  }
  =
  let open Dyn in
  record
    [ "id", Id.to_dyn id
    ; "synopsis", option string synopsis
    ; "description", option string description
    ; "depends", list Dependency.to_dyn depends
    ; "conflicts", list Dependency.to_dyn conflicts
    ; "depopts", list Dependency.to_dyn depopts
    ; "info", Info.to_dyn info
    ; "has_opam_file", dyn_of_opam_file has_opam_file
    ; "tags", list string tags
    ; "version", option Package_version.to_dyn version
    ; "deprecated_package_names", Name.Map.to_dyn Loc.to_dyn_hum deprecated_package_names
    ; "sites", Site.Map.to_dyn Section.to_dyn sites
    ; "allow_empty", Bool allow_empty
    ]
;;

let opam_file t = t.opam_file
let meta_file t = Path.Source.relative t.id.dir (Name.meta_fn t.id.name)
let file ~dir ~name = Path.relative dir (Name.to_string name ^ opam_ext)
let deprecated_meta_file t name = Path.Source.relative t.id.dir (Name.meta_fn name)

let default name dir =
  let depends =
    [ { Dependency.name = Name.of_string "ocaml"; constraint_ = None }
    ; { name = Name.of_string "dune"; constraint_ = None }
    ]
  in
  let id = { Id.name; dir } in
  { id
  ; loc = Loc.none
  ; version = None
  ; synopsis = Some "A short synopsis"
  ; description = Some "A longer description"
  ; depends
  ; conflicts = []
  ; info = Info.empty
  ; depopts = []
  ; has_opam_file = Exists false
  ; tags = [ "topics"; "to describe"; "your"; "project" ]
  ; deprecated_package_names = Name.Map.empty
  ; sites = Site.Map.empty
  ; allow_empty = false
  ; opam_file = Id.default_opam_file id
  }
;;

let load_opam_file file name =
  let loc = Loc.in_file (Path.source file) in
  let open Memo.O in
  let+ opam =
    let+ opam =
      Fs_memo.with_lexbuf_from_file (In_source_dir file) ~f:(fun lexbuf ->
        try Ok (Opam_file.parse lexbuf) with
        | User_error.E _ as exn -> Error exn)
    in
    match opam with
    | Ok s -> Some s
    | Error exn ->
      User_warning.emit
        ~loc
        [ Pp.text
            "Unable to read opam file. Some information about this package such as its \
             version will be ignored."
        ; Pp.textf "Reason: %s" (Printexc.to_string exn)
        ];
      None
  in
  let open Option.O in
  let get_one name =
    let* opam = opam in
    let* value = Opam_file.get_field opam name in
    match value.pelem with
    | String s -> Some s
    | _ -> None
  in
  let get_many name =
    let* opam = opam in
    let* value = Opam_file.get_field opam name in
    match value.pelem with
    | String s -> Some [ s ]
    | List l ->
      let+ l =
        List.fold_left
          l.pelem
          ~init:(Some [])
          ~f:(fun acc (v : OpamParserTypes.FullPos.value) ->
            let* acc = acc in
            match v.pelem with
            | String s -> Some (s :: acc)
            | _ -> None)
      in
      List.rev l
    | _ -> None
  in
  let id = { Id.name; dir = Path.Source.parent_exn file } in
  { id
  ; loc
  ; version = get_one "version" |> Option.map ~f:Package_version.of_string
  ; conflicts = []
  ; depends = []
  ; depopts = []
  ; info =
      { maintainers = get_many "maintainer"
      ; authors = get_many "authors"
      ; homepage = get_one "homepage"
      ; bug_reports = get_one "bug-reports"
      ; documentation = get_one "doc"
      ; license = get_many "license"
      ; source =
          (let+ url = get_one "dev-repo" in
           Source_kind.Url url)
      }
  ; synopsis = get_one "synopsis"
  ; description = get_one "description"
  ; has_opam_file = Exists true
  ; tags = Option.value (get_many "tags") ~default:[]
  ; deprecated_package_names = Name.Map.empty
  ; sites = Site.Map.empty
  ; allow_empty = true
  ; opam_file = Id.default_opam_file id
  }
;;

let equal = Poly.equal

let missing_deps (t : t) ~effective_deps =
  let specified_deps =
    List.map t.depends ~f:(fun (dep : Dependency.t) -> dep.name) |> Name.Set.of_list
  in
  Name.Set.diff effective_deps specified_deps
;;

let to_local_package t =
  { Dune_pkg.Local_package.name = name t
  ; version = t.version
  ; dependencies = t.depends
  ; loc = t.loc
  }
;;
