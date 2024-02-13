open! Import
module Stanza = Dune_lang.Stanza
module Opam_file = Dune_pkg.Opam_file
module Dependency = Dune_pkg.Package_dependency

let opam_ext = ".opam"

module Name = struct
  include Dune_lang.Package_name

  let of_opam_file_basename basename =
    let open Option.O in
    let* name = String.drop_suffix basename ~suffix:opam_ext in
    of_string_opt name
  ;;

  let opam_fn (t : t) = to_string t ^ opam_ext

  let of_opam_package_name opam_package_name =
    OpamPackage.Name.to_string opam_package_name |> of_string
  ;;

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

type opam_file =
  | Exists of bool
  | Generated

(* we need the original opam file when passing it [$ dune pkg lock] we want to
   to allow the opam library interpret the opam file directly. *)
type original_opam_file =
  { file : Path.Source.t
  ; contents : string
  }

type t =
  { id : Id.t
  ; opam_file : Path.Source.t
  ; loc : Loc.t
  ; synopsis : string option
  ; description : string option
  ; depends : Dependency.t list
  ; conflicts : Dependency.t list
  ; depopts : Dependency.t list
  ; info : Package_info.t
  ; version : Package_version.t option
  ; has_opam_file : opam_file
  ; tags : string list
  ; deprecated_package_names : Loc.t Name.Map.t
  ; sites : Section.t Site.Map.t
  ; allow_empty : bool
  ; original_opam_file : original_opam_file option
  }

(* Package name are globally unique, so we can reasonably expect that there will
   always be only a single value of type [t] with a given name in memory. That's
   why we only hash the name. *)
let hash t = Id.hash t.id
let name t = t.id.name
let dir t = t.id.dir
let loc t = t.loc
let deprecated_package_names t = t.deprecated_package_names
let set_has_opam_file t has_opam_file = { t with has_opam_file }
let version t = t.version
let depends t = t.depends
let conflicts t = t.conflicts
let depopts t = t.depopts
let tags t = t.tags
let synopsis t = t.synopsis
let info t = t.info
let description t = t.description
let id t = t.id

let set_inside_opam_dir t ~dir =
  { t with opam_file = Path.Source.relative dir (Name.opam_fn t.id.name) }
;;

let set_version_and_info t ~version ~info = { t with version; info }

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
  ; original_opam_file = _
  }
  =
  let open Dune_lang.Encoder in
  let fields =
    Package_info.encode_fields info
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
        ; field_l "sites" (pair Site.encode Section.encode) (Site.Map.to_list sites)
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
       and+ info = Package_info.decode ~since:(2, 0) ()
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
       ; original_opam_file = None
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
  ; original_opam_file = _
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
    ; "info", Package_info.to_dyn info
    ; "has_opam_file", dyn_of_opam_file has_opam_file
    ; "tags", list string tags
    ; "version", option Package_version.to_dyn version
    ; "deprecated_package_names", Name.Map.to_dyn Loc.to_dyn_hum deprecated_package_names
    ; "sites", Site.Map.to_dyn Section.to_dyn sites
    ; "allow_empty", Bool allow_empty
    ]
;;

let opam_file t = t.opam_file
let file ~dir ~name = Path.relative dir (Name.to_string name ^ opam_ext)
let sites t = t.sites
let has_opam_file t = t.has_opam_file
let allow_empty t = t.allow_empty
let map_depends t ~f = { t with depends = f t.depends }

let create ~name ~dir ~depends ~synopsis ~description ~tags =
  let id = { Id.name; dir } in
  { id
  ; loc = Loc.none
  ; version = None
  ; synopsis
  ; description
  ; depends
  ; conflicts = []
  ; info = Package_info.empty
  ; depopts = []
  ; has_opam_file = Exists false
  ; tags
  ; deprecated_package_names = Name.Map.empty
  ; sites = Site.Map.empty
  ; allow_empty = false
  ; opam_file = Id.default_opam_file id
  ; original_opam_file = None
  }
;;

let load_opam_file_with_contents ~contents:opam_file_string file name =
  let loc = Loc.in_file (Path.source file) in
  let opam =
    let opam =
      let lexbuf =
        Lexbuf.from_string opam_file_string ~fname:(Path.Source.to_string file)
      in
      try Ok (Opam_file.parse lexbuf) with
      | User_error.E _ as exn -> Error exn
    in
    match opam with
    | Ok s -> Some s
    | Error exn ->
      (* CR-rgrinberg: make it possible to disable this warning *)
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
    let* value =
      let* opam = opam in
      Opam_file.get_field opam name
    in
    match value.pelem with
    | String s -> Some s
    | _ -> None
  in
  let get_many name =
    let* value =
      let* opam = opam in
      Opam_file.get_field opam name
    in
    match value.pelem with
    | String s -> Some [ s ]
    | List l ->
      List.fold_left
        l.pelem
        ~init:(Some [])
        ~f:(fun acc (v : OpamParserTypes.FullPos.value) ->
          let* acc = acc in
          match v.pelem with
          | String s -> Some (s :: acc)
          | _ -> None)
      >>| List.rev
    | _ -> None
  in
  let dir = Path.Source.parent_exn file in
  let id = { Id.name; dir } in
  { id
  ; opam_file = Id.default_opam_file id
  ; loc
  ; version = get_one "version" |> Option.map ~f:Package_version.of_string
  ; conflicts = []
  ; depends = []
  ; depopts = []
  ; info =
      Package_info.create
        ~maintainers:(get_many "maintainer")
        ~authors:(get_many "authors")
        ~homepage:(get_one "homepage")
        ~bug_reports:(get_one "bug-reports")
        ~documentation:(get_one "doc")
        ~license:(get_many "license")
        ~source:
          (let+ url = get_one "dev-repo" in
           Source_kind.Url url)
  ; synopsis = get_one "synopsis"
  ; description = get_one "description"
  ; has_opam_file = Exists true
  ; tags = Option.value (get_many "tags") ~default:[]
  ; deprecated_package_names = Name.Map.empty
  ; sites = Site.Map.empty
  ; allow_empty = true
  ; original_opam_file = Some { file; contents = opam_file_string }
  }
;;

let equal =
  (* TODO get rid of this *)
  Poly.equal
;;

let to_local_package t =
  match t.original_opam_file with
  | None ->
    { Dune_pkg.Local_package.name = name t
    ; version = t.version
    ; dependencies = t.depends
    ; conflicts = t.conflicts
    ; depopts = t.depopts
    ; loc = t.loc
    ; conflict_class = []
    ; pins = Name.Map.empty
    }
  | Some { file; contents = opam_file_string } ->
    let opam_file =
      Dune_pkg.Opam_file.read_from_string_exn
        ~contents:opam_file_string
        (Path.source file)
    in
    let convert_filtered_formula filtered_formula =
      Dune_pkg.Package_dependency.list_of_opam_filtered_formula t.loc filtered_formula
    in
    let dependencies = convert_filtered_formula (OpamFile.OPAM.depends opam_file) in
    let conflicts = convert_filtered_formula (OpamFile.OPAM.conflicts opam_file) in
    let depopts = convert_filtered_formula (OpamFile.OPAM.depopts opam_file) in
    let conflict_class =
      OpamFile.OPAM.conflict_class opam_file |> List.map ~f:Name.of_opam_package_name
    in
    let pins =
      match
        OpamFile.OPAM.pin_depends opam_file
        |> List.map ~f:(fun (pkg, url) ->
          let name = Name.of_opam_package_name (OpamPackage.name pkg) in
          let version =
            Dune_pkg.Package_version.of_opam_package_version (OpamPackage.version pkg)
          in
          let loc = Loc.in_file (Path.source file) in
          ( name
          , { Dune_pkg.Local_package.loc; version; url = loc, url; name; origin = `Opam }
          ))
        |> Name.Map.of_list
      with
      | Ok x -> x
      | Error (_, pkg, _) ->
        User_error.raise
          ~loc:pkg.loc
          [ Pp.textf "package %s is already pinned" (Name.to_string pkg.name) ]
    in
    { Dune_pkg.Local_package.name = name t
    ; version = t.version
    ; dependencies
    ; conflicts
    ; depopts
    ; loc = t.loc
    ; conflict_class
    ; pins
    }
;;
