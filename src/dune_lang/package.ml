open Stdune
open Dune_sexp
module Name = Package_name

module Id = struct
  module T = struct
    type t =
      { name : Name.t
      ; dir : Path.Source.t
      }

    let compare { name; dir } pkg =
      match Name.compare name pkg.name with
      | Eq -> Path.Source.compare dir pkg.dir
      | e -> e
    ;;

    let to_dyn { dir; name } =
      Dyn.record [ "name", Name.to_dyn name; "dir", Path.Source.to_dyn dir ]
    ;;
  end

  include T

  let hash { name; dir } = Tuple.T2.hash Name.hash Path.Source.hash (name, dir)
  let name t = t.name

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
  ; depends : Package_dependency.t list
  ; conflicts : Package_dependency.t list
  ; depopts : Package_dependency.t list
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
let original_opam_file t = t.original_opam_file
let set_inside_opam_dir t ~dir = { t with opam_file = Name.file t.id.name ~dir }
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
  let open Encoder in
  let fields =
    Package_info.encode_fields info
    @ record_fields
        [ field "name" Name.encode name
        ; field_o "synopsis" string synopsis
        ; field_o "description" string description
        ; field_l "depends" Package_dependency.encode depends
        ; field_l "conflicts" Package_dependency.encode conflicts
        ; field_l "depopts" Package_dependency.encode depopts
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
  let open Decoder in
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
         field_o "version" (Syntax.since Stanza.syntax (2, 5) >>> Package_version.decode)
       and+ depends = field ~default:[] "depends" (repeat Package_dependency.decode)
       and+ conflicts = field ~default:[] "conflicts" (repeat Package_dependency.decode)
       and+ depopts = field ~default:[] "depopts" (repeat Package_dependency.decode)
       and+ info = Package_info.decode ~since:(2, 0) ()
       and+ tags = field "tags" (enter (repeat string)) ~default:[]
       and+ deprecated_package_names =
         name_map
           (Syntax.since Stanza.syntax (2, 0))
           Name.Map.of_list_map
           Name.to_string
           "deprecated_package_names"
           (located Name.decode)
           Loc.to_file_colon_line
           "Deprecated package name"
       and+ sites =
         name_map
           (Syntax.since Stanza.syntax (2, 8))
           Site.Map.of_list_map
           Site.to_string
           "sites"
           (pair Section.decode Site.decode)
           Section.to_string
           "Site location name"
       and+ allow_empty = field_b "allow_empty" ~check:(Syntax.since Stanza.syntax (3, 0))
       and+ lang_version = Syntax.get_exn Stanza.syntax in
       let allow_empty = lang_version < (3, 0) || allow_empty in
       let id = { Id.name; dir } in
       let opam_file = Name.file id.name ~dir:id.dir in
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
    ; "depends", list Package_dependency.to_dyn depends
    ; "conflicts", list Package_dependency.to_dyn conflicts
    ; "depopts", list Package_dependency.to_dyn depopts
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
let sites t = t.sites
let has_opam_file t = t.has_opam_file
let allow_empty t = t.allow_empty
let map_depends t ~f = { t with depends = f t.depends }

let create
  ~name
  ~loc
  ~version
  ~conflicts
  ~depends
  ~depopts
  ~info
  ~has_opam_file
  ~dir
  ~sites
  ~allow_empty
  ~synopsis
  ~description
  ~tags
  ~original_opam_file
  ~deprecated_package_names
  =
  let id = { Id.name; dir } in
  { id
  ; loc
  ; version
  ; synopsis
  ; description
  ; depends
  ; conflicts
  ; info
  ; depopts
  ; has_opam_file
  ; tags
  ; deprecated_package_names
  ; sites
  ; allow_empty
  ; opam_file = Name.file name ~dir
  ; original_opam_file
  }
;;
