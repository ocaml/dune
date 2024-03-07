open! Stdune

module Version = struct
  module T = struct
    type t = int * int

    let compare (major_a, minor_a) (major_b, minor_b) =
      let open Ordering.O in
      let= () = Int.compare major_a major_b in
      Int.compare minor_a minor_b
    ;;

    let to_dyn t =
      let open Dyn in
      pair int int t
    ;;
  end

  include T
  module Infix = Comparator.Operators (T)
  module Map = Map.Make (T)

  let equal = Infix.equal
  let to_string (a, b) = sprintf "%u.%u" a b
  let hash = Poly.hash
  let encode t = Encoder.string (to_string t)

  let decode : t Decoder.t =
    let open Decoder in
    raw
    >>| function
    | Atom (loc, A s) ->
      (match Scanf.sscanf s "%u.%u%s" (fun a b s -> (a, b), s) with
       | Ok (v, "") -> v
       | Ok (((a, b) as v), s) ->
         let is_error = v >= (3, 0) in
         User_warning.emit
           ~loc
           ~is_error
           [ Pp.textf "The %S part is ignored here." s
           ; Pp.textf "This version is parsed as just %d.%d." a b
           ];
         v
       | Error () -> User_error.raise ~loc [ Pp.text "Atom of the form NNN.NNN expected" ])
    | sexp -> User_error.raise ~loc:(Ast.loc sexp) [ Pp.text "Atom expected" ]
  ;;

  let can_read
    ~parser_version:(parser_major, parser_minor)
    ~data_version:(data_major, data_minor)
    =
    let open Int.Infix in
    parser_major = data_major && parser_minor >= data_minor
  ;;

  let min = Ordering.min compare
  let max = Ordering.max compare
end

module Supported_versions : sig
  type t

  val make : (Version.t * [ `Since of Version.t | `Deleted_in of Version.t ]) list -> t
  val to_dyn : t -> Dyn.t
  val greatest_supported_version : t -> Version.t option

  val greatest_supported_version_for_dune_lang
    :  t
    -> dune_lang_ver:Version.t
    -> Version.t option

  val minimum_versions : t -> (Version.t * Version.t) option

  val status
    :  t
    -> Version.t
    -> dune_lang_ver:Version.t
    -> [ `Supported
       | `Deleted_in of Version.t
       | `Unsupported_in_project of (Version.t * Version.t) list * Version.t option
       ]
end = struct
  (* The extension supported versions are declared using an explicit list of all
     versions but stored as a map from major versions to maps from minor version
     to dune_lang required versions. For instance, if:

     - version 1.0 of an extension was introduced in Dune 1.4

     - version 1.1 was introduced in Dune 1.6

     - version 1.2 was introduced in Dune 2.3

     - version 2.0 was introduced in Dune 2.4

     we'd have the following map (in associative list syntax):

     {[
       [ 1, [ 0, (1, 4); 1, (1, 6); 2, (2, 3) ]; 2, [ 0, (2, 3) ] ]
     ]} *)
  type t =
    { version_map : Version.t Int.Map.t Int.Map.t
    ; deleted_in : Version.t Version.Map.t
    }

  let to_dyn { version_map; deleted_in } =
    Dyn.record
      [ "version_map", Int.Map.to_dyn (Int.Map.to_dyn Version.to_dyn) version_map
      ; "deleted_in", Version.Map.to_dyn Version.to_dyn deleted_in
      ]
  ;;

  (* We convert the exposed extension version type: {[ (Version.t * [ `Since of
     Version.t ]) list ]} which is a list of fully qualified versions paired
     with the corresponding dune_lang version. To the internal representation:
     {[ (Version.t Int.Map.t) Int.Map.t ]} which is a list of major versions
     paired with lists of minor versions paires with a dune_lang version. *)
  let make
    (versions : (Version.t * [ `Since of Version.t | `Deleted_in of Version.t ]) list)
    : t
    =
    let version_map, deleted_in =
      List.fold_left
        versions
        ~init:(Int.Map.empty, Version.Map.empty)
        ~f:(fun (major_map, deleted_in) ((major, minor), version_spec) ->
          match version_spec with
          | `Since lang_ver ->
            let add_minor minor_map = Some (Int.Map.add_exn minor_map minor lang_ver) in
            let major_map' =
              Int.Map.update major_map major ~f:(function
                | Some minor_map -> add_minor minor_map
                | None -> add_minor Int.Map.empty)
            in
            major_map', deleted_in
          | `Deleted_in lang_ver ->
            let deleted_in' = Version.Map.add_exn deleted_in (major, minor) lang_ver in
            major_map, deleted_in')
    in
    { version_map; deleted_in }
  ;;

  let remove_incompatible_versions lang_ver =
    Int.Map.filter_map ~f:(fun minors ->
      let minors = Int.Map.filter minors ~f:(fun min_lang -> lang_ver >= min_lang) in
      Option.some_if (not (Int.Map.is_empty minors)) minors)
  ;;

  let greatest_supported_version_in_map map =
    let open Option.O in
    let* major, minors = Int.Map.max_binding map in
    let+ minor, _ = Int.Map.max_binding minors in
    major, minor
  ;;

  let greatest_supported_version t = greatest_supported_version_in_map t.version_map

  let greatest_supported_version_for_dune_lang t ~dune_lang_ver =
    let compat = remove_incompatible_versions dune_lang_ver t.version_map in
    greatest_supported_version_in_map compat
  ;;

  let get_min_lang_ver t (major, minor) =
    let open Option.O in
    let* minors = Int.Map.find t.version_map major in
    Int.Map.find minors minor
  ;;

  let is_supported t (major, minor) lang_ver =
    match Int.Map.find t.version_map major with
    | None -> false
    | Some t ->
      (match Int.Map.find t minor with
       | Some min_lang_ver -> lang_ver >= min_lang_ver
       | None -> false)
  ;;

  let supported_ranges lang_ver (t : t) =
    let compat = remove_incompatible_versions lang_ver t.version_map in
    Int.Map.to_list_map compat ~f:(fun major minors ->
      let max_minor, _ = Option.value_exn (Int.Map.max_binding minors) in
      let lower_bound =
        (* Map 0.0 to 0.1 since 0.0 is not a valid version number *)
        if major = 0 then 0, 1 else major, 0
      in
      let upper_bound = major, max_minor in
      assert (lower_bound <= upper_bound);
      lower_bound, upper_bound)
  ;;

  let minimum_versions t =
    let open Option.O in
    let* major, major_map = Int.Map.min_binding t.version_map in
    let+ minor, lang = Int.Map.min_binding major_map in
    (major, minor), lang
  ;;

  let status t ver ~dune_lang_ver =
    if is_supported t ver dune_lang_ver
    then `Supported
    else (
      match Version.Map.find t.deleted_in ver with
      | Some version -> `Deleted_in version
      | None ->
        let supported_ranges = supported_ranges dune_lang_ver t in
        let min_lang_ver = get_min_lang_ver t ver in
        `Unsupported_in_project (supported_ranges, min_lang_ver))
  ;;
end

type t =
  { name : string
  ; desc : string
  ; key : key Univ_map.Key.t
  ; supported_versions : Supported_versions.t
  ; experimental : bool
  }

and key =
  | Active of Version.t
  | Inactive of
      { lang : t
      ; dune_lang_ver : Version.t
      }

let to_dyn { name; desc; key = _; supported_versions; experimental } =
  let open Dyn in
  record
    [ "name", string name
    ; "desc", string desc
    ; "supported_versions", Supported_versions.to_dyn supported_versions
    ; "experimental", bool experimental
    ]
;;

module Key = struct
  type nonrec t = key =
    | Active of Version.t
    | Inactive of
        { lang : t
        ; dune_lang_ver : Version.t
        }

  let to_dyn =
    let open Dyn in
    function
    | Active v -> Version.to_dyn v
    | Inactive { lang; dune_lang_ver } ->
      record [ "lang", to_dyn lang; "dune_lang_ver", Version.to_dyn dune_lang_ver ]
  ;;
end

module Error_msg = struct
  let since t ver ~what =
    let lang_or_using = if t.name = "dune" then "lang" else "using" in
    Printf.sprintf
      "%s is only available since version %s of %s. Please update your dune-project file \
       to have (%s %s %s)."
      what
      (Version.to_string ver)
      t.desc
      lang_or_using
      t.name
      (Version.to_string ver)
  ;;
end

module Error = struct
  let since loc t ver ~what =
    User_error.raise ~loc [ Pp.text (Error_msg.since t ver ~what) ]
  ;;

  let renamed_in loc t ver ~what ~to_ =
    User_error.raise
      ~loc
      [ Pp.textf
          "%s was renamed to '%s' in the %s version of %s"
          what
          to_
          (Version.to_string ver)
          t.desc
      ]
  ;;

  let deleted_in ?(extra_info = "") loc t ?(repl = []) ver ~what =
    User_error.raise
      ~loc
      (Pp.concat
         [ Pp.textf
             "%s was deleted in version %s of %s."
             what
             (Version.to_string ver)
             t.desc
         ; (if extra_info = "" then Pp.nop else Pp.space)
         ; Pp.text extra_info
         ]
       :: repl)
  ;;

  let inactive loc t ~dune_lang_ver ~what =
    let greatest_supported_version =
      Supported_versions.greatest_supported_version_for_dune_lang
        ~dune_lang_ver
        t.supported_versions
    in
    User_error.raise
      ~loc
      ([ Pp.textf
           "%s is available only when %s is enabled in the dune-project file. You must \
            enable it using (using %s %s) in your dune-project file."
           what
           t.name
           t.name
           (match greatest_supported_version with
            | Some v -> Version.to_string v
            | None -> "..")
       ]
       @
       if t.experimental
       then
         [ Pp.textf
             "Note however that %s is experimental and might change without notice in \
              the future."
             t.name
         ]
       else (
         match greatest_supported_version with
         | Some _ -> []
         | None ->
           let first_version_message =
             match Supported_versions.minimum_versions t.supported_versions with
             | None -> ""
             | Some (min_lang_version, min_dune_version) ->
               sprintf
                 " The first version of this plugin is %s and was introduced in dune %s."
                 (Version.to_string min_lang_version)
                 (Version.to_string min_dune_version)
           in
           [ Pp.textf
               "Note however that the currently selected version of dune (%s) does not \
                support this plugin.%s"
               (Version.to_string dune_lang_ver)
               first_version_message
           ]))
  ;;
end

module Warning = struct
  let deprecated_in ?(extra_info = "") loc t ?(repl = []) ver ~what =
    User_warning.emit
      ~loc
      (Pp.concat
         [ Pp.textf
             "%s was deprecated in version %s of %s."
             what
             (Version.to_string ver)
             t.desc
         ; (if extra_info = "" then Pp.nop else Pp.space)
         ; Pp.text extra_info
         ]
       :: repl)
  ;;
end

let create ?(experimental = false) ~name ~desc supported_versions =
  { name
  ; desc
  ; key = Univ_map.Key.create ~name Key.to_dyn
  ; supported_versions = Supported_versions.make supported_versions
  ; experimental
  }
;;

let name t = t.name

let check_supported ~dune_lang_ver t (loc, ver) =
  match Supported_versions.status t.supported_versions ver ~dune_lang_ver with
  | `Supported -> ()
  | `Deleted_in deleted_in ->
    let min_ext_ver, min_dune_lang_ver =
      match Supported_versions.minimum_versions t.supported_versions with
      | None -> None, None
      | Some (x, y) -> Some x, Some y
    in
    let please_port_message =
      match min_ext_ver with
      | None -> ""
      | Some min_ext_ver ->
        sprintf
          " Please port this project to a newer version of the extension, such as %s."
          (Version.to_string min_ext_ver)
    in
    let hints =
      match min_dune_lang_ver with
      | None -> []
      | Some min_dune_lang_ver ->
        if dune_lang_ver >= min_dune_lang_ver
        then []
        else
          [ Pp.textf
              "You will also need to upgrade to (lang dune %s)."
              (Version.to_string min_dune_lang_ver)
          ]
    in
    User_error.raise
      ~loc
      ~hints
      [ Pp.textf
          "Version %s of the %s extension has been deleted in Dune %s.%s"
          (Version.to_string ver)
          t.name
          (Version.to_string deleted_in)
          please_port_message
      ]
  | `Unsupported_in_project (supported_ranges, min_lang_ver) ->
    let dune_ver_text v =
      Printf.sprintf "version %s of the dune language" (Version.to_string v)
    in
    let until =
      match min_lang_ver with
      | Some v -> Printf.sprintf " until %s" (dune_ver_text v)
      | None -> ""
    in
    let supported =
      (if List.is_empty supported_ranges
       then Pp.textf "There are no supported versions of this extension in %s."
       else Pp.textf "Supported versions of this extension in %s:")
        (dune_ver_text dune_lang_ver)
    in
    let message =
      [ Pp.textf
          "Version %s of %s is not supported%s."
          (Version.to_string ver)
          t.desc
          until
      ; supported
      ; Pp.enumerate supported_ranges ~f:(fun (a, b) ->
          let open Version.Infix in
          if a = b
          then Pp.text (Version.to_string a)
          else Pp.textf "%s to %s" (Version.to_string a) (Version.to_string b))
      ]
    in
    let is_error = String.is_empty until || dune_lang_ver >= (2, 6) in
    User_warning.emit ~is_error ~loc message
;;

let greatest_supported_version t =
  Supported_versions.greatest_supported_version t.supported_versions
;;

let greatest_supported_version_exn t =
  match greatest_supported_version t with
  | Some s -> s
  | None ->
    Code_error.raise
      "no supported versions for extension"
      [ "supported_versions", Supported_versions.to_dyn t.supported_versions ]
;;

let key t = t.key
let experimental t = t.experimental

open Decoder

let set t ver parser = set t.key ver parser

let desc () =
  let+ kind = kind in
  match kind with
  | Values (loc, None) -> loc, "This syntax"
  | Fields (loc, None) -> loc, "This field"
  | Values (loc, Some s) -> loc, sprintf "'%s'" s
  | Fields (loc, Some s) -> loc, sprintf "Field '%s'" s
;;

let get_exn t =
  get t.key
  >>= function
  | Some (Active x) -> return x
  | Some (Inactive { dune_lang_ver; lang }) ->
    let* loc, what = desc () in
    Error.inactive loc lang ~what ~dune_lang_ver
  | None ->
    let+ context = get_all in
    Code_error.raise
      "Syntax identifier is unset"
      [ "name", Dyn.string t.name
      ; "supported_versions", Supported_versions.to_dyn t.supported_versions
      ; "context", Univ_map.to_dyn context
      ]
;;

let deleted_in ?(extra_info = "") t ver =
  let open Version.Infix in
  let* current_ver = get_exn t in
  if current_ver < ver
  then return ()
  else
    let* loc, what = desc () in
    Error.deleted_in ~extra_info loc t ver ~what
;;

let deprecated_in ?(extra_info = "") t ver =
  let open Version.Infix in
  let* current_ver = get_exn t in
  if current_ver < ver
  then return ()
  else
    let+ loc, what = desc () in
    Warning.deprecated_in ~extra_info loc t ver ~what
;;

let renamed_in t ver ~to_ =
  let open Version.Infix in
  let* current_ver = get_exn t in
  if current_ver < ver
  then return ()
  else
    let+ loc, what = desc () in
    Error.renamed_in loc t ver ~what ~to_
;;

let since ?what ?(fatal = true) t ver =
  let open Version.Infix in
  let* current_ver = get_exn t in
  if current_ver >= ver
  then return ()
  else
    let* loc, what_ctx = desc () in
    let what = Option.value what ~default:what_ctx in
    if fatal
    then Error.since loc t ver ~what
    else User_warning.emit ~loc [ Pp.text (Error_msg.since t ver ~what) ];
    return ()
;;
