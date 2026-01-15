open Import

(** A vendor stanza specifies a vendored subdirectory and which
    libraries/packages to expose from it.

    Syntax:
    {[
      (vendor fmt.0.9.0 (libraries fmt fmt.tty))
      (vendor yojson.1.7.0 (libraries (yojson :as yojson_v1)))
      (vendor cohttp.6.0.0 (libraries :standard \ cohttp-async))
    ]}

    The [(libraries ...)] field uses ordered set language with aliasing:
    - [:standard] means all libraries found in the directory
    - [\ name] excludes items from the set
    - [(name :as alias)] exposes library under a different name *)

module Library_entry = struct
  type t =
    { lib_name : Lib_name.t
    ; alias : Lib_name.t option
    }

  let to_dyn { lib_name; alias } =
    Dyn.record
      [ "lib_name", Lib_name.to_dyn lib_name; "alias", Dyn.option Lib_name.to_dyn alias ]
  ;;

  let exposed_name t = Option.value t.alias ~default:t.lib_name
  let has_alias t = Option.is_some t.alias
end

module Libraries_ast = struct
  (** AST for the libraries field with aliasing support *)
  type t = (Library_entry.t, Ordered_set_lang.Ast.expanded) Ordered_set_lang.Ast.t

  let decode =
    let open Decoder in
    (* Simple element: just a library name *)
    let simple_elt =
      let+ lib_name = Lib_name.decode in
      Ordered_set_lang.Ast.Element { Library_entry.lib_name; alias = None }
    in
    (* List element: (name :as alias) *)
    let list_elt =
      enter
        (let* lib_name = Lib_name.decode in
         let* () = keyword ":as" in
         let+ alias = Lib_name.decode in
         Ordered_set_lang.Ast.Element { Library_entry.lib_name; alias = Some alias })
    in
    Ordered_set_lang.Parse.without_include ~elt:simple_elt ~list_elt ()
  ;;

  let standard : t = Ordered_set_lang.Ast.Standard

  let is_standard = function
    | Ordered_set_lang.Ast.Standard -> true
    | _ -> false
  ;;

  (** Check if any entry has aliasing *)
  let rec has_aliasing : t -> bool = function
    | Ordered_set_lang.Ast.Element e -> Library_entry.has_alias e
    | Standard -> false
    | Union elts -> List.exists elts ~f:has_aliasing
    | Diff (a, b) -> has_aliasing a || has_aliasing b
    | _ -> false (* Include is unreachable for expanded AST *)
  ;;

  (** Evaluate the AST against a standard set of libraries.
      Returns list of (original_name, exposed_name) pairs. *)
  let eval (ast : t) ~(standard : Lib_name.t list) : (Lib_name.t * Lib_name.t) list =
    let rec eval_set (ast : t) : Library_entry.t list =
      match ast with
      | Ordered_set_lang.Ast.Element e -> [ e ]
      | Standard ->
        List.map standard ~f:(fun lib_name -> { Library_entry.lib_name; alias = None })
      | Union elts -> List.concat_map elts ~f:eval_set
      | Diff (base, excluded) ->
        let base_entries = eval_set base in
        let excluded_names =
          eval_set excluded
          |> List.map ~f:(fun (e : Library_entry.t) -> e.lib_name)
          |> Lib_name.Set.of_list
        in
        List.filter base_entries ~f:(fun (e : Library_entry.t) ->
          not (Lib_name.Set.mem excluded_names e.lib_name))
      | _ -> [] (* Include is unreachable for expanded AST *)
    in
    eval_set ast
    |> List.map ~f:(fun e -> e.Library_entry.lib_name, Library_entry.exposed_name e)
  ;;

  (** Check if a library should be visible according to this spec.
      For :standard, all libraries are visible.
      For :standard \ exclusions, check exclusion list.
      For explicit list, check membership. *)
  let rec library_visible (ast : t) ~lib_name =
    match ast with
    | Ordered_set_lang.Ast.Standard -> true
    | Element e -> Lib_name.equal e.Library_entry.lib_name lib_name
    | Union elts -> List.exists elts ~f:(library_visible ~lib_name)
    | Diff (base, excluded) ->
      library_visible base ~lib_name && not (library_visible excluded ~lib_name)
    | _ -> false (* Include is unreachable for expanded AST *)
  ;;

  (** Find the exposed name for a library (may be aliased).
      Returns None if the library is not visible.
      Returns Some exposed_name if visible (may equal lib_name or be an alias). *)
  let rec library_exposed_name (ast : t) ~lib_name =
    match ast with
    | Ordered_set_lang.Ast.Standard -> Some lib_name
    | Element e ->
      if Lib_name.equal e.Library_entry.lib_name lib_name
      then Some (Library_entry.exposed_name e)
      else None
    | Union elts -> List.find_map elts ~f:(library_exposed_name ~lib_name)
    | Diff (base, excluded) ->
      if library_visible excluded ~lib_name
      then None
      else library_exposed_name base ~lib_name
    | _ -> None (* Include is unreachable for expanded AST *)
  ;;
end

type t =
  { loc : Loc.t
  ; directory : Filename.t
  ; libraries : Libraries_ast.t
  ; package : Package_name.t option
  }

let decode =
  let open Decoder in
  let* loc = loc in
  let* directory = filename in
  fields
  @@
  let+ libraries = field "libraries" Libraries_ast.decode ~default:Libraries_ast.standard
  and+ package = field_o "package" Package_name.decode in
  { loc; directory; libraries; package }
;;

let to_dyn { loc = _; directory; libraries = _; package = _ } =
  Dyn.record [ "directory", Dyn.string directory ]
;;

let eval_libraries t ~standard = Libraries_ast.eval t.libraries ~standard
let is_standard_libraries t = Libraries_ast.is_standard t.libraries
let package t = t.package

(** Find the exposed name for a library (may be aliased).
    Returns None if the library is not visible.
    Returns Some exposed_name if visible. *)
let library_exposed_name t ~lib_name =
  Libraries_ast.library_exposed_name t.libraries ~lib_name
;;

(** Check if this vendor stanza applies to the given package.
    If no package is specified in the stanza, it applies to all packages.
    If a package is specified, it only applies to that package. *)
let applies_to_package t ~pkg_name =
  match t.package with
  | None -> true
  | Some p -> Package_name.equal p pkg_name
;;

let applies_to_package_list stanzas ~pkg_name =
  match stanzas with
  | [] -> true
  | stanzas -> List.exists stanzas ~f:(fun t -> applies_to_package t ~pkg_name)
;;

(** Find the vendor stanza that applies to a library's package *)
let find_applicable_stanza stanzas ~lib_pkg =
  List.find stanzas ~f:(fun vs ->
    match lib_pkg with
    | None -> package vs = None
    | Some pkg_name -> applies_to_package vs ~pkg_name)
;;

(** Find the vendor stanza that applies to a library based on its package,
    and return the library's visibility and alias information.
    Returns:
    - `Excluded if no stanza applies to the library's package
    - `Included None if the library is visible under its original name
    - `Included (Some alias) if the library is visible under an alias *)
let find_library_status stanzas ~lib_name ~lib_pkg =
  match stanzas with
  | [] -> `Included None (* No vendor stanzas means library is visible *)
  | _ ->
    (match find_applicable_stanza stanzas ~lib_pkg with
     | None -> `Excluded
     | Some vendor ->
       (match library_exposed_name vendor ~lib_name with
        | None -> `Excluded
        | Some exposed_name ->
          if Lib_name.equal exposed_name lib_name
          then `Included None
          else `Included (Some exposed_name)))
;;

let is_package_visible stanzas ~pkg_name =
  match stanzas with
  | [] -> true
  | _ ->
    List.exists stanzas ~f:(fun vs ->
      match package vs with
      | Some p -> Package_name.equal p pkg_name
      | None -> not (Libraries_ast.has_aliasing vs.libraries))
;;
