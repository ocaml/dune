open Import

module File = struct
  type t =
    { path : Path.t
    ; original_path : Path.t
        (* while path can be changed for a module (when it is being pp'ed), the
           original_path stays the same and points to an original source file *)
    ; dialect : Dialect.t
    }

  let decode ~dir =
    let open Dune_lang.Decoder in
    fields
    @@ let+ path = field "path" (Dune_lang.Path.Local.decode ~dir) in
       (* TODO do not just assume the dialect is OCaml *)
       { path; original_path = path; dialect = Dialect.ocaml }
  ;;

  let encode { path; original_path = _; dialect = _ } ~dir =
    let open Dune_lang.Encoder in
    record_fields [ field "path" (Dune_lang.Path.Local.encode ~dir) path ]
  ;;

  let dialect t = t.dialect
  let path t = t.path
  let original_path t = t.original_path

  let version_installed t ~src_root ~install_dir =
    let path =
      Path.descendant ~of_:src_root t.path
      |> Option.value_exn
      |> Path.as_in_source_tree_exn
      |> Path.append_source install_dir
    in
    { t with path }
  ;;

  let make dialect path = { dialect; path; original_path = path }

  let to_dyn { path; original_path; dialect } =
    let open Dyn in
    record
      [ "path", Path.to_dyn path
      ; "original_path", Path.to_dyn original_path
      ; "dialect", Dyn.string @@ Dialect.name dialect
      ]
  ;;
end

module Kind = struct
  type t =
    | Intf_only
    | Virtual
    | Impl
    | Alias of Module_name.Path.t
    | Impl_vmodule
    | Wrapped_compat
    | Root

  let to_dyn =
    let open Dyn in
    function
    | Intf_only -> variant "Intf_only" []
    | Virtual -> variant "Virtual" []
    | Impl -> variant "Impl" []
    | Alias path -> variant "Alias" [ Module_name.Path.to_dyn path ]
    | Impl_vmodule -> variant "Impl_vmodule" []
    | Wrapped_compat -> variant "Wrapped_compat" []
    | Root -> variant "Root" []
  ;;

  let encode =
    let open Dune_lang.Encoder in
    function
    | Intf_only -> string "intf_only"
    | Virtual -> string "virtual"
    | Impl -> string "impl"
    | Alias path ->
      (match path with
       | [] -> string "alias"
       | _ :: _ -> constr "alias" (fun x -> List (Module_name.Path.encode x)) path)
    | Impl_vmodule -> string "impl_vmodule"
    | Wrapped_compat -> string "wrapped_compat"
    | Root -> string "root"
  ;;

  let decode =
    let open Dune_lang.Decoder in
    sum
      [ "intf_only", return Intf_only
      ; "virtual", return Virtual
      ; "impl", return Impl
      ; "impl_vmodule", return Impl_vmodule
      ; "wrapped_compat", return Wrapped_compat
      ; "root", return Root
      ; ( "alias"
        , let* next = peek in
          (* TODO remove this once everyone recompiles *)
          match next with
          | None -> return (Alias [])
          | Some _ ->
            let+ path = enter Module_name.Path.decode in
            Alias path )
      ]
  ;;

  let has_impl = function
    | Alias _ | Impl_vmodule | Wrapped_compat | Root | Impl -> true
    | Intf_only | Virtual -> false
  ;;
end

(* Only the source of a module, not yet associated to a library *)
module Source = struct
  type t =
    { path : Module_name.Path.t
    ; files : File.t option Ml_kind.Dict.t
    }

  let decode ~dir =
    let open Dune_lang.Decoder in
    fields
    @@ let+ path = field "path" Module_name.Path.decode
       and+ intf = field_o "intf" (File.decode ~dir)
       and+ impl = field_o "impl" (File.decode ~dir) in
       { path; files = { Ml_kind.Dict.intf; impl } }
  ;;

  let encode ~dir { path; files = { Ml_kind.Dict.intf; impl } } =
    let open Dune_lang.Encoder in
    let file = function
      | None -> []
      | Some x -> File.encode ~dir x
    in
    record_fields
      [ field_l "path" Fun.id (Module_name.Path.encode path)
      ; field_l "intf" Fun.id (file intf)
      ; field_l "impl" Fun.id (file impl)
      ]
  ;;

  let to_dyn { path; files } =
    let open Dyn in
    record
      [ "path", Module_name.Path.to_dyn path
      ; "files", Ml_kind.Dict.to_dyn (option File.to_dyn) files
      ]
  ;;

  let make ?impl ?intf path =
    if path = [] then Code_error.raise "path cannot be empty" [];
    (match impl, intf with
     | None, None ->
       Code_error.raise
         "Module.Source.make called with no files"
         [ "path", Module_name.Path.to_dyn path ]
     | Some _, _ | _, Some _ -> ());
    let files = Ml_kind.Dict.make ~impl ~intf in
    { path; files }
  ;;

  let has t ~ml_kind = Ml_kind.Dict.get t.files ml_kind |> Option.is_some
  let name t = List.last t.path |> Option.value_exn
  let path t = t.path

  let choose_file { files = { impl; intf }; path = _ } =
    match intf, impl with
    | None, None -> assert false
    | Some x, Some _ | Some x, None | None, Some x -> x
  ;;

  let add_file t ml_kind file =
    if has t ~ml_kind
    then
      Code_error.raise
        "Attempted to add a duplicate file to module"
        [ "module", to_dyn t; "file", File.to_dyn file ];
    match ml_kind with
    | Ml_kind.Impl -> { t with files = { t.files with impl = Some file } }
    | Intf -> { t with files = { t.files with intf = Some file } }
  ;;

  let set_source t ml_kind file =
    match ml_kind with
    | Ml_kind.Impl -> { t with files = { t.files with impl = file } }
    | Intf -> { t with files = { t.files with intf = file } }
  ;;

  let src_dir t = Path.parent_exn (choose_file t).path

  let files t =
    let { Ml_kind.Dict.impl; intf } = t.files in
    let base = Option.to_list impl in
    match intf with
    | None -> base
    | Some i -> i :: base
  ;;

  let map_files t ~f =
    let files = Ml_kind.Dict.mapi ~f t.files in
    { t with files }
  ;;
end

type t =
  { source : Source.t
  ; obj_name : Module_name.Unique.t
  ; pp : (string list Action_builder.t * Sandbox_config.t) option
  ; visibility : Visibility.t
  ; kind : Kind.t
  ; install_as : Path.Local.t option
  }

let name t = Source.name t.source
let path t = t.source.path
let kind t = t.kind
let pp_flags t = t.pp
let install_as t = t.install_as

let of_source ?install_as ~obj_name ~visibility ~(kind : Kind.t) (source : Source.t) =
  (match kind, visibility with
   | (Alias _ | Impl_vmodule | Virtual | Wrapped_compat), Visibility.Public
   | Root, Private
   | (Impl | Intf_only), _ -> ()
   | _, _ ->
     Code_error.raise
       "Module.of_source: invalid kind, visibility combination"
       [ "path", Module_name.Path.to_dyn source.path
       ; "kind", Kind.to_dyn kind
       ; "visibility", Visibility.to_dyn visibility
       ]);
  (match kind, source.files.impl, source.files.intf with
   | (Alias _ | Impl_vmodule | Impl | Wrapped_compat), None, _
   | (Alias _ | Impl_vmodule | Wrapped_compat), Some _, Some _
   | (Intf_only | Virtual), Some _, _
   | (Intf_only | Virtual), _, None ->
     let open Dyn in
     Code_error.raise
       "Module.make: invalid kind, impl, intf combination"
       [ "path", Module_name.Path.to_dyn source.path
       ; "kind", Kind.to_dyn kind
       ; "intf", (option File.to_dyn) source.files.intf
       ; "impl", (option File.to_dyn) source.files.impl
       ]
   | _, _, _ -> ());
  let obj_name =
    match obj_name with
    | Some s -> s
    | None ->
      let file = Source.choose_file source in
      (* CR-someday aalekseyev: seems fragile to assume no mangling without any
         indication by the caller. *)
      Module_name.Unique.of_path_assuming_needs_no_mangling_allow_invalid file.path
  in
  { install_as; source; obj_name; pp = None; visibility; kind }
;;

let has t ~ml_kind =
  match (ml_kind : Ml_kind.t) with
  | Impl -> Kind.has_impl t.kind
  | Intf -> Option.is_some t.source.files.intf
;;

let source t ~(ml_kind : Ml_kind.t) = Ml_kind.Dict.get t.source.files ml_kind
let file t ~(ml_kind : Ml_kind.t) = source t ~ml_kind |> Option.map ~f:File.path
let obj_name t = t.obj_name

let iter t ~f =
  Memo.parallel_iter Ml_kind.all ~f:(fun kind ->
    Memo.Option.iter (Ml_kind.Dict.get t.source.files kind) ~f:(f kind))
;;

let set_obj_name t obj_name = { t with obj_name }

let set_path t path =
  let source = { t.source with Source.path } in
  { t with source }
;;

let add_file t kind file =
  let source = Source.add_file t.source kind file in
  { t with source }
;;

let set_source t kind file =
  let source = Source.set_source t.source kind file in
  { t with source }
;;

let map_files t ~f =
  let source = Source.map_files t.source ~f:(fun kind -> Option.map ~f:(f kind)) in
  { t with source }
;;

let src_dir t = Source.src_dir t.source
let set_pp t pp = { t with pp }

let to_dyn { source; obj_name; pp; visibility; kind; install_as } =
  Dyn.record
    [ "source", Source.to_dyn source
    ; "obj_name", Module_name.Unique.to_dyn obj_name
    ; "pp", Dyn.(option string) (Option.map ~f:(fun _ -> "has pp") pp)
    ; "visibility", Visibility.to_dyn visibility
    ; "kind", Kind.to_dyn kind
    ; "install_as", Dyn.option Path.Local.to_dyn install_as
    ]
;;

let ml_gen = ".ml-gen"

let wrapped_compat t =
  assert (t.visibility = Public);
  let source =
    let impl =
      let path =
        (* TODO(andreypopp): is this comment still relevant? *)
        (* Option.value_exn cannot fail because we disallow wrapped
           compatibility mode for virtual libraries. That means none of the
           modules are implementing a virtual module, and therefore all have
           a source dir *)
        Path.L.relative
          (src_dir t)
          [ ".wrapped_compat"; Module_name.Path.to_string t.source.path ^ ml_gen ]
      in
      Some { File.dialect = Dialect.ocaml; path; original_path = path }
    in
    { t.source with files = { intf = None; impl } }
  in
  { t with source; kind = Wrapped_compat }
;;

let visibility t = t.visibility

let sources t =
  List.filter_map
    [ t.source.files.intf; t.source.files.impl ]
    ~f:(Option.map ~f:(fun (x : File.t) -> x.path))
;;

let sources_without_pp t =
  List.filter_map
    [ t.source.files.intf; t.source.files.impl ]
    ~f:(Option.map ~f:(fun (x : File.t) -> x.original_path))
;;

module Obj_map = struct
  include Map.Make (struct
      type nonrec t = t

      let compare m1 m2 = Module_name.Unique.compare m1.obj_name m2.obj_name
      let to_dyn = to_dyn
    end)
end

let encode ({ source; obj_name; pp = _; visibility; kind; install_as = _ } as t) ~src_dir =
  let open Dune_lang.Encoder in
  let has_impl = has t ~ml_kind:Impl in
  let kind =
    match kind with
    | Kind.Impl when has_impl -> None
    | Intf_only when not has_impl -> None
    | Root | Wrapped_compat | Impl_vmodule | Alias _ | Impl | Virtual | Intf_only ->
      Some kind
  in
  record_fields
    [ field "obj_name" Module_name.Unique.encode obj_name
    ; field "visibility" Visibility.encode visibility
    ; field_o "kind" Kind.encode kind
    ; field_l "source" Fun.id (Source.encode ~dir:src_dir source)
    ]
;;

let decode ~src_dir =
  let open Dune_lang.Decoder in
  fields
    (let+ obj_name = field "obj_name" Module_name.Unique.decode
     and+ visibility = field "visibility" Visibility.decode
     and+ kind = field_o "kind" Kind.decode
     and+ source = field "source" (Source.decode ~dir:src_dir) in
     let kind =
       match kind with
       | Some k -> k
       | None when Option.is_some source.files.impl -> Impl
       | None -> Intf_only
     in
     { install_as = None; source; obj_name; pp = None; kind; visibility })
;;

let pped =
  map_files ~f:(fun _kind (file : File.t) ->
    (* We need to insert the suffix before the extension as some tools inspect
       the extension *)
    let pp_path = Path.map_extension file.path ~f:(fun ext -> ".pp" ^ ext) in
    { file with path = pp_path })
;;

let ml_source =
  map_files ~f:(fun ml_kind f ->
    match Dialect.ml_suffix f.dialect ml_kind with
    | None -> f
    | Some suffix ->
      let path = Path.extend_basename f.path ~suffix in
      { f with dialect = Dialect.ocaml; path })
;;

let version_installed t ~src_root ~install_dir =
  map_files t ~f:(fun _ -> File.version_installed ~src_root ~install_dir)
;;

let generated ?install_as ?obj_name ~(kind : Kind.t) ~src_dir (path : Module_name.Path.t) =
  let obj_name =
    match obj_name with
    | Some obj_name -> obj_name
    | None -> Module_name.Path.wrap path
  in
  let source =
    let impl =
      let basename = Module_name.Unique.artifact_filename obj_name ~ext:ml_gen in
      Path.Build.relative src_dir basename |> Path.build |> File.make Dialect.ocaml
    in
    Source.make ~impl path
  in
  let visibility : Visibility.t =
    match kind with
    | Root -> Private
    | _ -> Public
  in
  of_source ?install_as ~visibility ~kind ~obj_name:(Some obj_name) source
;;

let of_source ~visibility ~kind source = of_source ~obj_name:None ~visibility ~kind source

module Name_map = struct
  type nonrec t = t Module_name.Map.t

  let to_dyn = Module_name.Map.to_dyn to_dyn

  let decode ~src_dir =
    let open Dune_lang.Decoder in
    let+ modules = repeat (enter (decode ~src_dir)) in
    Module_name.Map.of_list_map_exn ~f:(fun m -> name m, m) modules
  ;;

  let encode t ~src_dir =
    Module_name.Map.to_list_map t ~f:(fun _ x -> Dune_lang.List (encode ~src_dir x))
  ;;

  let of_list_exn modules =
    List.rev_map modules ~f:(fun m -> name m, m) |> Module_name.Map.of_list_exn
  ;;

  let add t module_ = Module_name.Map.set t (name module_) module_
end
