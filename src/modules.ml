open Stdune

let real_unit_map_of_name_map map =
  Module.Name.Map.fold map ~init:Module.Name.Map.empty
    ~f:(fun m acc ->
      Module.Name.Map.add acc
        (Module.real_unit_name m) m)

module Wrapped = struct
  module Mode = Wrapped
  type t =
    { modules : Module.Name_map.t
    ; wrapped_compat : Module.Name_map.t
    ; mode : Mode.t
    ; alias_module : Module.t
    ; lib_interface : Module.t option
    }

  let real_unit_names
        { modules; wrapped_compat; alias_module
        ; lib_interface = _ ; mode = _
        } =
    let m = real_unit_map_of_name_map modules in
    let m =
      Module.Name.Map.add m (Module.real_unit_name alias_module) alias_module
    in
    Module.Name.Map.union m
      (real_unit_map_of_name_map wrapped_compat)
      ~f:(fun _name _ _ -> assert false)

  let main_module t =
    Option.value t.lib_interface ~default:t.alias_module

  let make_alias_module ~src_dir ~implements ~lib_name
        ~main_module_name ~modules =
    if implements then
      let name =
        Module.Name.add_suffix main_module_name
          (sprintf  "__%s__" (Lib_name.Local.to_string lib_name))
      in
      Module.generated_alias ~src_dir name
    else if Module.Name.Map.mem modules main_module_name then
      (* This module needs an implementation for non-dune
         users of the library:

         https://github.com/ocaml/dune/issues/567 *)
      let name = Module.Name.add_suffix main_module_name "__" in
      Module.generated_alias ~src_dir name
    else
      Module.generated_alias ~src_dir main_module_name

  let wrap_modules ~modules ~lib ~main_module_name =
    let prefix =
      if not (Dune_file.Library.is_impl lib) then
        fun _ -> main_module_name
      else
        (* for implementations we need to pick a different prefix for private
           modules. This is to guarantee that the private modules will never
           collide with the names of modules in the virtual library. *)
        let private_module_prefix =
          if Dune_file.Library.is_impl lib then
            Module.Name.of_string
              (sprintf "%s__%s"
                 (Module.Name.to_string main_module_name)
                 (Lib_name.Local.to_string (snd lib.name)))
          else
            main_module_name
        in
        fun m ->
          match Module.visibility m with
          | Private -> private_module_prefix
          | Public -> main_module_name
    in
    let open Module.Name.Infix in
    Module.Name.Map.map modules ~f:(fun (m : Module.t) ->
      if Module.name m = main_module_name ||
         Dune_file.Library.special_compiler_module lib m then
        m
      else
        Module.with_wrapper m ~main_module_name:(prefix m))

  let make ~(lib : Dune_file.Library.t) ~src_dir ~mode ~modules ~main_module_name =
    let wrapped_compat =
      match (mode : Mode.t) with
      | Simple false -> assert false
      | Simple true -> Module.Name.Map.empty
      | Yes_with_transition _ ->
        Module.Name.Map.remove modules main_module_name
        |> Module.Name.Map.filter_map ~f:(fun m ->
          match Module.visibility m with
          | Public -> Some (Module.wrapped_compat m)
          | Private -> None)
    in
    let alias_module =
      let (_, lib_name) = lib.name in
      let implements = Dune_file.Library.is_impl lib in
      make_alias_module ~main_module_name ~src_dir ~lib_name ~modules
        ~implements

    in
    let modules = wrap_modules ~modules ~main_module_name ~lib in
    { modules
    ; alias_module
    ; mode
    ; wrapped_compat
    ; lib_interface = Module.Name.Map.find modules main_module_name
    }

  let installable_modules
        { modules ; alias_module; wrapped_compat
        ; lib_interface = _; mode = _ } =
    alias_module
    :: Module.Name.Map.values modules
    @ Module.Name.Map.values wrapped_compat

  let mode t = t.mode

  let decode ~src_dir =
    let open Stanza.Decoder in
    fields (
      let+ modules =
        field ~default:[] "modules"
          (list (enter (Module.decode ~src_dir)))
      and+ alias_module = field "alias_module" (Module.decode ~src_dir)
      and+ main_module_name = field "main_module_name" Module.Name.decode
      in
      let modules = Module.Name_map.of_list_exn modules in
      let lib_interface = Module.Name.Map.find modules main_module_name in
      let wrapped_compat = Module.Name.Map.empty in
      let mode = Mode.Simple true in
      { modules
      ; alias_module
      ; mode
      ; wrapped_compat
      ; lib_interface
      }
    )

  let encode w =
    let main_module_name = Module.name (main_module w) in
    let open Dune_lang.Encoder in
    record_fields
      [ field_l "alias_module" sexp (Module.encode w.alias_module)
      ; field "main_module_name" Module.Name.encode main_module_name
      ; field_l "modules" sexp (Module.Name_map.encode w.modules)
      ]

  let map ({ modules
           ; alias_module
           ; lib_interface
           ; wrapped_compat
           ; mode = _} as t) ~f =
    let wrapped_compat = Module.Name.Map.map wrapped_compat ~f in
    let alias_module = f alias_module in
    let modules = Module.Name.Map.map modules ~f in
    let lib_interface =
      (* we don't want to run [f] more than once per module *)
      Option.map lib_interface ~f:(fun interface ->
        let name = Module.name interface in
        Module.Name.Map.find_exn modules name)
    in
    { t with
      modules
    ; lib_interface
    ; alias_module
    ; wrapped_compat
    }
end

type t =
  | Singleton of Module.t
  | Unwrapped of Module.Name_map.t
  | Wrapped of Wrapped.t
  | Impl of impl

and impl =
  { impl : t
  ; vlib : t
  }

let rec lib_interface = function
  | Singleton m -> Some m
  | Unwrapped _ -> None
  | Wrapped w -> w.lib_interface
  | Impl { impl = _; vlib } -> lib_interface vlib

let rec alias_module = function
  | Singleton _
  | Unwrapped _ -> None
  | Wrapped w -> Some w.alias_module
  | Impl { impl ; vlib = _ } -> alias_module impl

let exe m = Unwrapped m

let as_singleton m =
  if Module.Name.Map.cardinal m <> 1 then
    None
  else
    Module.Name.Map.choose m
    |> Option.map ~f:snd

let lib ~lib ~src_dir ~user_written_modules:modules
      ~main_module_name ~(mode : Wrapped.Mode.t) =
  let wrapped main_module_name =
    Wrapped (Wrapped.make ~mode ~modules ~src_dir ~main_module_name ~lib)
  in
  match mode, main_module_name, as_singleton modules with
  | Simple false, _, Some m -> Singleton m
  | Simple false, _, None -> Unwrapped modules
  | (Yes_with_transition _ | Simple true), Some main_module_name, Some m ->
    if Module.name m = main_module_name then
      Singleton m
    else
      wrapped main_module_name
  | (Yes_with_transition _ | Simple true), Some main_module_name, None ->
    wrapped main_module_name
  | (Simple true | Yes_with_transition _), None, _ ->
    Code_error.raise "Modules.lib: cannot wrap without main module name"
      []

let impl t ~vlib = Impl { impl = t; vlib } (* TODO validation *)

type from = Vlib | Impl_or_lib

let from_impl_or_lib = Option.map ~f:(fun m -> (Impl_or_lib, m))

let rec find_from t name =
  match t with
  | Singleton m ->
    Option.some_if (Module.name m = name) m
    |> from_impl_or_lib
  | Unwrapped m -> from_impl_or_lib (Module.Name.Map.find m name)
  | Wrapped { modules ; wrapped_compat
            ; alias_module; lib_interface = _ ; mode = _} ->
    from_impl_or_lib (
      if Module.name alias_module = name then
        Some alias_module
      else
        begin match Module.Name.Map.find modules name with
        | Some _ as m -> m
        | None -> Module.Name.Map.find wrapped_compat name
        end
    )
  | Impl { impl; vlib } ->
    begin match find_from impl name with
    | Some _ as m -> m
    | None ->
      let open Option.O in
      let+ (_, m) = find_from vlib name in
      (Vlib, m)
    end

let find t name = Option.map (find_from t name) ~f:snd

let find_dep t name =
  let open Option.O in
  let* (from, m) = find_from t name in
  let kind = Module.kind m in
  let* m = Option.some_if (kind <> Wrapped_compat) m in
  match from with
  | Impl_or_lib -> Some m
  | Vlib -> Option.some_if (Module.visibility m = Public) m

let singleton m = Singleton m

let rec impl_only = function
  | Singleton m ->
    if Module.has m ~ml_kind:Impl then
      [m]
    else
      []
  | Unwrapped m -> Module.Name_map.impl_only m
  | Wrapped { modules ; wrapped_compat ; alias_module ; lib_interface = _
            ; mode = _ } ->
    alias_module
    :: Module.Name_map.impl_only modules
    @ Module.Name.Map.values wrapped_compat
  | Impl { vlib ; impl } ->
    impl_only impl @ impl_only vlib

let rec fold t ~init ~f =
  match t with
  | Singleton m -> f m init
  | Unwrapped m -> Module.Name.Map.fold m ~f ~init
  | Wrapped { modules ; wrapped_compat ; alias_module ; lib_interface = _
            ; mode = _ } ->
    let init = f alias_module init in
    let init = Module.Name.Map.fold modules ~f ~init in
    Module.Name.Map.fold wrapped_compat ~f ~init
  | Impl { vlib; impl } ->
    let init = fold impl ~f ~init in
    fold vlib ~f ~init

let fold_no_vlib t ~init ~f =
  match t with
  | Impl { vlib = _ ; impl } -> fold impl ~init ~f
  | t -> fold t ~init ~f

let compat_for_exn t m =
  match t with
  | Singleton _
  | Unwrapped _ -> assert false
  | Wrapped { modules ; _ } ->
    Module.Name.Map.find modules (Module.name m)
    |> Option.value_exn
  | Impl _ ->
    Code_error.raise "wrapped compat not supported for vlib"
      []

let iter t ~f = fold t ~init:() ~f:(fun x () -> f x)

let rec for_alias_exn = function
  | Singleton _
  | Unwrapped _ -> assert false
  | Wrapped { modules ; lib_interface
            ; alias_module = _; wrapped_compat = _; mode = _ } ->
    begin match lib_interface with
    | None -> modules
    | Some lib_interface ->
      let name = Module.name lib_interface in
      Module.Name.Map.remove modules name
    end
  | Impl { vlib ; impl } ->
    let impl = for_alias_exn impl in
    let vlib = for_alias_exn vlib in
    Module.Name.Map.merge impl vlib ~f:(fun _ impl vlib ->
      match impl, vlib with
      | None, None -> assert false
      | Some _, _ -> impl
      | _, Some vlib ->
        Option.some_if (Module.visibility vlib = Public) vlib)

let wrapped_compat = function
  | Singleton _
  | Impl _
  | Unwrapped _ -> Module.Name.Map.empty
  | Wrapped w -> w.wrapped_compat

let rec fold_user_written t ~f ~init =
  match t with
  | Singleton m -> f m init
  | Wrapped { modules ; _ }
  | Unwrapped modules -> Module.Name.Map.fold modules ~init ~f
  | Impl { impl ; vlib = _ } -> fold_user_written impl ~f ~init

let rec map t ~f =
  match t with
  | Singleton m -> Singleton (f m)
  | Unwrapped m -> Unwrapped (Module.Name.Map.map m ~f)
  | Wrapped w -> Wrapped (Wrapped.map w ~f)
  | Impl t -> Impl { t with impl = map t.impl ~f }

let rec map_user_written t ~f =
  match t with
  | Singleton m -> Singleton (f m)
  | Unwrapped m -> Unwrapped (Module.Name.Map.map m ~f)
  | Wrapped ({ modules
             ; alias_module = _
             ; lib_interface
             ; wrapped_compat = _
             ; mode = _
             } as w) ->
    let modules = Module.Name.Map.map modules ~f in
    let lib_interface =
      (* we don't want to run [f] more than once per module *)
      Option.map lib_interface ~f:(fun interface ->
        let name = Module.name interface in
        Module.Name.Map.find_exn modules name)
    in
    Wrapped
      { w with
        modules
      ; lib_interface
      }
  | Impl t ->
    Impl { t with impl = map_user_written t.impl ~f }

let rec for_odoc = function
  | Singleton m -> [m]
  | Unwrapped m -> Module.Name.Map.values m
  | Wrapped { modules
            ; alias_module
            ; lib_interface = _
            ; wrapped_compat = _
            ; mode = _ } ->
    alias_module :: Module.Name.Map.values modules
  | Impl { vlib ; _ } ->
    (* TODO wrong but odoc doesn't support this yet anwyway *)
    for_odoc vlib

let rec installable_modules = function
  | Wrapped m -> Wrapped.installable_modules m
  | Singleton m -> [m]
  | Unwrapped m -> Module.Name.Map.values m
  | Impl { impl ; vlib = _ } -> installable_modules impl

let virtual_module_names =
  fold_user_written ~init:Module.Name.Set.empty ~f:(fun m acc ->
    match Module.kind m with
    | Virtual -> Module.Name.Set.add acc (Module.name m)
    | _ -> acc)

let version_installed t ~install_dir:src_dir =
  map t ~f:(Module.set_src_dir ~src_dir)

let rec wrapped = function
  | Singleton _
  | Unwrapped _ -> Wrapped.Mode.Simple false
  | Wrapped w -> Wrapped.mode w
  | Impl { vlib ; _ } -> wrapped vlib

let rec main_module = function
  | Singleton m -> Some m
  | Unwrapped _ -> None
  | Wrapped w -> Some (Wrapped.main_module w)
  | Impl { vlib ; impl = _ } -> main_module vlib

let main_module_name_exn t =
  match main_module t with
  | None -> assert false
  | Some m -> Module.name m

let entry_modules = function
  | Singleton m -> [m]
  | Unwrapped m ->
    Module.Name.Map.values m
    |> List.filter ~f:(fun m -> Module.visibility m = Public)
  | Wrapped w -> [Wrapped.main_module w]
  | Impl _ -> assert false

let rec encode = function
  | Singleton m ->
    Dune_lang.atom "singleton" :: Module.encode m
  | Unwrapped m ->
    Dune_lang.atom "unwrapped"
    :: (Module.Name.Map.values m
        |> List.map ~f:(fun m ->
          Dune_lang.List (Module.encode m)))
  | Wrapped w ->
    Dune_lang.atom "wrapped"
    :: Wrapped.encode w
  | Impl { impl ; vlib = _ } -> encode impl

let decode ~src_dir =
  let open Stanza.Decoder in
  sum
    [ "singleton", (
        let+ m = Module.decode ~src_dir in
        Singleton m
      )
    ; "unwrapped", (
        let+ modules = Module.Name_map.decode ~src_dir in
        Unwrapped modules
      )
    ; "wrapped", (
        let+ w = Wrapped.decode ~src_dir in
        Wrapped w
      )
    ]

let real_unit_names = function
  | Singleton m ->
    Module.Name.Map.singleton (Module.real_unit_name m) m
  | Unwrapped m -> real_unit_map_of_name_map m
  | Wrapped m -> Wrapped.real_unit_names m
  | Impl _ ->
    Code_error.raise
      "real_unit_names: not well defined for implementations"
      []
