open Import
module Mode = Wrapped

module Common = struct
  module Encode = struct
    open Dune_lang.Encoder

    let main_module_name = field "main_module_name" Module_name.encode

    let modules ?(name = "modules") modules =
      field_l name (fun x -> x) (Module.Name_map.encode modules)
  end

  module Decode = struct
    open Dune_lang.Decoder

    let main_module_name = field "main_module_name" Module_name.decode

    let modules ?(name = "modules") ~src_dir () =
      field ~default:Module_name.Map.empty name
        (Module.Name_map.decode ~src_dir)
  end
end

module Stdlib = struct
  type t =
    { modules : Module.Name_map.t
    ; unwrapped : Module_name.Set.t
    ; exit_module : Module_name.t option
    ; main_module_name : Module_name.t
    }

  let encode { modules; unwrapped; exit_module; main_module_name } =
    let open Dune_lang.Encoder in
    let module E = Common.Encode in
    record_fields
      [ E.main_module_name main_module_name
      ; E.modules modules
      ; field_o "exit_module" Module_name.encode exit_module
      ; field_l "unwrapped" Module_name.encode
          (Module_name.Set.to_list unwrapped)
      ]

  let decode ~src_dir =
    let open Dune_lang.Decoder in
    let open Common.Decode in
    fields
      (let+ main_module_name = main_module_name
       and+ modules = modules ~src_dir ()
       and+ exit_module = field_o "exit_module" Module_name.decode
       and+ unwrapped =
         field ~default:[] "unwrapped" (repeat (enter Module_name.decode))
       in
       let unwrapped = Module_name.Set.of_list unwrapped in
       { modules; main_module_name; exit_module; unwrapped })

  let to_dyn { modules; unwrapped; exit_module; main_module_name } =
    let open Dyn in
    record
      [ ("modules", Module.Name_map.to_dyn modules)
      ; ("unwrapped", Module_name.Set.to_dyn unwrapped)
      ; ("exit_module", option Module_name.to_dyn exit_module)
      ; ("main_module_name", Module_name.to_dyn main_module_name)
      ]

  let exit_module t =
    let open Option.O in
    let* name = t.exit_module in
    Module_name.Map.find t.modules name

  let exists t ~f = Module_name.Map.exists t.modules ~f

  let fold t ~init ~f = Module_name.Map.fold t.modules ~f ~init

  let map t ~f = { t with modules = Module_name.Map.map t.modules ~f }

  let traverse t ~f =
    let open Memo.O in
    let+ modules =
      Module_name.Map_traversals.parallel_map t.modules ~f:(fun _ -> f)
    in
    { t with modules }

  let lib_interface t = Module_name.Map.find t.modules t.main_module_name

  (* Returns [true] is a special module, i.e. one whose compilation unit name is
     hard-coded inside the compiler. It is not possible to change the
     compilation unit name of such modules, so they cannot be wrapped. *)
  let special_compiler_module (stdlib : Ocaml_stdlib.t) m =
    let name = Module.name m in
    Glob.test stdlib.internal_modules (Module_name.to_string name)
    ||
    match stdlib.exit_module with
    | None -> false
    | Some n -> n = name

  let make ~(stdlib : Ocaml_stdlib.t) ~modules ~main_module_name =
    let modules =
      Module_name.Map.map modules ~f:(fun m ->
          if
            Module.name m = main_module_name || special_compiler_module stdlib m
          then m
          else Module.with_wrapper m ~main_module_name ~path:[])
    in
    let unwrapped = stdlib.modules_before_stdlib in
    let exit_module = stdlib.exit_module in
    { modules; unwrapped; exit_module; main_module_name }

  let obj_map t ~f =
    Module_name.Map.fold t.modules ~init:Module.Obj_map.empty ~f:(fun m acc ->
        Module.Obj_map.add_exn acc m (f m))

  let impl_only t =
    Module_name.Map.values t.modules
    |> List.filter ~f:(fun m ->
           (* TODO what about checking for implementations? *)
           Some (Module.name m) <> t.exit_module)

  let find t = Module_name.Map.find t.modules

  let find_dep t ~of_ name =
    let of_name = Module.name of_ in
    if of_name = t.main_module_name then
      if Module_name.Set.mem t.unwrapped name then
        Module_name.Map.find t.modules name
      else None
    else Module_name.Map.find t.modules name

  let alias_for t m =
    let name = Module.name m in
    if name = t.main_module_name || Module_name.Set.mem t.unwrapped name then
      None
    else lib_interface t
end

module Mangle = struct
  module Lib = struct
    type kind =
      | Has_lib_interface
      | Implementation of Lib_name.Local.t
      | Neither

    type t =
      { main_module_name : Module_name.t
      ; kind : kind
      }
  end

  type t =
    | Lib of Lib.t
    | Exe
    | Melange

  let of_lib ~lib_name ~implements ~main_module_name ~modules =
    let kind : Lib.kind =
      if implements then Implementation lib_name
      else if Module_trie.mem modules [ main_module_name ] then
        Has_lib_interface
      else Neither
    in
    Lib { main_module_name; kind }

  let prefix t : Module_name.t Visibility.Map.t =
    match t with
    | Lib { main_module_name; kind } -> (
      match kind with
      | Has_lib_interface | Neither -> Visibility.Map.make_both main_module_name
      | Implementation lib ->
        { private_ =
            sprintf "%s__%s__"
              (Module_name.to_string main_module_name)
              (Lib_name.Local.to_string lib)
            |> Module_name.of_string
        ; public = main_module_name
        })
    | Exe ->
      sprintf "dune__exe" |> Module_name.of_string |> Visibility.Map.make_both
    | Melange ->
      sprintf "melange" |> Module_name.of_string |> Visibility.Map.make_both

  let make_alias_module t ~src_dir =
    let prefix = prefix t in
    let name =
      match t with
      | Lib { kind = Has_lib_interface; _ } ->
        Module_name.add_suffix prefix.public "__"
      | Lib { kind = Implementation _; _ } -> prefix.private_
      | _ -> prefix.public
    in
    Module.generated ~kind:Alias ~src_dir name

  let wrap_modules t modules =
    let prefix = prefix t in
    let f =
      (* TODO wrap all lib interfaces properly *)
      match t with
      | Exe | Melange ->
        fun path m -> Module.set_path m ~main_module_name:prefix.public ~path
      | Lib { main_module_name; kind = _ } ->
        fun path m ->
          if Module.name m = main_module_name then m
          else
            let visibility = Module.visibility m in
            let prefix = Visibility.Map.find prefix visibility in
            Module.set_path m ~main_module_name:prefix ~path
    in
    Module_trie.mapi modules ~f
end

let impl_only_of_map m =
  Module_name.Map.fold m ~init:[] ~f:(fun m acc ->
      if Module.has m ~ml_kind:Impl then m :: acc else acc)

module Wrapped = struct
  module Group = struct
    type t =
      { alias : Module.t
      ; modules : node Module_name.Map.t
      ; (* TODO rename *) path : Module_name.t
      }

    and node =
      | Group of t
      | Module of Module.t

    let of_trie (trie : Module.t Module_trie.t) ~mangle ~src_dir : t =
      let rec loop path trie =
        { alias = Mangle.make_alias_module mangle ~src_dir
        ; path
        ; modules =
            Module_name.Map.mapi trie ~f:(fun path (m : 'a Module_trie.node) ->
                match m with
                | Leaf m -> Module m
                | Map m -> Group (loop path m))
        }
      in
      loop (Mangle.prefix mangle).public trie

    let rec relocate_alias_module t ~src_dir =
      { t with
        alias = Module.set_src_dir t.alias ~src_dir
      ; modules =
          Module_name.Map.map t.modules ~f:(function
            | Module m -> Module m
            | Group g -> Group (relocate_alias_module g ~src_dir))
      }

    let rec fold { alias; modules; path = _ } ~f ~init =
      let init = f alias init in
      Module_name.Map.fold modules ~init ~f:(fun node init ->
          match node with
          | Module m -> f m init
          | Group t -> fold t ~f ~init)

    let rec exists { alias; modules; path = _ } ~f =
      f alias
      || Module_name.Map.exists modules ~f:(function
           | Module m -> f m
           | Group p -> exists p ~f)

    let rec to_dyn { alias; modules; path } =
      let open Dyn in
      record
        [ ("alias", Module.to_dyn alias)
        ; ("path", Module_name.to_dyn path)
        ; ( "modules"
          , Module_name.Map.to_dyn
              (function
                | Module m -> variant "module" [ Module.to_dyn m ]
                | Group g -> variant "group" [ to_dyn g ])
              modules )
        ]

    let rec map ({ alias; modules; path = _ } as t) ~f =
      let alias = f alias in
      let modules =
        Module_name.Map.map modules ~f:(function
          | Module m -> Module (f m)
          | Group g -> Group (map g ~f))
      in
      { t with alias; modules }

    let lib_interface t =
      match Module_name.Map.find t.modules t.path with
      | None | Some (Group _) -> t.alias
      | Some (Module m) -> m

    let decode ~src_dir:_ = assert false

    let rec encode { alias; modules; path } =
      let open Dune_lang.Encoder in
      record_fields
        [ field_l "alias" sexp (Module.encode alias)
        ; field "path" Module_name.encode path
        ; field_l "modules"
            (fun x -> x)
            (Module_name.Map.to_list_map modules ~f:(fun _ t ->
                 Dune_lang.List
                   (match t with
                   | Group g -> Dune_lang.atom "group" :: encode g
                   | Module m -> Dune_lang.atom "module" :: Module.encode m)))
        ]

    let parents t m =
      let rec loop acc t = function
        | [] -> acc
        | p :: ps -> (
          match Module_name.Map.find t.modules p with
          | None -> assert false
          | Some (Module _) ->
            assert (ps = []);
            acc
          | Some (Group g) -> loop (g :: acc) g ps)
      in
      loop [ t ] t (List.tl (Module.path m))

    module Memo_traversals = struct
      let rec parallel_map ({ alias; modules; path = _ } as t) ~f =
        let open Memo.O in
        let+ alias, modules =
          Memo.fork_and_join
            (fun () -> f alias)
            (fun () ->
              Module_name.Map_traversals.parallel_map modules ~f:(fun _ n ->
                  match n with
                  | Module m ->
                    let+ m = f m in
                    Module m
                  | Group g ->
                    let+ g = parallel_map g ~f in
                    Group g))
        in
        { t with alias; modules }
    end
  end

  type t =
    { group : Group.t
    ; wrapped_compat : Module.Name_map.t
    ; wrapped : Mode.t
    }

  let lib_interface t = Group.lib_interface t.group

  let for_alias t m =
    assert (Module.kind m = Alias);
    let neighbours = List.hd (Group.parents t.group m) in
    Module_name.Map.remove neighbours.modules neighbours.path
    |> Module_name.Map.map ~f:(fun (g : Group.node) ->
           match g with
           | Module m -> m
           | Group g -> Group.lib_interface g)

  let encode { group; wrapped_compat; wrapped } =
    let open Dune_lang.Encoder in
    let module E = Common.Encode in
    record_fields
      [ field_l "group" (fun x -> x) (Group.encode group)
      ; E.modules ~name:"wrapped_compat" wrapped_compat
      ; field "wrapped" Wrapped.encode wrapped
      ]

  let decode ~src_dir =
    let open Dune_lang.Decoder in
    let open Common.Decode in
    fields
      (let+ group = field "group" (Group.decode ~src_dir)
       and+ wrapped_compat = modules ~name:"wrapped_compat" ~src_dir ()
       and+ wrapped = field "wrapped" Mode.decode in
       { group; wrapped_compat; wrapped })

  let map ({ group; wrapped_compat; wrapped = _ } as t) ~f =
    { t with
      group = Group.map group ~f
    ; wrapped_compat = Module_name.Map.map wrapped_compat ~f
    }

  let make ~src_dir ~lib_name ~implements ~modules ~main_module_name ~wrapped =
    let mangle =
      Mangle.of_lib ~main_module_name ~lib_name ~implements ~modules
    in
    let modules, wrapped_compat =
      let wrapped_modules = Mangle.wrap_modules mangle modules in
      match (wrapped : Mode.t) with
      | Simple false -> assert false
      | Simple true -> (wrapped_modules, Module_name.Map.empty)
      | Yes_with_transition _ ->
        ( wrapped_modules
        , Module_trie.remove modules [ main_module_name ]
          |> Module_trie.toplevel_only
          |> Module_name.Map.filter_map ~f:(fun m ->
                 match Module.visibility m with
                 | Private -> None
                 | Public -> Some (Module.wrapped_compat m)) )
    in
    let group = Group.of_trie modules ~mangle ~src_dir in
    { group; wrapped_compat; wrapped }

  let make_exe_or_melange ~src_dir ~modules mangle =
    let modules = Mangle.wrap_modules mangle modules in
    let group = Group.of_trie modules ~mangle ~src_dir in
    { group; wrapped_compat = Module_name.Map.empty; wrapped = Simple true }

  let obj_map { group; wrapped_compat; wrapped = _ } ~f =
    let add_module m acc = Module.Obj_map.add_exn acc m (f m) in
    let init = Group.fold group ~init:Module.Obj_map.empty ~f:add_module in
    Module_name.Map.fold ~init wrapped_compat ~f:add_module

  let to_dyn { group; wrapped_compat; wrapped } =
    let open Dyn in
    record
      [ ("group", Group.to_dyn group)
      ; ("wrapped_compat", Module_name.Map.to_dyn Module.to_dyn wrapped_compat)
      ; ("wrapped", Mode.to_dyn wrapped)
      ]

  let impl_only { group; wrapped_compat; wrapped = _ } =
    let init = Module_name.Map.values wrapped_compat in
    Group.fold group ~init ~f:(fun v acc ->
        if Module.has v ~ml_kind:Impl then v :: acc else acc)

  let fold { group; wrapped_compat; wrapped = _ } ~init ~f =
    let init = Group.fold group ~f ~init in
    Module_name.Map.fold wrapped_compat ~f ~init

  let exists { group; wrapped_compat; wrapped = _ } ~f =
    Group.exists group ~f || Module_name.Map.exists wrapped_compat ~f

  let find t name =
    match Module_name.Map.find t.group.modules name with
    | Some (Module m) -> Some m
    | Some (Group _) | None -> None

  let find_dep t ~of_ name =
    match Module.kind of_ with
    | Alias -> None
    | Wrapped_compat ->
      let li = lib_interface t in
      Option.some_if (name = Module.name li) li
    | _ ->
      (* TODO don't recompute this  *)
      let parents =
        match Group.parents t.group of_ with
        | [] -> assert false
        | top :: rest as parents ->
          if
            Module_name.Unique.equal
              (Module.obj_name @@ Group.lib_interface top)
              (Module.obj_name of_)
          then rest
          else parents
      in
      List.find_map parents ~f:(fun parent ->
          match Module_name.Map.find parent.modules name with
          | None -> None
          | Some (Module m) -> Some m
          | Some (Group g) -> Some (Group.lib_interface g))

  let alias_for t m =
    match Module.kind m with
    | Alias | Wrapped_compat -> []
    | _ -> Group.parents t.group m |> List.map ~f:(fun (s : Group.t) -> s.alias)

  let relocate_alias_module t ~src_dir =
    let group = Group.relocate_alias_module t.group ~src_dir in
    { t with group }
end

type t =
  | Singleton of Module.t
  | Unwrapped of Module.Name_map.t
  | Wrapped of Wrapped.t
  | Impl of impl
  | Stdlib of Stdlib.t

and impl =
  { impl : t
  ; vlib : t
  }

let equal (x : t) (y : t) = Poly.equal x y

let rec encode t =
  let open Dune_lang in
  match t with
  | Singleton m -> List (atom "singleton" :: Module.encode m)
  | Unwrapped m -> List (atom "unwrapped" :: Module.Name_map.encode m)
  | Wrapped m -> List (atom "wrapped" :: Wrapped.encode m)
  | Stdlib m -> List (atom "stdlib" :: Stdlib.encode m)
  | Impl { impl; _ } -> encode impl

let singleton m = Singleton m

let decode ~src_dir =
  let open Dune_lang.Decoder in
  sum
    [ ( "singleton"
      , let+ m = Module.decode ~src_dir in
        Singleton m )
    ; ( "unwrapped"
      , let+ modules = Module.Name_map.decode ~src_dir in
        Unwrapped modules )
    ; ( "wrapped"
      , let+ w = Wrapped.decode ~src_dir in
        Wrapped w )
    ; ( "stdlib"
      , let+ stdlib = Stdlib.decode ~src_dir in
        Stdlib stdlib )
    ]

let rec to_dyn =
  let open Dyn in
  function
  | Singleton m -> variant "Singleton" [ Module.to_dyn m ]
  | Unwrapped m -> variant "Unwrapped" [ Module.Name_map.to_dyn m ]
  | Wrapped w -> variant "Wrapped" [ Wrapped.to_dyn w ]
  | Stdlib s -> variant "Stdlib" [ Stdlib.to_dyn s ]
  | Impl impl -> variant "Impl" [ dyn_of_impl impl ]

and dyn_of_impl { impl; vlib } =
  let open Dyn in
  record [ ("impl", to_dyn impl); ("vlib", to_dyn vlib) ]

let rec lib_interface = function
  | Singleton m -> Some m
  | Unwrapped _ -> None
  | Wrapped w -> Some (Wrapped.lib_interface w)
  | Stdlib w -> Stdlib.lib_interface w
  | Impl { impl = _; vlib } -> lib_interface vlib

let rec main_module_name = function
  | Singleton m -> Some (Module.name m)
  | Unwrapped _ -> None
  | Wrapped w -> Some w.group.path
  | Stdlib w -> Some w.main_module_name
  | Impl { vlib; impl = _ } -> main_module_name vlib

let lib ~src_dir ~main_module_name ~wrapped ~stdlib ~lib_name ~implements
    ~modules =
  let make_wrapped main_module_name =
    Wrapped
      (Wrapped.make ~src_dir ~lib_name ~implements ~modules ~main_module_name
         ~wrapped)
  in
  match stdlib with
  | Some stdlib ->
    let main_module_name = Option.value_exn main_module_name in
    let modules = Module_trie.to_map modules in
    Stdlib (Stdlib.make ~stdlib ~modules ~main_module_name)
  | None -> (
    match (wrapped, main_module_name, Module_trie.as_singleton modules) with
    | Simple false, _, Some m -> Singleton m
    | Simple false, _, None ->
      (* TODO allow unwrapped modules to use [(include_subdirs qualified)] *)
      Unwrapped (Module_trie.to_map modules)
    | (Yes_with_transition _ | Simple true), Some main_module_name, Some m ->
      if Module.name m = main_module_name && not implements then Singleton m
      else make_wrapped main_module_name
    | (Yes_with_transition _ | Simple true), Some main_module_name, None ->
      make_wrapped main_module_name
    | (Simple true | Yes_with_transition _), None, _ ->
      Code_error.raise "Modules.lib: cannot wrap without main module name" [])

let impl impl ~vlib =
  match (impl, vlib) with
  | _, Impl _ | Impl _, _ | Stdlib _, _ | _, Stdlib _ ->
    Code_error.raise "Modules.impl: invalid arguments"
      [ ("impl", to_dyn impl); ("vlib", to_dyn vlib) ]
  | _, _ -> Impl { impl; vlib }

let rec find t name =
  match t with
  | Singleton m -> Option.some_if (Module.name m = name) m
  | Unwrapped m -> Module_name.Map.find m name
  | Stdlib w -> Stdlib.find w name
  | Wrapped w -> Wrapped.find w name
  | Impl { impl; vlib } -> (
    match find impl name with
    | Some _ as m -> m
    | None -> find vlib name)

type from =
  | Vlib
  | Impl_or_lib

let from_impl_or_lib = Option.map ~f:(fun m -> (Impl_or_lib, m))

let rec find_dep t ~of_ name =
  if Module.name of_ = name then None
  else
    let open Option.O in
    let* from, m =
      match t with
      | Stdlib s -> from_impl_or_lib (Stdlib.find_dep s ~of_ name)
      | Wrapped w -> from_impl_or_lib (Wrapped.find_dep w ~of_ name)
      | Impl { vlib; impl } -> (
        match find_dep impl ~of_ name with
        | Some m -> Some (Impl_or_lib, m)
        | None ->
          let open Option.O in
          let+ m = find_dep vlib ~of_ name in
          (Vlib, m))
      | _ -> from_impl_or_lib (find t name)
    in
    match from with
    | Impl_or_lib -> Some m
    | Vlib -> Option.some_if (Module.visibility m = Public) m

let make_singleton m mangle =
  Singleton
    (let main_module_name = (Mangle.prefix mangle).public in
     Module.with_wrapper m ~main_module_name ~path:[])

let singleton_exe m = make_singleton m Exe

let exe_unwrapped m = Unwrapped m

let make_wrapped ~src_dir ~modules kind =
  let mangle : Mangle.t =
    match kind with
    | `Exe -> Exe
    | `Melange -> Melange
  in
  match Module_trie.as_singleton modules with
  | Some m -> make_singleton m mangle
  | None -> Wrapped (Wrapped.make_exe_or_melange ~src_dir ~modules mangle)

let rec impl_only = function
  | Stdlib w -> Stdlib.impl_only w
  | Singleton m -> if Module.has ~ml_kind:Impl m then [ m ] else []
  | Unwrapped m -> impl_only_of_map m
  | Wrapped w -> Wrapped.impl_only w
  | Impl { vlib; impl } -> impl_only impl @ impl_only vlib

let rec exists t ~f =
  match t with
  | Stdlib w -> Stdlib.exists w ~f
  | Wrapped m -> Wrapped.exists m ~f
  | Singleton m -> f m
  | Unwrapped m -> Module_name.Map.exists m ~f
  | Impl { vlib; impl } -> exists vlib ~f || exists impl ~f

let has_impl =
  let has = Module.has ~ml_kind:Impl in
  exists ~f:has

let rec fold_no_vlib t ~init ~f =
  match t with
  | Stdlib w -> Stdlib.fold w ~init ~f
  | Singleton m -> f m init
  | Unwrapped m -> Module_name.Map.fold m ~f ~init
  | Wrapped w -> Wrapped.fold w ~init ~f
  | Impl { vlib = _; impl } -> fold_no_vlib impl ~f ~init

let rec map t ~f =
  match t with
  | Stdlib w -> Stdlib (Stdlib.map w ~f)
  | Singleton m -> Singleton (f m)
  | Unwrapped m -> Unwrapped (Module_name.Map.map m ~f)
  | Wrapped w -> Wrapped (Wrapped.map w ~f)
  | Impl { vlib; impl } -> Impl { vlib = map vlib ~f; impl = map impl ~f }

type split_by_lib =
  { vlib : Module.t list
  ; impl : Module.t list
  }

let split_by_lib t =
  let f m acc = m :: acc in
  let init = [] in
  match t with
  | Impl { vlib; impl } ->
    let vlib = fold_no_vlib vlib ~init ~f in
    let impl = fold_no_vlib impl ~init ~f in
    { vlib; impl }
  | _ -> { impl = fold_no_vlib t ~init ~f; vlib = [] }

let compat_for_exn t m =
  match t with
  | Singleton _ | Stdlib _ | Unwrapped _ -> assert false
  | Impl _ -> Code_error.raise "wrapped compat not supported for vlib" []
  | Wrapped { group; _ } -> (
    match Module_name.Map.find group.modules (Module.name m) with
    | None -> assert false
    | Some (Module m) -> m
    | Some (Group g) -> Wrapped.Group.lib_interface g)

let rec for_alias t m =
  match t with
  | Stdlib _ | Singleton _ | Unwrapped _ -> Module_name.Map.empty
  | Wrapped w -> Wrapped.for_alias w m
  | Impl { vlib; impl } ->
    let impl = for_alias impl m in
    let vlib = for_alias vlib m in
    Module_name.Map.merge impl vlib ~f:(fun _ impl vlib ->
        match (impl, vlib) with
        | None, None -> assert false
        | Some _, _ -> impl
        | _, Some vlib -> Option.some_if (Module.visibility vlib = Public) vlib)

let wrapped_compat = function
  | Stdlib _ | Singleton _ | Impl _ | Unwrapped _ -> Module_name.Map.empty
  | Wrapped w -> w.wrapped_compat

let rec fold_user_available t ~f ~init =
  match t with
  | Stdlib w -> Stdlib.fold w ~init ~f
  | Singleton m -> f m init
  | Unwrapped modules -> Module_name.Map.fold modules ~init ~f
  | Wrapped { group; _ } -> Wrapped.Group.fold group ~init ~f
  | Impl { impl; vlib = _ } ->
    (* XXX shouldn't we folding over [vlib] as well? *)
    fold_user_available impl ~f ~init

let is_user_written m =
  match Module.kind m with
  | Root -> false
  | Wrapped_compat | Alias ->
    (* Logically, this should be [acc]. But this is unreachable these are stored
       separately *)
    assert false
  | _ -> true

let rec fold_user_written t ~f ~init =
  let f m acc = if is_user_written m then f m acc else acc in
  match t with
  | Stdlib w -> Stdlib.fold w ~init ~f
  | Singleton m -> f m init
  | Unwrapped modules -> Module_name.Map.fold modules ~init ~f
  | Wrapped { group; _ } -> Wrapped.Group.fold group ~init ~f
  | Impl { impl; vlib = _ } -> fold_user_written impl ~f ~init

let rec map_user_written t ~f =
  let f m = if is_user_written m then f m else Memo.return m in
  let open Memo.O in
  match t with
  | Singleton m ->
    let+ res = f m in
    Singleton res
  | Unwrapped m ->
    let+ res = Module_name.Map_traversals.parallel_map m ~f:(fun _ -> f) in
    Unwrapped res
  | Stdlib w ->
    let+ res = Stdlib.traverse w ~f in
    Stdlib res
  | Wrapped ({ group; wrapped_compat = _; wrapped = _ } as w) ->
    let+ group = Wrapped.Group.Memo_traversals.parallel_map group ~f in
    Wrapped { w with group }
  | Impl t ->
    let+ vlib = map_user_written t.vlib ~f in
    Impl { t with vlib }

let version_installed t ~install_dir =
  let f = Module.set_src_dir ~src_dir:install_dir in
  let rec loop = function
    | Singleton m -> Singleton (f m)
    | Unwrapped m -> Unwrapped (Module_name.Map.map ~f m)
    | Stdlib w -> Stdlib (Stdlib.map w ~f)
    | Wrapped w -> Wrapped (Wrapped.map w ~f)
    | Impl w -> Impl { w with impl = loop w.impl }
  in
  loop t

module Sourced_module = struct
  type t =
    | Normal of Module.t
    | Imported_from_vlib of Module.t
    | Impl_of_virtual_module of Module.t Ml_kind.Dict.t
end

let rec obj_map : 'a. t -> f:(Sourced_module.t -> 'a) -> 'a Module.Obj_map.t =
 fun t ~f ->
  let normal m = f (Sourced_module.Normal m) in
  match t with
  | Singleton m -> Module.Obj_map.add_exn Module.Obj_map.empty m (normal m)
  | Unwrapped m ->
    Module_name.Map.fold m ~init:Module.Obj_map.empty ~f:(fun m acc ->
        Module.Obj_map.add_exn acc m (normal m))
  | Wrapped w -> Wrapped.obj_map w ~f:normal
  | Stdlib w -> Stdlib.obj_map w ~f:normal
  | Impl { vlib; impl } ->
    Module.Obj_map.merge (obj_map vlib ~f:Fun.id) (obj_map impl ~f:Fun.id)
      ~f:(fun _ vlib impl ->
        match (vlib, impl) with
        | None, None -> assert false
        | Some (Normal m), None ->
          Some (f (Sourced_module.Imported_from_vlib m))
        | None, Some (Normal m) -> Some (f (Normal m))
        | Some (Normal intf), Some (Normal impl) ->
          Some (f (Sourced_module.Impl_of_virtual_module { intf; impl }))
        | Some (Imported_from_vlib _ | Impl_of_virtual_module _), _
        | _, Some (Imported_from_vlib _ | Impl_of_virtual_module _) ->
          assert false)

let obj_map_build :
      'a. t -> f:(Sourced_module.t -> 'a Memo.t) -> 'a Module.Obj_map.t Memo.t =
 fun t ~f ->
  Module.Obj_map_traversals.parallel_map (obj_map t ~f) ~f:(fun _ x -> x)

let entry_modules t =
  List.filter
    ~f:(fun m -> Module.visibility m = Public)
    (match t with
    | Stdlib w -> Stdlib.lib_interface w |> Option.to_list
    | Singleton m -> [ m ]
    | Unwrapped m -> Module_name.Map.values m
    | Wrapped m ->
      [ (* we assume this is never called for implementations *)
        Wrapped.lib_interface m
      ]
    | Impl i ->
      Code_error.raise "entry_modules: not defined for implementations"
        [ ("impl", dyn_of_impl i) ])

let virtual_module_names =
  fold_no_vlib ~init:Module_name.Path.Set.empty ~f:(fun m acc ->
      match Module.kind m with
      | Virtual -> Module_name.Path.Set.add acc [ Module.name m ]
      | _ -> acc)

let rec wrapped = function
  | Wrapped w -> w.wrapped
  | Singleton _ | Unwrapped _ -> Simple false
  | Stdlib _ -> Simple true
  | Impl { vlib = _; impl } -> wrapped impl

let rec alias_for t m =
  match Module.kind m with
  | Root -> []
  | _ -> (
    match t with
    | Singleton _ | Unwrapped _ -> []
    | Wrapped w -> Wrapped.alias_for w m
    | Stdlib w -> Stdlib.alias_for w m |> Option.to_list
    | Impl { impl; vlib = _ } -> alias_for impl m)

let local_open t m = alias_for t m |> List.map ~f:Module.name

let is_stdlib_alias t m =
  match t with
  | Stdlib w -> w.main_module_name = Module.name m
  | _ -> false

let exit_module = function
  | Stdlib w -> Stdlib.exit_module w
  | _ -> None

let relocate_alias_module t ~src_dir =
  match t with
  | Wrapped t -> Wrapped (Wrapped.relocate_alias_module t ~src_dir)
  | s -> s

let as_singleton = function
  | Singleton m -> Some m
  | _ -> None

let source_dirs =
  fold_user_written ~init:Path.Set.empty ~f:(fun m acc ->
      Module.sources m
      |> List.fold_left ~init:acc ~f:(fun acc f ->
             Path.Set.add acc (Path.parent_exn f)))
