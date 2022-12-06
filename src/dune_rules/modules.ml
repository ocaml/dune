open Import
module Mode = Wrapped

module Common = struct
  module Encode = struct
    open Dune_lang.Encoder

    let main_module_name = field "main_module_name" Module_name.encode

    let modules ?(name = "modules") modules =
      field_l name Fun.id (Module.Name_map.encode modules)
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
          else
            let path = [ main_module_name; Module.name m ] in
            let m = Module.set_path m path in
            Module.set_obj_name m (Module_name.Path.wrap path))
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
            sprintf "%s__%s"
              (Module_name.to_string main_module_name)
              (Lib_name.Local.to_string lib)
            |> Module_name.of_string
        ; public = main_module_name
        })
    | Exe ->
      sprintf "dune__exe" |> Module_name.of_string |> Visibility.Map.make_both
    | Melange ->
      sprintf "melange" |> Module_name.of_string |> Visibility.Map.make_both

  let make_alias_module (t : t) ~has_lib_interface ~src_dir ~interface path =
    let kind : Module.Kind.t = Alias path in
    let prefix, has_lib_interface =
      let prefix = prefix t in
      match t with
      | Lib { kind = Implementation _; _ } -> (prefix.private_, true)
      | _ -> (prefix.public, has_lib_interface)
    in
    let obj_name =
      Module_name.Path.wrap
      @@ prefix
         ::
         (if has_lib_interface then
          Module_name.Path.append_double_underscore path
         else path)
    in
    let name = Module_name.Unique.to_name ~loc:Loc.none obj_name in
    let path = if has_lib_interface then [ name ] else interface :: path in
    Module.generated ~path ~obj_name ~kind ~src_dir name

  let wrap_module t m ~interface =
    let is_lib_interface =
      match interface with
      | None -> false
      | Some interface -> Module_name.equal interface (Module.name m)
    in
    let path_for_mangle =
      let path = Module.path m in
      let prefix = prefix t in
      match t with
      | Exe | Melange -> prefix.public :: path
      | Lib _ ->
        let path =
          if is_lib_interface then List.rev path |> List.tl |> List.rev
          else path
        in
        Visibility.Map.find prefix (Module.visibility m) :: path
    in
    Module.set_obj_name m (Module_name.Path.wrap path_for_mangle)
end

let impl_only_of_map m =
  Module_name.Map.fold m ~init:[] ~f:(fun m acc ->
      if Module.has m ~ml_kind:Impl then m :: acc else acc)

module Wrapped = struct
  module Group = struct
    type t =
      { alias : Module.t
      ; modules : node Module_name.Map.t
      ; name : Module_name.t
      }

    and node =
      | Group of t
      | Module of Module.t

    let of_trie (trie : Module.t Module_trie.t) ~mangle ~src_dir : t =
      let rec loop interface rev_path trie =
        let has_lib_interface =
          match Module_name.Map.find trie interface with
          | None | Some (Module_trie.Map _) -> false
          | Some (Leaf _) -> true
        in
        { alias =
            Mangle.make_alias_module mangle ~has_lib_interface ~src_dir
              ~interface (List.rev rev_path)
        ; name = interface
        ; modules =
            Module_name.Map.mapi trie ~f:(fun name (m : 'a Module_trie.node) ->
                let rev_path = name :: rev_path in
                match m with
                | Map m -> Group (loop name rev_path m)
                | Leaf m ->
                  let m = Module.set_path m (List.rev rev_path) in
                  Module
                    (Mangle.wrap_module mangle m ~interface:(Some interface)))
        }
      in
      let prefix = (Mangle.prefix mangle).public in
      loop prefix [] trie

    let rec relocate_alias_module t ~src_dir =
      { t with
        alias = Module.set_src_dir t.alias ~src_dir
      ; modules =
          Module_name.Map.map t.modules ~f:(function
            | Module m -> Module m
            | Group g -> Group (relocate_alias_module g ~src_dir))
      }

    let rec fold { alias; modules; name = _ } ~f ~init =
      let init = f alias init in
      Module_name.Map.fold modules ~init ~f:(fun node init ->
          match node with
          | Module m -> f m init
          | Group t -> fold t ~f ~init)

    let rec exists { alias; modules; name = _ } ~f =
      f alias
      || Module_name.Map.exists modules ~f:(function
           | Module m -> f m
           | Group p -> exists p ~f)

    let rec to_dyn { alias; modules; name } =
      let open Dyn in
      record
        [ ("alias", Module.to_dyn alias)
        ; ("name", Module_name.to_dyn name)
        ; ( "modules"
          , Module_name.Map.to_dyn
              (function
                | Module m -> variant "module" [ Module.to_dyn m ]
                | Group g -> variant "group" [ to_dyn g ])
              modules )
        ]

    let rec map ({ alias; modules; name = _ } as t) ~f =
      let alias = f alias in
      let modules =
        Module_name.Map.map modules ~f:(function
          | Module m -> Module (f m)
          | Group g -> Group (map g ~f))
      in
      { t with alias; modules }

    let lib_interface t =
      match Module_name.Map.find t.modules t.name with
      | None | Some (Group _) -> t.alias
      | Some (Module m) -> m

    let decode ~src_dir =
      let open Dune_lang.Decoder in
      let rec t =
        lazy
          (fields
          @@ let+ alias = field "alias" (Module.decode ~src_dir)
             and+ modules =
               field ~default:[] "modules" (repeat (Lazy.force node))
             and+ name = field "name" Module_name.decode in
             { alias; modules = Module_name.Map.of_list_exn modules; name })
      and node =
        lazy
          (sum
             [ ( "module"
               , let+ m = Module.decode ~src_dir in
                 (Module.name m, Module m) )
             ; ( "group"
               , let* p = Module_name.decode in
                 let+ m = Lazy.force t in
                 (p, Group m) )
             ])
      in
      Lazy.force t

    let rec encode { alias; modules; name } =
      let open Dune_lang.Encoder in
      record_fields
        [ field_l "alias" sexp (Module.encode alias)
        ; field "name" Module_name.encode name
        ; field_l "modules" Fun.id
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
          | None ->
            (* TODO this happens with "side" modules like menhir mock modules *)
            acc
          | Some (Module _) -> acc
          | Some (Group g) -> loop (g :: acc) g ps)
      in
      loop [ t ] t (Module.path m)

    module Memo_traversals = struct
      let rec parallel_map ({ alias; modules; name = _ } as t) ~f =
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
    ; toplevel_module : [ `Exported | `Hidden ]
    }

  let to_dyn { group; wrapped_compat; wrapped; toplevel_module = _ } =
    let open Dyn in
    record
      [ ("group", Group.to_dyn group)
      ; ("wrapped_compat", Module_name.Map.to_dyn Module.to_dyn wrapped_compat)
      ; ("wrapped", Mode.to_dyn wrapped)
      ]

  let lib_interface t = Group.lib_interface t.group

  let fold_user_available { group; toplevel_module; _ } ~init ~f =
    let init =
      match toplevel_module with
      | `Hidden -> init
      | `Exported -> f group.alias init
    in
    Module_name.Map.fold group.modules ~init ~f:(fun node init ->
        match node with
        | Module m -> f m init
        | Group t -> Group.fold t ~f ~init)

  let for_alias (t : t) alias =
    let path =
      match Module.kind alias with
      | Alias for_ -> for_
      | _ -> Code_error.raise "for_alias: not an alias module" []
    in
    let rec loop (t : Group.t) = function
      | [] -> t
      | name :: path -> (
        match Module_name.Map.find t.modules name with
        | Some (Group g) -> loop g path
        | Some (Module m) ->
          Code_error.raise "for_alias: unexpected module"
            [ ("m", Module.to_dyn m); ("alias", Module.to_dyn alias) ]
        | None ->
          Code_error.raise "for_alias: not found"
            [ ("alias", Module.to_dyn alias) ])
    in
    let group = loop t.group path in
    Module_name.Map.remove group.modules group.name
    |> Module_name.Map.map ~f:(fun (g : Group.node) ->
           match g with
           | Module m -> m
           | Group g -> Group.lib_interface g)

  let encode { group; wrapped_compat; wrapped; toplevel_module = _ } =
    let open Dune_lang.Encoder in
    let module E = Common.Encode in
    record_fields
      [ field_l "group" Fun.id (Group.encode group)
      ; E.modules ~name:"wrapped_compat" wrapped_compat
      ; field "wrapped" Wrapped.encode wrapped
      ]

  (* TODO remove this eventually *)
  let old_decode ~src_dir =
    let open Dune_lang.Decoder in
    let open Common.Decode in
    fields
      (let+ main_module_name = main_module_name
       and+ modules = modules ~src_dir ()
       and+ wrapped_compat = modules ~name:"wrapped_compat" ~src_dir ()
       and+ alias = field "alias_module" (Module.decode ~src_dir)
       and+ wrapped = field "wrapped" Mode.decode in
       let group =
         { Group.alias
         ; name = main_module_name
         ; modules = modules |> Module_name.Map.map ~f:(fun m -> Group.Module m)
         }
       in
       { group; wrapped_compat; wrapped; toplevel_module = `Exported })

  let new_decode ~src_dir =
    let open Dune_lang.Decoder in
    let open Common.Decode in
    fields
      (let+ group = field "group" (Group.decode ~src_dir)
       and+ wrapped_compat = modules ~name:"wrapped_compat" ~src_dir ()
       and+ wrapped = field "wrapped" Mode.decode in
       { group; wrapped_compat; wrapped; toplevel_module = `Exported })

  let decode ~src_dir =
    let open Dune_lang.Decoder in
    new_decode ~src_dir <|> old_decode ~src_dir

  let map ({ group; wrapped_compat; toplevel_module = _; wrapped = _ } as t) ~f
      =
    { t with
      group = Group.map group ~f
    ; wrapped_compat = Module_name.Map.map wrapped_compat ~f
    }

  let make ~src_dir ~lib_name ~implements ~modules ~main_module_name ~wrapped =
    let mangle =
      Mangle.of_lib ~main_module_name ~lib_name ~implements ~modules
    in
    let wrapped_compat =
      match (wrapped : Mode.t) with
      | Simple false -> assert false
      | Simple true -> Module_name.Map.empty
      | Yes_with_transition _ ->
        let toplevel = Module_trie.toplevel_only modules in
        Module_name.Map.remove toplevel main_module_name
        |> Module_name.Map.filter_map ~f:(fun m ->
               match Module.visibility m with
               | Private -> None
               | Public -> Some (Module.wrapped_compat m))
    in
    let group = Group.of_trie modules ~mangle ~src_dir in
    { group; wrapped_compat; wrapped; toplevel_module = `Exported }

  let make_exe_or_melange ~src_dir ~modules mangle =
    let group = Group.of_trie modules ~mangle ~src_dir in
    { group
    ; wrapped_compat = Module_name.Map.empty
    ; wrapped = Simple true
    ; toplevel_module = `Hidden
    }

  let obj_map { group; wrapped_compat; wrapped = _; toplevel_module = _ } ~f =
    let add_module m acc = Module.Obj_map.add_exn acc m (f m) in
    let init = Group.fold group ~init:Module.Obj_map.empty ~f:add_module in
    Module_name.Map.fold ~init wrapped_compat ~f:add_module

  let impl_only { group; wrapped_compat; wrapped = _; toplevel_module = _ } =
    let init = Module_name.Map.values wrapped_compat in
    Group.fold group ~init ~f:(fun v acc ->
        if Module.has v ~ml_kind:Impl then v :: acc else acc)

  let fold { group; wrapped_compat; wrapped = _; toplevel_module = _ } ~init ~f
      =
    let init = Group.fold group ~f ~init in
    Module_name.Map.fold wrapped_compat ~f ~init

  let exists { group; wrapped_compat; wrapped = _; toplevel_module = _ } ~f =
    Group.exists group ~f || Module_name.Map.exists wrapped_compat ~f

  let find t name =
    match Module_name.Map.find t.group.modules name with
    | Some (Module m) -> Some m
    | Some (Group _) | None -> None

  let find_dep =
    let rec closure_group g =
      let lib_interface = Group.lib_interface g in
      match Module.kind lib_interface with
      | Alias _ ->
        (* XXX ocamldep can't currently give us precise dependencies for
           modules under [(include_subdirs qualified)] directories. For that
           reason we currently depend on everything under the sub-directory. *)
        Module_name.Map.values g.modules |> List.concat_map ~f:closure_node
      | _ -> [ lib_interface ]
    and closure_node = function
      | Module m -> [ m ]
      | Group g -> closure_group g
    in
    fun t ~of_ name ->
      match Module.kind of_ with
      | Alias _ -> Ok []
      | Wrapped_compat ->
        let li = lib_interface t in
        Ok (if Module_name.equal name (Module.name li) then [ li ] else [])
      | _ -> (
        (* TODO don't recompute this  *)
        let parents = Group.parents t.group of_ in
        match
          List.find_map parents ~f:(fun parent ->
              if Module_name.equal parent.name name then Some `Parent_cycle
              else
                Module_name.Map.find parent.modules name
                |> Option.map ~f:(fun x -> `Found x))
        with
        | None -> Ok []
        | Some `Parent_cycle -> Error `Parent_cycle
        | Some (`Found m) -> Ok (closure_node m))

  let group_interfaces (t : t) m =
    Group.parents t.group m |> List.map ~f:Group.lib_interface

  let alias_for t m =
    match Module.kind m with
    | Alias _ | Wrapped_compat -> []
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
  | Wrapped w -> Some w.group.name
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

exception Parent_cycle

let find_dep =
  let from_impl_or_lib = List.map ~f:(fun m -> (`Impl_or_lib, m)) in
  let find_dep_result =
    List.filter_map ~f:(fun (from, m) ->
        match from with
        | `Impl_or_lib -> Some m
        | `Vlib -> Option.some_if (Module.visibility m = Public) m)
  in
  let rec find_dep t ~of_ name : Module.t list =
    if Module.name of_ = name then []
    else
      let result =
        match t with
        | Wrapped w -> (
          match Wrapped.find_dep w ~of_ name with
          | Ok s -> from_impl_or_lib s
          | Error `Parent_cycle -> raise_notrace Parent_cycle)
        | Stdlib s ->
          Stdlib.find_dep s ~of_ name |> Option.to_list |> from_impl_or_lib
        | Impl { vlib; impl } -> (
          match find_dep impl ~of_ name with
          | [] -> find_dep vlib ~of_ name |> List.map ~f:(fun m -> (`Vlib, m))
          | xs -> from_impl_or_lib xs)
        | _ -> find t name |> Option.to_list |> from_impl_or_lib
      in
      find_dep_result result
  in
  fun t ~of_ name ->
    match find_dep t ~of_ name with
    | s -> Ok s
    | exception Parent_cycle -> Error `Parent_cycle

let make_singleton m mangle =
  Singleton
    (let name = Module.name m in
     let m = Module.set_path m [ name ] in
     Mangle.wrap_module mangle m ~interface:None)

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

let fold_no_vlib_with_aliases t ~init ~normal ~alias =
  fold_no_vlib t ~init ~f:(fun m acc ->
      match Module.kind m with
      | Alias _ -> alias m (for_alias t m) acc
      | _ -> normal m acc)

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

let wrapped_compat = function
  | Stdlib _ | Singleton _ | Impl _ | Unwrapped _ -> Module_name.Map.empty
  | Wrapped w -> w.wrapped_compat

let rec fold_user_available t ~f ~init =
  match t with
  | Stdlib w -> Stdlib.fold w ~init ~f
  | Singleton m -> f m init
  | Unwrapped modules -> Module_name.Map.fold modules ~init ~f
  | Wrapped w -> Wrapped.fold_user_available w ~init ~f
  | Impl { impl; vlib = _ } ->
    (* XXX shouldn't we folding over [vlib] as well? *)
    fold_user_available impl ~f ~init

let is_user_written m =
  match Module.kind m with
  | Root | Wrapped_compat | Alias _ -> false
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
  | Wrapped
      ({ group; wrapped_compat = _; wrapped = _; toplevel_module = _ } as w) ->
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

let rec group_interfaces t m =
  match t with
  | Wrapped w -> Wrapped.group_interfaces w m
  | Impl { impl; vlib } -> group_interfaces impl m @ group_interfaces vlib m
  | Singleton w -> [ w ]
  | _ -> []

let local_open t m =
  alias_for t m
  |> List.map ~f:(fun m ->
         Module.obj_name m |> Module_name.Unique.to_name ~loc:Loc.none)

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
