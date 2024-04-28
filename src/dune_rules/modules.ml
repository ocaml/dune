open Import

module Common = struct
  module Encode = struct
    open Dune_lang.Encoder

    let main_module_name = field "main_module_name" Module_name.encode

    let modules ?(name = "modules") ~src_dir modules =
      field_l name Fun.id (Module.Name_map.encode modules ~src_dir)
    ;;
  end

  module Decode = struct
    open Dune_lang.Decoder

    let main_module_name = field "main_module_name" Module_name.decode

    let modules ?(name = "modules") ~src_dir () =
      field ~default:Module_name.Map.empty name (Module.Name_map.decode ~src_dir)
    ;;
  end
end

module Stdlib = struct
  type t =
    { modules : Module.Name_map.t
    ; unwrapped : Module_name.Set.t
    ; exit_module : Module_name.t option
    ; main_module_name : Module_name.t
    }

  let encode ~src_dir { modules; unwrapped; exit_module; main_module_name } =
    let open Dune_lang.Encoder in
    record_fields
      [ Common.Encode.main_module_name main_module_name
      ; Common.Encode.modules modules ~src_dir
      ; field_o "exit_module" Module_name.encode exit_module
      ; field_l "unwrapped" Module_name.encode (Module_name.Set.to_list unwrapped)
      ]
  ;;

  let decode ~src_dir =
    let open Dune_lang.Decoder in
    fields
      (let+ main_module_name = Common.Decode.main_module_name
       and+ modules = Common.Decode.modules ~src_dir ()
       and+ exit_module = field_o "exit_module" Module_name.decode
       and+ unwrapped = field ~default:[] "unwrapped" (repeat Module_name.decode) in
       let unwrapped = Module_name.Set.of_list unwrapped in
       { modules; main_module_name; exit_module; unwrapped })
  ;;

  let to_dyn { modules; unwrapped; exit_module; main_module_name } =
    let open Dyn in
    record
      [ "modules", Module.Name_map.to_dyn modules
      ; "unwrapped", Module_name.Set.to_dyn unwrapped
      ; "exit_module", option Module_name.to_dyn exit_module
      ; "main_module_name", Module_name.to_dyn main_module_name
      ]
  ;;

  let exit_module t =
    let open Option.O in
    let* name = t.exit_module in
    Module_name.Map.find t.modules name
  ;;

  let exists t ~f = Module_name.Map.exists t.modules ~f
  let fold t ~init ~f = Module_name.Map.fold t.modules ~f ~init
  let map t ~f = { t with modules = Module_name.Map.map t.modules ~f }

  let traverse t ~f =
    let open Memo.O in
    let+ modules = Module_name.Parallel_map.parallel_map t.modules ~f:(fun _ -> f) in
    { t with modules }
  ;;

  let lib_interface t = Module_name.Map.find t.modules t.main_module_name

  (* Returns [true] is a special module, i.e. one whose compilation unit name is
     hard-coded inside the compiler. It is not possible to change the
     compilation unit name of such modules, so they cannot be wrapped. *)
  let special_compiler_module (stdlib : Ocaml_stdlib.t) m =
    let name = Module.name m in
    Predicate_lang.Glob.test
      stdlib.internal_modules
      ~standard:Predicate_lang.false_
      (Module_name.to_string name)
    ||
    match stdlib.exit_module with
    | None -> false
    | Some n -> n = name
  ;;

  let make ~(stdlib : Ocaml_stdlib.t) ~modules ~wrapped ~main_module_name =
    let modules =
      match wrapped with
      | Wrapped.Simple false -> modules
      | Simple true | Yes_with_transition _ ->
        Module_name.Map.map modules ~f:(fun m ->
          if Module.name m = main_module_name || special_compiler_module stdlib m
          then m
          else (
            let path = [ main_module_name; Module.name m ] in
            let m = Module.set_path m path in
            Module.set_obj_name m (Module_name.Path.wrap path)))
    in
    let unwrapped = stdlib.modules_before_stdlib in
    let exit_module = stdlib.exit_module in
    { modules; unwrapped; exit_module; main_module_name }
  ;;

  let impl_only t =
    Module_name.Map.values t.modules
    |> List.filter ~f:(fun m ->
      (* TODO what about checking for implementations? *)
      Some (Module.name m) <> t.exit_module)
  ;;

  let find t = Module_name.Map.find t.modules

  let find_dep t ~of_ name =
    let of_name = Module.name of_ in
    if of_name = t.main_module_name
    then
      if Module_name.Set.mem t.unwrapped name
      then Module_name.Map.find t.modules name
      else None
    else Module_name.Map.find t.modules name
  ;;

  let alias_for t m =
    let name = Module.name m in
    if name = t.main_module_name || Module_name.Set.mem t.unwrapped name
    then None
    else lib_interface t
  ;;
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
    | Unwrapped

  let of_lib ~lib_name ~implements ~main_module_name ~modules =
    let kind : Lib.kind =
      if implements
      then Implementation lib_name
      else if Module_trie.mem modules [ main_module_name ]
      then Has_lib_interface
      else Neither
    in
    Lib { main_module_name; kind }
  ;;

  let prefix t : Module_name.t Visibility.Map.t option =
    match t with
    | Lib { main_module_name; kind } ->
      (match kind with
       | Has_lib_interface | Neither -> Some (Visibility.Map.make_both main_module_name)
       | Implementation lib ->
         Some
           { private_ =
               sprintf
                 "%s__%s"
                 (Module_name.to_string main_module_name)
                 (Lib_name.Local.to_string lib)
               |> Module_name.of_string
           ; public = main_module_name
           })
    | Exe ->
      sprintf "dune__exe"
      |> Module_name.of_string
      |> Visibility.Map.make_both
      |> Option.some
    | Melange ->
      sprintf "melange"
      |> Module_name.of_string
      |> Visibility.Map.make_both
      |> Option.some
    | Unwrapped -> None
  ;;

  let make_alias_module (t : t) ~has_lib_interface ~obj_dir ~interface path =
    let kind : Module.Kind.t = Alias path in
    let prefix, has_lib_interface =
      let prefix = prefix t in
      match t with
      | Lib { kind = Implementation _; _ } ->
        Option.map prefix ~f:(fun p -> p.private_), true
      | _ -> Option.map prefix ~f:(fun p -> p.public), has_lib_interface
    in
    let obj_name =
      Module_name.Path.wrap
      @@
      let base =
        if has_lib_interface then Module_name.Path.append_double_underscore path else path
      in
      match prefix with
      | None -> base
      | Some prefix -> prefix :: base
    in
    let path =
      if has_lib_interface
      then [ Module_name.Unique.to_name ~loc:Loc.none obj_name ]
      else path @ [ interface ]
    in
    let install_as =
      if has_lib_interface
      then None
      else
        Some
          (Path.Local.L.relative
             Path.Local.root
             (List.map ~f:Module_name.uncapitalize path)
           |> Path.Local.set_extension ~ext:".ml")
    in
    Module.generated ?install_as path ~obj_name ~kind ~src_dir:obj_dir
  ;;

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
      | Exe | Melange -> (Option.value_exn prefix).public :: path
      | Unwrapped -> if is_lib_interface then List.remove_last_exn path else path
      | Lib _ ->
        let path = if is_lib_interface then List.remove_last_exn path else path in
        Visibility.Map.find (Option.value_exn prefix) (Module.visibility m) :: path
    in
    Module.set_obj_name m (Module_name.Path.wrap path_for_mangle)
  ;;
end

module Group = struct
  type t =
    { alias : Module.t
    ; modules : node Module_name.Map.t
    ; name : Module_name.t
    }

  and node =
    | Group of t
    | Module of Module.t

  let alias t = t.alias

  module Of_trie = struct
    let of_trie ~obj_dir ~mangle ~interface ~rev_path trie =
      let rec loop interface rev_path trie =
        let has_lib_interface =
          match Module_name.Map.find trie interface with
          | None | Some (Module_trie.Map _) -> false
          | Some (Leaf _) -> true
        in
        { alias =
            Mangle.make_alias_module
              mangle
              ~has_lib_interface
              ~obj_dir
              ~interface
              (List.rev rev_path)
        ; name = interface
        ; modules =
            Module_name.Map.mapi trie ~f:(fun name (m : 'a Module_trie.node) ->
              let rev_path = name :: rev_path in
              match m with
              | Map m -> Group (loop name rev_path m)
              | Leaf m ->
                let m = Module.set_path m (List.rev rev_path) in
                Module (Mangle.wrap_module mangle m ~interface:(Some interface)))
        }
      in
      loop interface rev_path trie
    ;;
  end

  let of_trie (trie : Module.t Module_trie.t) ~mangle ~obj_dir : t =
    let prefix =
      Mangle.prefix mangle |> Option.map ~f:(fun (p : _ Visibility.Map.t) -> p.public)
    in
    Of_trie.of_trie
      ~obj_dir
      ~mangle
      ~interface:(Option.value_exn prefix)
      ~rev_path:[]
      trie
  ;;

  let rec fold { alias; modules; name = _ } ~f ~init =
    let init = f alias init in
    fold_modules modules ~f ~init

  and fold_modules modules ~f ~init =
    Module_name.Map.fold modules ~init ~f:(fun node init ->
      match node with
      | Module m -> f m init
      | Group t -> fold t ~f ~init)
  ;;

  let rec exists { alias; modules; name = _ } ~f = f alias || exists_modules modules ~f

  and exists_modules modules ~f =
    Module_name.Map.exists modules ~f:(function
      | Module m -> f m
      | Group g -> exists g ~f)
  ;;

  let rec to_dyn { alias; modules; name } =
    let open Dyn in
    record
      [ "alias", Module.to_dyn alias
      ; "name", Module_name.to_dyn name
      ; "modules", Module_name.Map.to_dyn dyn_of_node modules
      ]

  and dyn_of_node =
    let open Dyn in
    function
    | Module m -> variant "module" [ Module.to_dyn m ]
    | Group g -> variant "group" [ to_dyn g ]
  ;;

  let rec map ({ alias; modules; name = _ } as t) ~f =
    let alias = f alias in
    let modules = map_modules modules ~f in
    { t with alias; modules }

  and map_modules modules ~f =
    Module_name.Map.map modules ~f:(function
      | Module m -> Module (f m)
      | Group g -> Group (map g ~f))
  ;;

  let lib_interface t =
    match Module_name.Map.find t.modules t.name with
    | None | Some (Group _) -> t.alias
    | Some (Module m) -> m
  ;;

  let rec decode ~src_dir =
    let open Dune_lang.Decoder in
    fields
    @@ let+ alias = field "alias" (Module.decode ~src_dir)
       and+ modules =
         field ~default:Module_name.Map.empty "modules" (decode_modules ~src_dir)
       and+ name = field "name" Module_name.decode in
       { alias; modules; name }

  and decode_modules ~src_dir =
    let open Dune_lang.Decoder in
    let node =
      sum
        [ ( "module"
          , let+ m = Module.decode ~src_dir in
            Module.name m, Module m )
        ; ( "group"
          , let* p = Module_name.decode in
            let+ m = decode ~src_dir in
            p, Group m )
        ]
    in
    let+ modules = repeat node in
    Module_name.Map.of_list_exn modules
  ;;

  let rec encode { alias; modules; name } ~src_dir =
    let open Dune_lang.Encoder in
    record_fields
      [ field_l "alias" sexp (Module.encode ~src_dir alias)
      ; field "name" Module_name.encode name
      ; field_l "modules" Fun.id (encode_modules ~src_dir modules)
      ]

  and encode_modules modules ~src_dir =
    Module_name.Map.to_list_map modules ~f:(fun _ t ->
      Dune_lang.List
        (match t with
         | Group g ->
           Dune_lang.atom "group" :: Module_name.encode g.name :: encode ~src_dir g
         | Module m -> Dune_lang.atom "module" :: Module.encode ~src_dir m))
  ;;

  let parents_modules =
    let rec loop acc modules = function
      | [] -> acc
      | p :: ps ->
        (match Module_name.Map.find modules p with
         | None ->
           (* TODO this happens with "side" modules like menhir mock modules *)
           acc
         | Some (Module _) -> acc
         | Some (Group g) -> loop (g :: acc) g.modules ps)
    in
    fun acc modules m -> loop acc modules (Module.path m)
  ;;

  let parents (t : t) m = parents_modules [ t ] t.modules m

  module Memo_traversals = struct
    let rec parallel_map ({ alias; modules; name = _ } as t) ~f =
      let open Memo.O in
      let+ alias, modules =
        Memo.fork_and_join (fun () -> f alias) (fun () -> parallel_map_modules modules ~f)
      in
      { t with alias; modules }

    and parallel_map_modules modules ~f =
      let open Memo.O in
      Module_name.Parallel_map.parallel_map modules ~f:(fun _ n ->
        match n with
        | Module m ->
          let+ m = f m in
          Module m
        | Group g ->
          let+ g = parallel_map g ~f in
          Group g)
    ;;
  end

  let group_interfaces (t : t) m = parents t m |> List.map ~f:lib_interface

  let make_alias_for t m ~parents =
    match Module.kind m with
    | Alias _ | Wrapped_compat -> []
    | _ -> parents t m |> List.map ~f:(fun (s : t) -> s.alias)
  ;;

  let alias_for = make_alias_for ~parents

  let find t name =
    match Module_name.Map.find t.modules name with
    | Some (Module m) -> Some m
    | Some (Group _) | None -> None
  ;;

  module Find_dep = struct
    let rec closure_group g =
      let lib_interface = lib_interface g in
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
    ;;

    let find_dep_of_parents parents name =
      match
        List.find_map parents ~f:(fun (parent, name') ->
          match name' with
          | Some name' when Module_name.equal name' name -> Some `Parent_cycle
          | _ -> Module_name.Map.find parent name |> Option.map ~f:(fun x -> `Found x))
      with
      | None -> Ok []
      | Some `Parent_cycle -> Error `Parent_cycle
      | Some (`Found m) -> Ok (closure_node m)
    ;;
  end

  let find_dep t ~of_ name =
    match Module.kind of_ with
    | Alias _ -> Ok []
    | Wrapped_compat ->
      let li = lib_interface t in
      Ok (if Module_name.equal name (Module.name li) then [ li ] else [])
    | _ ->
      (* TODO don't recompute this *)
      let parents = parents t of_ |> List.map ~f:(fun g -> g.modules, Some g.name) in
      Find_dep.find_dep_of_parents parents name
  ;;

  module For_alias = struct
    let find_module alias modules name =
      match Module_name.Map.find modules name with
      | Some (Group g) -> g
      | Some (Module m) ->
        Code_error.raise
          "group_of_alias: unexpected module"
          [ "m", Module.to_dyn m; "alias", Module.to_dyn alias ]
      | None ->
        Code_error.raise
          "group_of_alias: not found"
          [ "name", Module_name.to_dyn name
          ; "modules", Module_name.Map.to_dyn dyn_of_node modules
          ; "alias", Module.to_dyn alias
          ]
    ;;

    let path_of_alias_module alias =
      match Module.kind alias with
      | Alias for_ -> for_
      | _ -> Code_error.raise "group_of_alias: not an alias module" []
    ;;

    let make_group_of_alias t alias path =
      let rec loop (t : t) = function
        | [] -> t
        | name :: path ->
          let group = find_module alias t.modules name in
          loop group path
      in
      loop t path
    ;;
  end

  let group_of_alias t alias =
    For_alias.make_group_of_alias t alias (For_alias.path_of_alias_module alias)
  ;;

  let for_alias t =
    Module_name.Map.remove t.modules t.name
    |> Module_name.Map.to_list_map ~f:(fun name node ->
      let m =
        match node with
        | Module m -> m
        | Group g -> lib_interface g
      in
      name, m)
  ;;
end

module Unwrapped = struct
  type t = Group.node Module_name.Map.t

  let decode ~src_dir =
    let open Dune_lang.Decoder in
    Group.decode_modules ~src_dir
    <|> let+ map = Module.Name_map.decode ~src_dir in
        Module_name.Map.map map ~f:(fun m -> Group.Module m)
  ;;

  let encode = Group.encode_modules
  let to_dyn t = Module_name.Map.to_dyn Group.dyn_of_node t

  let of_trie trie ~mangle ~obj_dir =
    Module_name.Map.mapi trie ~f:(fun name (m : 'a Module_trie.node) ->
      match m with
      | Map trie ->
        let group =
          Group.Of_trie.of_trie trie ~mangle ~obj_dir ~interface:name ~rev_path:[ name ]
        in
        Group.Group group
      | Leaf m ->
        let m = Module.set_path m [ name ] in
        Module m)
  ;;

  let find (t : t) name =
    match Module_name.Map.find t name with
    | Some (Module m) -> Some m
    | Some (Group _) | None -> None
  ;;

  let parents t m = Group.parents_modules [] t m

  let find_dep t ~of_ name =
    match Module.kind of_ with
    | Alias _ -> Ok []
    | Wrapped_compat -> assert false
    | _ ->
      let parents =
        (t, None)
        :: List.map (parents t of_) ~f:(fun (g : Group.t) -> g.modules, Some g.name)
      in
      Group.Find_dep.find_dep_of_parents parents name
  ;;

  let fold t ~init ~f = Group.fold_modules t ~init ~f
  let exists t ~f = Group.exists_modules t ~f
  let alias_for : t -> _ -> Module.t list = Group.make_alias_for ~parents
  let map t ~f = Group.map_modules t ~f

  let entry_modules m =
    Module_name.Map.to_list_map m ~f:(fun _ m ->
      match (m : Group.node) with
      | Module m -> m
      | Group g -> Group.lib_interface g)
  ;;

  let group_of_alias t alias =
    match Group.For_alias.path_of_alias_module alias with
    | [] -> assert false
    | name :: path ->
      let group = Group.For_alias.find_module alias t name in
      Group.For_alias.make_group_of_alias group alias path
  ;;

  module Memo_traversals = struct
    let parallel_map t ~f = Group.Memo_traversals.parallel_map_modules t ~f
  end
end

module Wrapped = struct
  type t =
    { group : Group.t
    ; wrapped_compat : Module.Name_map.t
    ; wrapped : Dune_lang.Wrapped.t
    ; toplevel_module : [ `Exported | `Hidden ]
    }

  let to_dyn { group; wrapped_compat; wrapped; toplevel_module = _ } =
    let open Dyn in
    record
      [ "group", Group.to_dyn group
      ; "wrapped_compat", Module_name.Map.to_dyn Module.to_dyn wrapped_compat
      ; "wrapped", Dune_lang.Wrapped.to_dyn wrapped
      ]
  ;;

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
  ;;

  let group_of_alias t m = Group.group_of_alias t.group m

  let encode { group; wrapped_compat; wrapped; toplevel_module = _ } ~src_dir =
    let open Dune_lang.Encoder in
    record_fields
      [ field_l "group" Fun.id (Group.encode ~src_dir group)
      ; Common.Encode.modules ~name:"wrapped_compat" ~src_dir wrapped_compat
      ; field "wrapped" Wrapped.encode wrapped
      ]
  ;;

  (* TODO remove this eventually *)
  let old_decode ~src_dir =
    let open Dune_lang.Decoder in
    let open Common.Decode in
    fields
      (let+ main_module_name = main_module_name
       and+ modules = modules ~src_dir ()
       and+ wrapped_compat = modules ~name:"wrapped_compat" ~src_dir ()
       and+ alias = field "alias_module" (Module.decode ~src_dir)
       and+ wrapped = field "wrapped" Dune_lang.Wrapped.decode in
       let group =
         { Group.alias
         ; name = main_module_name
         ; modules = modules |> Module_name.Map.map ~f:(fun m -> Group.Module m)
         }
       in
       { group; wrapped_compat; wrapped; toplevel_module = `Exported })
  ;;

  let new_decode ~src_dir =
    let open Dune_lang.Decoder in
    let open Common.Decode in
    fields
      (let+ group = field "group" (Group.decode ~src_dir)
       and+ wrapped_compat = modules ~name:"wrapped_compat" ~src_dir ()
       and+ wrapped = field "wrapped" Dune_lang.Wrapped.decode in
       { group; wrapped_compat; wrapped; toplevel_module = `Exported })
  ;;

  let decode ~src_dir =
    let open Dune_lang.Decoder in
    new_decode ~src_dir <|> old_decode ~src_dir
  ;;

  let map ({ group; wrapped_compat; toplevel_module = _; wrapped = _ } as t) ~f =
    { t with
      group = Group.map group ~f
    ; wrapped_compat = Module_name.Map.map wrapped_compat ~f
    }
  ;;

  let make ~obj_dir ~lib_name ~implements ~modules ~main_module_name ~wrapped =
    let mangle = Mangle.of_lib ~main_module_name ~lib_name ~implements ~modules in
    let wrapped_compat =
      match (wrapped : Dune_lang.Wrapped.t) with
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
    let group = Group.of_trie modules ~mangle ~obj_dir in
    { group; wrapped_compat; wrapped; toplevel_module = `Exported }
  ;;

  let make_exe_or_melange ~obj_dir ~modules mangle =
    let group = Group.of_trie modules ~mangle ~obj_dir in
    { group
    ; wrapped_compat = Module_name.Map.empty
    ; wrapped = Simple true
    ; toplevel_module = `Hidden
    }
  ;;

  let impl_only { group; wrapped_compat; wrapped = _; toplevel_module = _ } =
    let init = Module_name.Map.values wrapped_compat in
    Group.fold group ~init ~f:(fun v acc ->
      if Module.has v ~ml_kind:Impl then v :: acc else acc)
  ;;

  let fold { group; wrapped_compat; wrapped = _; toplevel_module = _ } ~init ~f =
    let init = Group.fold group ~f ~init in
    Module_name.Map.fold wrapped_compat ~f ~init
  ;;

  let exists { group; wrapped_compat; wrapped = _; toplevel_module = _ } ~f =
    Group.exists group ~f || Module_name.Map.exists wrapped_compat ~f
  ;;

  let find t name = Group.find t.group name
  let find_dep t ~of_ name = Group.find_dep t.group ~of_ name
  let group_interfaces (t : t) m = Group.group_interfaces t.group m
  let alias_for t m = Group.alias_for t.group m
end

module Sourced_module = struct
  type t =
    | Normal of Module.t
    | Imported_from_vlib of Module.t
    | Impl_of_virtual_module of Module.t Ml_kind.Dict.t

  let to_module = function
    | Normal m -> m
    | Imported_from_vlib m -> m
    | Impl_of_virtual_module { intf = _; impl } -> impl
  ;;
end

type modules =
  | Singleton of Module.t
  | Unwrapped of Unwrapped.t
  | Wrapped of Wrapped.t
  | Stdlib of Stdlib.t

type t =
  { obj_map : Sourced_module.t Module_name.Unique.Map.t Lazy.t
  ; modules : modules
  }

let obj_map : 'a. modules -> Sourced_module.t Module_name.Unique.Map.t =
  let module Map = Module_name.Unique.Map in
  let normal m = Sourced_module.Normal m in
  let f m acc = Map.add_exn acc (Module.obj_name m) (normal m) in
  fun t ->
    match t with
    | Singleton m -> Map.add_exn Map.empty (Module.obj_name m) (normal m)
    | Unwrapped m -> Unwrapped.fold m ~init:Map.empty ~f
    | Wrapped w -> Wrapped.fold w ~init:Map.empty ~f
    | Stdlib w -> Stdlib.fold w ~init:Map.empty ~f
;;

let with_obj_map modules =
  let obj_map = lazy (obj_map modules) in
  { obj_map; modules }
;;

let obj_map t = Lazy.force t.obj_map

let decode ~src_dir =
  let open Dune_lang.Decoder in
  sum
    [ ( "singleton"
      , let+ m = Module.decode ~src_dir in
        Singleton m )
    ; ( "unwrapped"
      , let+ modules = Unwrapped.decode ~src_dir in
        Unwrapped modules )
    ; ( "wrapped"
      , let+ w = Wrapped.decode ~src_dir in
        Wrapped w )
    ; ( "stdlib"
      , let+ stdlib = Stdlib.decode ~src_dir in
        Stdlib stdlib )
    ]
  >>| with_obj_map
;;

let to_dyn t =
  let open Dyn in
  match t.modules with
  | Singleton m -> variant "Singleton" [ Module.to_dyn m ]
  | Unwrapped m -> variant "Unwrapped" [ Unwrapped.to_dyn m ]
  | Wrapped w -> variant "Wrapped" [ Wrapped.to_dyn w ]
  | Stdlib s -> variant "Stdlib" [ Stdlib.to_dyn s ]
;;

let lib ~obj_dir ~main_module_name ~wrapped ~stdlib ~lib_name ~implements ~modules =
  let make_wrapped main_module_name =
    Wrapped
      (Wrapped.make ~obj_dir ~lib_name ~implements ~modules ~main_module_name ~wrapped)
  in
  let modules =
    match stdlib with
    | Some stdlib ->
      let main_module_name = Option.value_exn main_module_name in
      let modules = Module_trie.to_map modules in
      Stdlib (Stdlib.make ~stdlib ~modules ~wrapped ~main_module_name)
    | None ->
      (match wrapped, main_module_name, Module_trie.as_singleton modules with
       | Simple false, _, Some m -> Singleton m
       | Simple false, _, None ->
         let mangle = Mangle.Unwrapped in
         Unwrapped (Unwrapped.of_trie modules ~mangle ~obj_dir)
       | (Yes_with_transition _ | Simple true), Some main_module_name, Some m ->
         if Module.name m = main_module_name && not implements
         then Singleton m
         else make_wrapped main_module_name
       | (Yes_with_transition _ | Simple true), Some main_module_name, None ->
         make_wrapped main_module_name
       | (Simple true | Yes_with_transition _), None, _ ->
         Code_error.raise "Modules.lib: cannot wrap without main module name" [])
  in
  with_obj_map modules
;;

let make_singleton m mangle =
  let modules =
    Singleton
      (let name = Module.name m in
       let m = Module.set_path m [ name ] in
       Mangle.wrap_module mangle m ~interface:None)
  in
  with_obj_map modules
;;

let exe_unwrapped modules ~obj_dir =
  let mangle = Mangle.Unwrapped in
  let modules = Unwrapped (Unwrapped.of_trie modules ~mangle ~obj_dir) in
  with_obj_map modules
;;

let make_wrapped ~obj_dir ~modules kind =
  let mangle : Mangle.t =
    match kind with
    | `Exe -> Exe
    | `Melange -> Melange
  in
  match Module_trie.as_singleton modules with
  | Some m -> make_singleton m mangle
  | None ->
    let modules = Wrapped (Wrapped.make_exe_or_melange ~obj_dir ~modules mangle) in
    with_obj_map modules
;;

let fold t ~init ~f =
  match t.modules with
  | Stdlib w -> Stdlib.fold w ~init ~f
  | Singleton m -> f m init
  | Unwrapped m -> Unwrapped.fold m ~f ~init
  | Wrapped w -> Wrapped.fold w ~init ~f
;;

let wrapped t =
  match t.modules with
  | Wrapped w -> w.wrapped
  | Singleton _ | Unwrapped _ -> Simple false
  | Stdlib _ -> Simple true
;;

let is_user_written m =
  match Module.kind m with
  | Root | Wrapped_compat | Alias _ -> false
  | _ -> true
;;

let fold_user_available t ~f ~init =
  match t.modules with
  | Stdlib w -> Stdlib.fold w ~init ~f
  | Singleton m -> f m init
  | Unwrapped modules -> Unwrapped.fold modules ~init ~f
  | Wrapped w -> Wrapped.fold_user_available w ~init ~f
;;

let map_user_written t ~f =
  let f m = if is_user_written m then f m else Memo.return m in
  let open Memo.O in
  let+ modules =
    match t.modules with
    | Singleton m ->
      let+ res = f m in
      Singleton res
    | Unwrapped m ->
      let+ res = Unwrapped.Memo_traversals.parallel_map m ~f in
      Unwrapped res
    | Stdlib w ->
      let+ res = Stdlib.traverse w ~f in
      Stdlib res
    | Wrapped ({ group; wrapped_compat = _; wrapped = _; toplevel_module = _ } as w) ->
      let+ group = Group.Memo_traversals.parallel_map group ~f in
      Wrapped { w with group }
  in
  with_obj_map modules
;;

let fold_user_written t ~f ~init =
  let f m acc = if is_user_written m then f m acc else acc in
  match t.modules with
  | Stdlib w -> Stdlib.fold w ~init ~f
  | Singleton m -> f m init
  | Unwrapped modules -> Unwrapped.fold modules ~init ~f
  | Wrapped { group; _ } -> Group.fold group ~init ~f
;;

let virtual_module_names =
  fold ~init:Module_name.Path.Set.empty ~f:(fun m acc ->
    match Module.kind m with
    | Virtual -> Module_name.Path.Set.add acc [ Module.name m ]
    | _ -> acc)
;;

let source_dirs =
  fold_user_written ~init:Path.Set.empty ~f:(fun m acc ->
    Module.sources m
    |> List.fold_left ~init:acc ~f:(fun acc f -> Path.Set.add acc (Path.parent_exn f)))
;;

module With_vlib = struct
  type impl =
    { obj_map : Sourced_module.t Module_name.Unique.Map.t Lazy.t
    ; impl : t
    ; vlib : t
    }

  type nonrec t =
    | Modules of t
    | Impl of impl

  let modules m = Modules m
  let with_modules_obj_map = with_obj_map

  let with_obj_map =
    let modules_obj_map = obj_map in
    let obj_map : impl -> Sourced_module.t Module_name.Unique.Map.t =
      let module Map = Module_name.Unique.Map in
      fun t ->
        let { obj_map = _; vlib; impl } = t in
        Map.merge (modules_obj_map vlib) (modules_obj_map impl) ~f:(fun _ vlib impl ->
          match vlib, impl with
          | None, None -> assert false
          | Some (Normal m), None -> Some (Sourced_module.Imported_from_vlib m)
          | None, Some (Normal m) -> Some (Normal m)
          | Some (Normal intf), Some (Normal impl) ->
            Some (Sourced_module.Impl_of_virtual_module { intf; impl })
          | Some (Imported_from_vlib _ | Impl_of_virtual_module _), _
          | _, Some (Imported_from_vlib _ | Impl_of_virtual_module _) -> assert false)
    in
    function
    | Modules t -> Modules (with_modules_obj_map t.modules)
    | Impl t ->
      let obj_map = lazy (obj_map t) in
      Impl { t with obj_map }
  ;;

  let obj_map = function
    | Modules t -> obj_map t
    | Impl { obj_map; _ } -> Lazy.force obj_map
  ;;

  let encode =
    let encode t ~src_dir =
      let open Dune_sexp in
      match t.modules with
      | Singleton m -> List (atom "singleton" :: Module.encode m ~src_dir)
      | Unwrapped m -> List (atom "unwrapped" :: Unwrapped.encode m ~src_dir)
      | Wrapped m -> List (atom "wrapped" :: Wrapped.encode m ~src_dir)
      | Stdlib m -> List (atom "stdlib" :: Stdlib.encode m ~src_dir)
    in
    fun t ~src_dir ->
      match t with
      | Modules m -> encode m ~src_dir
      | Impl { impl; _ } -> encode impl ~src_dir
  ;;

  let singleton m = Modules (with_modules_obj_map (Singleton m))

  let dyn_of_impl { impl; vlib; _ } =
    let open Dyn in
    record [ "impl", to_dyn impl; "vlib", to_dyn vlib ]
  ;;

  let modules_to_dyn = to_dyn

  let to_dyn t =
    let open Dyn in
    match t with
    | Modules t -> variant "Modules" [ modules_to_dyn t ]
    | Impl impl -> variant "Impl" [ dyn_of_impl impl ]
  ;;

  let lib_interface =
    let lib_interface t =
      match t.modules with
      | Singleton m -> Some m
      | Unwrapped _ -> None
      | Wrapped w -> Some (Wrapped.lib_interface w)
      | Stdlib w -> Stdlib.lib_interface w
    in
    function
    | Modules t -> lib_interface t
    | Impl { impl = _; vlib; _ } -> lib_interface vlib
  ;;

  let main_module_name =
    let main_module_name t =
      match t.modules with
      | Singleton m -> Some (Module.name m)
      | Unwrapped _ -> None
      | Wrapped w -> Some w.group.name
      | Stdlib w -> Some w.main_module_name
    in
    function
    | Modules t -> main_module_name t
    | Impl { vlib; impl = _; _ } -> main_module_name vlib
  ;;

  let impl =
    let empty = lazy Module_name.Unique.Map.empty in
    fun impl ~vlib ->
      let modules =
        match impl.modules, vlib.modules with
        | Stdlib _, _ | _, Stdlib _ ->
          Code_error.raise
            "Modules.impl: invalid arguments"
            [ "impl", modules_to_dyn impl; "vlib", modules_to_dyn vlib ]
        | _, _ -> Impl { obj_map = empty; impl; vlib }
      in
      with_obj_map modules
  ;;

  let modules_find t name =
    match t.modules with
    | Singleton m -> Option.some_if (Module.name m = name) m
    | Unwrapped m -> Unwrapped.find m name
    | Stdlib w -> Stdlib.find w name
    | Wrapped w -> Wrapped.find w name
  ;;

  let find t name =
    match t with
    | Modules t -> modules_find t name
    | Impl { impl; vlib; _ } ->
      (match modules_find impl name with
       | Some _ as m -> m
       | None -> modules_find vlib name)
  ;;

  exception Parent_cycle

  let find_dep =
    let from_impl_or_lib = List.map ~f:(fun m -> `Impl_or_lib, m) in
    let find_dep_result =
      List.filter_map ~f:(fun (from, m) ->
        match from with
        | `Impl_or_lib -> Some m
        | `Vlib -> Option.some_if (Module.visibility m = Public) m)
    in
    let raise_parent_cycle = function
      | Ok s -> from_impl_or_lib s
      | Error `Parent_cycle -> raise_notrace Parent_cycle
    in
    let find_dep t ~of_ name : Module.t list =
      if Module.name of_ = name
      then []
      else (
        let result =
          match t.modules with
          | Singleton _ -> modules_find t name |> Option.to_list |> from_impl_or_lib
          | Unwrapped w -> Unwrapped.find_dep w ~of_ name |> raise_parent_cycle
          | Wrapped w -> Wrapped.find_dep w ~of_ name |> raise_parent_cycle
          | Stdlib s -> Stdlib.find_dep s ~of_ name |> Option.to_list |> from_impl_or_lib
        in
        find_dep_result result)
    in
    fun t ~of_ name ->
      try
        Ok
          (match t with
           | Modules t -> find_dep t ~of_ name
           | Impl { vlib; impl; _ } ->
             (match find_dep impl ~of_ name with
              | [] -> find_dep vlib ~of_ name |> List.map ~f:(fun m -> `Vlib, m)
              | xs -> from_impl_or_lib xs)
             |> find_dep_result)
      with
      | Parent_cycle -> Error `Parent_cycle
  ;;

  let singleton_exe m = Modules (make_singleton m Exe)

  let impl_only =
    let impl_only t =
      match t.modules with
      | Stdlib w -> Stdlib.impl_only w
      | Singleton m -> if Module.has ~ml_kind:Impl m then [ m ] else []
      | Unwrapped m ->
        Unwrapped.fold m ~init:[] ~f:(fun v acc ->
          if Module.has v ~ml_kind:Impl then v :: acc else acc)
      | Wrapped w -> Wrapped.impl_only w
    in
    fun t ->
      match t with
      | Modules t -> impl_only t
      | Impl { vlib; impl; _ } -> impl_only impl @ impl_only vlib
  ;;

  let exists =
    let exists t ~f =
      match t.modules with
      | Stdlib w -> Stdlib.exists w ~f
      | Wrapped m -> Wrapped.exists m ~f
      | Singleton m -> f m
      | Unwrapped m -> Unwrapped.exists m ~f
    in
    fun t ~f ->
      match t with
      | Modules t -> exists t ~f
      | Impl { vlib; impl; _ } -> exists vlib ~f || exists impl ~f
  ;;

  let has_impl =
    let has = Module.has ~ml_kind:Impl in
    exists ~f:has
  ;;

  let drop_vlib = function
    | Modules t -> t
    | Impl { vlib = _; impl; _ } -> impl
  ;;

  let fold_no_vlib_with_aliases =
    let group_of_alias t m =
      match t.modules with
      | Wrapped w -> Some (Wrapped.group_of_alias w m)
      | Unwrapped w -> Some (Unwrapped.group_of_alias w m)
      | _ -> None
    in
    let group_of_alias t m =
      match t with
      | Modules t -> group_of_alias t m
      | Impl { vlib; impl; _ } ->
        let vlib = group_of_alias vlib m in
        let impl = group_of_alias impl m in
        (match vlib, impl with
         | None, None -> assert false
         | Some _, None -> vlib
         | None, Some _ -> impl
         | Some vlib, Some impl ->
           let modules =
             Module_name.Map.merge vlib.modules impl.modules ~f:(fun _ vlib impl ->
               match vlib, impl with
               | None, None -> assert false
               | _, Some _ -> impl
               | Some vlib, _ ->
                 let vlib =
                   match (vlib : Group.node) with
                   | Module m -> m
                   | Group g -> Group.lib_interface g
                 in
                 Option.some_if (Module.visibility vlib = Public) vlib
                 |> Option.map ~f:(fun m -> Group.Module m))
           in
           Some { impl with Group.modules })
    in
    fun t ~init ~normal ~alias ->
      t
      |> drop_vlib
      |> fold ~init ~f:(fun m acc ->
        match Module.kind m with
        | Alias _ ->
          (match group_of_alias t m with
           | None ->
             Code_error.raise
               "alias module for group without alias"
               [ "t", to_dyn t; "m", Module.to_dyn m ]
           | Some group -> alias group acc)
        | _ -> normal m acc)
  ;;

  type split_by_lib =
    { vlib : Module.t list
    ; impl : Module.t list
    }

  let split_by_lib t =
    let f m acc = m :: acc in
    let init = [] in
    match t with
    | Impl { vlib; impl; _ } ->
      let vlib = fold vlib ~init ~f in
      let impl = fold impl ~init ~f in
      { vlib; impl }
    | Modules t -> { impl = fold t ~init ~f; vlib = [] }
  ;;

  let compat_for_exn t m =
    match t with
    | Impl _ -> Code_error.raise "wrapped compat not supported for vlib" []
    | Modules t ->
      (match t.modules with
       | Singleton _ | Stdlib _ | Unwrapped _ -> assert false
       | Wrapped { group; _ } ->
         (match Module_name.Map.find group.modules (Module.name m) with
          | None -> assert false
          | Some (Module m) -> m
          | Some (Group g) -> Group.lib_interface g))
  ;;

  let wrapped_compat t =
    match t with
    | Impl _ | Modules { modules = Stdlib _ | Singleton _ | Unwrapped _; _ } ->
      Module_name.Map.empty
    | Modules { modules = Wrapped w; _ } -> w.wrapped_compat
  ;;

  let version_installed t ~src_root ~install_dir =
    let f = Module.version_installed ~src_root ~install_dir in
    let map t =
      let modules =
        match t.modules with
        | Singleton m -> Singleton (f m)
        | Unwrapped m -> Unwrapped (Unwrapped.map ~f m)
        | Stdlib w -> Stdlib (Stdlib.map w ~f)
        | Wrapped w -> Wrapped (Wrapped.map w ~f)
      in
      with_modules_obj_map modules
    in
    match t with
    | Modules t -> Modules (map t)
    | Impl w -> Impl { w with impl = map w.impl }
  ;;

  let entry_modules = function
    | Impl i ->
      Code_error.raise
        "entry_modules: not defined for implementations"
        [ "impl", dyn_of_impl i ]
    | Modules t ->
      List.filter
        ~f:(fun m -> Module.visibility m = Public)
        (match t.modules with
         | Stdlib w -> Stdlib.lib_interface w |> Option.to_list
         | Singleton m -> [ m ]
         | Unwrapped m -> Unwrapped.entry_modules m
         | Wrapped m ->
           (* we assume this is never called for implementations *)
           [ Wrapped.lib_interface m ])
  ;;

  let wrapped = function
    | Modules t -> wrapped t
    | Impl { vlib = _; impl; _ } -> wrapped impl
  ;;

  let alias_for =
    let alias_for t m =
      match t.modules with
      | Singleton _ -> []
      | Unwrapped w -> Unwrapped.alias_for w m
      | Wrapped w -> Wrapped.alias_for w m
      | Stdlib w -> Stdlib.alias_for w m |> Option.to_list
    in
    fun t m ->
      match Module.kind m with
      | Root -> []
      | _ ->
        (match t with
         | Modules t -> alias_for t m
         | Impl { impl; vlib = _; _ } -> alias_for impl m)
  ;;

  let group_interfaces =
    let group_interfaces t m =
      match t.modules with
      | Wrapped w -> Wrapped.group_interfaces w m
      | Singleton w -> [ w ]
      | _ -> []
    in
    fun t m ->
      match t with
      | Modules t -> group_interfaces t m
      | Impl { impl; vlib; _ } -> group_interfaces impl m @ group_interfaces vlib m
  ;;

  let local_open t m =
    alias_for t m
    |> List.map ~f:(fun m ->
      Module.obj_name m |> Module_name.Unique.to_name ~loc:Loc.none)
  ;;

  let is_stdlib_alias t m =
    match t with
    | Modules { modules = Stdlib w; _ } -> w.main_module_name = Module.name m
    | _ -> false
  ;;

  let exit_module t =
    match t with
    | Modules { modules = Stdlib w; _ } -> Stdlib.exit_module w
    | _ -> None
  ;;

  let as_singleton t =
    match t with
    | Modules { modules = Singleton m; _ } -> Some m
    | _ -> None
  ;;

  let canonical_path t (group : Group.t) m =
    let path =
      let path = Module.path m in
      match Module_name.Map.find group.modules (Module.name m) with
      | None | Some (Group.Module _) -> path
      | Some (Group _) ->
        (* The path for group interfaces always duplicates
           the last component.

           For example: foo/foo.ml would has the path [ "Foo"; "Foo" ] *)
        List.remove_last_exn path
    in
    match t with
    | Impl { impl = { modules = Wrapped w; _ }; _ } | Modules { modules = Wrapped w; _ }
      -> w.group.name :: path
    | _ -> Module.path m
  ;;
end
