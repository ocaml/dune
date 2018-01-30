open Import
open Jbuild

type scope =
  { mutable libs : Lib.Internal.t String_map.t
  ; scope        : Scope.t
  }

type t =
  { findlib                  : Findlib.t
  ; (* This include both libraries from the current workspace and external ones *)
    by_public_name           : (string, Lib.t) Hashtbl.t
  ; (* This is to implement the scoping described in the manual *)
    by_internal_name         : (Path.t, scope) Hashtbl.t
  ; (* This is to filter out libraries that are not installable because of missing
       dependencies *)
    instalable_internal_libs : Lib.Internal.t String_map.t
  ; local_public_libs        : Path.t String_map.t
  ; anonymous_root           : Path.t
  ; by_scope_name            : (string, scope) Hashtbl.t
  }

let internal_name_scope t ~dir =
  let rec loop d =
    match Hashtbl.find t.by_internal_name d with
    | Some scope -> scope
    | None ->
      if Path.is_root d || not (Path.is_local d) then (
        Sexp.code_error "Lib_db.Scope.internal_name_scope got an invalid path"
          [ "dir", Path.sexp_of_t dir
          ; "t.anonymous_root", Path.sexp_of_t t.anonymous_root ]
      );
      let scope = loop (Path.parent d) in
      Hashtbl.add t.by_internal_name ~key:d ~data:scope;
      scope in
  loop dir

type 'a with_required_by =
  { required_by: string list
  ; data: 'a
  }

type resolved_select =
  { src_fn : string
  ; dst_fn : string
  }

module Scope = struct
  type nonrec t =
    { scope : scope
    ; lib_db : t
    }

  let find_exn (t : t with_required_by) name =
    match String_map.find name t.data.scope.libs with
    | Some l -> Lib.Internal l
    | None ->
      Hashtbl.find_or_add t.data.lib_db.by_public_name name
        ~f:(fun name ->
          External (Findlib.find_exn t.data.lib_db.findlib name
                      ~required_by:t.required_by))

  let find t name =
    match find_exn t name with
    | exception (Findlib.Findlib _) -> None
    | x -> Some x

  let find_internal' t name =
    match String_map.find name t.scope.libs with
    | Some _ as some -> some
    | None ->
      match Hashtbl.find t.lib_db.by_public_name name with
      | Some (Internal x) -> Some x
      | _ -> None

  let find_internal t name = find_internal' t.data name

  let lib_is_available (t : t with_required_by) name =
    match find_internal t name with
    | Some (_, lib) -> String_map.mem lib.name t.data.lib_db.instalable_internal_libs
    | None -> Findlib.available t.data.lib_db.findlib name ~required_by:t.required_by

  let choice_is_possible t { Lib_dep.required; forbidden; _ } =
    String_set.for_all required  ~f:(fun name ->      lib_is_available t name ) &&
    String_set.for_all forbidden ~f:(fun name -> not (lib_is_available t name))

  let dep_is_available t dep =
    match (dep : Lib_dep.t) with
    | Direct s -> lib_is_available t s
    | Select { choices; _ } -> List.exists choices ~f:(choice_is_possible t)

  let interpret_lib_dep (t : t with_required_by) lib_dep =
    match lib_dep with
    | Lib_dep.Direct name -> begin
        match find_exn t name with
        | x -> Inl [x]
        | exception _ ->
          (* Call [find] again to get a proper backtrace *)
          Inr { fail = fun () ->
            ignore (find_exn t name : Lib.t);
            assert false }
      end
    | Select { choices; loc; _ } ->
      match
        List.find_map choices ~f:(fun { required; forbidden; _ } ->
          if String_set.exists forbidden ~f:(lib_is_available t) then
            None
          else
            match
              List.map (String_set.elements required) ~f:(find_exn t)
            with
            | l           -> Some l
            | exception (Findlib.Findlib _) -> None)
      with
      | Some l -> Inl l
      | None ->
        Inr { fail = fun () ->
          Loc.fail loc "No solution found for this select form"
        }

  let interpret_lib_deps t lib_deps =
    let libs, failures =
      List.partition_map lib_deps ~f:(interpret_lib_dep t)
    in
    let internals, externals =
      List.partition_map (List.concat libs) ~f:(function
        | Internal x -> Inl x
        | External x -> Inr x)
    in
    (internals, externals,
     match failures with
     | [] -> None
     | f :: _ -> Some f)

  let best_lib_dep_names_exn t lib_deps =
    List.concat_map lib_deps ~f:(fun lib_dep ->
      match interpret_lib_dep t lib_dep with
      | Inl libs -> List.map libs ~f:Lib.best_name
      | Inr fail -> fail.fail ())

  let resolve_selects t lib_deps =
    List.filter_map lib_deps ~f:(function
      | Lib_dep.Direct _ -> None
      | Select { result_fn; choices; _ } ->
        let src_fn =
          match List.find choices ~f:(choice_is_possible t) with
          | Some c -> c.file
          | None -> "no solution found"
        in
        Some { dst_fn = result_fn; src_fn })

  let root t = t.scope.scope.root
  let name t =
    Option.value ~default:"" t.scope.scope.name

  let resolve t =
    (* TODO do something with required_by here *)
    Jbuild.Scope.resolve t.data.scope.scope

  let required_in_jbuild t ~jbuild_dir =
    { required_by = [Utils.jbuild_name_in ~dir:jbuild_dir]
    ; data = t }

  let find_scope t ~dir =
    { lib_db = t
    ; scope = internal_name_scope t ~dir
    }

  let find_scope' t ~dir =
    let scope = find_scope t ~dir in
    required_in_jbuild scope ~jbuild_dir:dir

  (* Fold the transitive closure, not necessarily in topological order *)
  let fold_transitive_closure scope ~deep_traverse_externals lib_deps ~init ~f =
    let seen = ref String_set.empty in
    let rec loop scope acc lib_dep =
      match interpret_lib_dep scope lib_dep with
      | Inr fail -> fail.fail ()
      | Inl libs -> List.fold_left libs ~init:acc ~f:process
    and process acc (lib : Lib.t) =
      let unique_id =
        match lib with
        | External pkg -> pkg.name
        | Internal (dir, lib) ->
          match lib.public with
          | Some p -> p.name
          | None -> Path.to_string dir ^ "\000" ^ lib.name
      in
      if String_set.mem unique_id !seen then
        acc
      else begin
        seen := String_set.add unique_id !seen;
        let acc = f lib acc in
        match lib with
        | Internal (dir, lib) ->
          let scope = find_scope' scope.data.lib_db ~dir in
          List.fold_left lib.buildable.libraries ~init:acc ~f:(loop scope)
        | External pkg ->
          if deep_traverse_externals then
            List.fold_left pkg.requires ~init:acc ~f:(fun acc pkg ->
              process acc (External pkg))
          else begin
            seen :=
              String_set.union !seen
                (String_set.of_list
                   (List.map pkg.requires ~f:(fun p -> p.Findlib.name)));
            acc
          end
      end
    in
    List.fold_left lib_deps ~init ~f:(loop scope)

  let all_ppx_runtime_deps_exn scope lib_deps =
    (* The [ppx_runtime_deps] of [Findlib.package] already holds the transitive closure. *)
    let deep_traverse_externals = false in
    fold_transitive_closure scope ~deep_traverse_externals lib_deps
      ~init:String_set.empty ~f:(fun lib acc ->
        let rt_deps =
          match lib with
          | Internal (dir, lib) ->
            let scope = lazy (find_scope' scope.data.lib_db ~dir) in
            List.map lib.ppx_runtime_libraries ~f:(fun name ->
              Lib.best_name (find_exn (Lazy.force scope) name))
          | External pkg ->
            List.map pkg.ppx_runtime_deps ~f:(fun p -> p.Findlib.name)
        in
        String_set.union acc (String_set.of_list rt_deps))
end

let find_scope = Scope.find_scope
let find_scope' = Scope.find_scope'

let local_public_libs t = t.local_public_libs

module Local_closure = Top_closure.Make(String)(struct
    type graph = t
    type t = Lib.Internal.t
    let key ((_, lib) : t) = lib.name
    let deps ((dir, lib) : Lib.Internal.t) graph =
      let scope = find_scope' graph ~dir in
      List.concat_map lib.buildable.libraries ~f:(fun dep ->
        List.filter_map (Lib_dep.to_lib_names dep) ~f:(Scope.find_internal scope)) @
      List.filter_map lib.ppx_runtime_libraries ~f:(fun dep ->
        Scope.find_internal scope dep)
  end)

let top_sort_internals t ~internal_libraries =
  match Local_closure.top_closure t internal_libraries with
  | Ok l -> l
  | Error cycle ->
    die "dependency cycle between libraries:\n   %s"
      (List.map cycle ~f:(fun lib -> Lib.describe (Internal lib))
       |> String.concat ~sep:"\n-> ")

let compute_instalable_internal_libs t ~internal_libraries =
  List.fold_left (top_sort_internals t ~internal_libraries) ~init:t
    ~f:(fun t (dir, lib) ->
      let scope = find_scope' t ~dir in
      if not lib.Library.optional ||
         (List.for_all (Library.all_lib_deps lib) ~f:(Scope.dep_is_available scope) &&
          List.for_all lib.ppx_runtime_libraries  ~f:(Scope.lib_is_available scope))
      then
        { t with
          instalable_internal_libs =
            String_map.add t.instalable_internal_libs
              ~key:lib.name ~data:(dir, lib)
        }
      else
        t)

let create findlib ~scopes ~root internal_libraries =
  let local_public_libs =
    List.fold_left internal_libraries ~init:String_map.empty ~f:(fun acc (dir, lib) ->
      match lib.Library.public with
      | None -> acc
      | Some { name; _ } -> String_map.add acc ~key:name ~data:dir)
  in
  let t =
    { findlib
    ; by_public_name   = Hashtbl.create 1024
    ; by_internal_name = Hashtbl.create 1024
    ; instalable_internal_libs = String_map.empty
    ; local_public_libs
    ; anonymous_root = root
    ; by_scope_name = Hashtbl.create 1024
    }
  in
  (* Initializes the scopes, including [Path.root] so that when there are no <pkg>.opam
     files in parent directories, the scope is the whole workspace. *)
  List.iter scopes ~f:(fun (scope : Jbuild.Scope.t) ->
    let lib_scope = { libs = String_map.empty; scope } in
    Option.iter scope.name ~f:(fun name ->
      assert (name <> "");
      assert (not (Hashtbl.mem t.by_scope_name name));
      Hashtbl.add t.by_scope_name ~key:name ~data:lib_scope;
    );
    Hashtbl.add t.by_internal_name ~key:scope.root ~data:lib_scope
  );
  let anon_scope = internal_name_scope t ~dir:t.anonymous_root in
  Hashtbl.add t.by_scope_name ~key:"" ~data:anon_scope;
  List.iter internal_libraries ~f:(fun ((dir, lib) as internal) ->
    let scope = internal_name_scope t ~dir in
    scope.libs <- String_map.add scope.libs ~key:lib.Library.name ~data:internal;
    Option.iter lib.public ~f:(fun { name; _ } ->
      match Hashtbl.find t.by_public_name name with
      | None
      | Some (External _) ->
        Hashtbl.add t.by_public_name ~key:name ~data:(Internal internal)
      | Some (Internal dup) ->
        let internal_path (path, _) = Path.relative path "jbuild" in
        die "Libraries with identical public names %s defined in %a and %a."
          name Path.pp (internal_path internal) Path.pp (internal_path dup)
    ));
  compute_instalable_internal_libs t ~internal_libraries

let internal_libs_without_non_installable_optional_ones t =
  String_map.values t.instalable_internal_libs

let unique_library_name t (lib : Lib.t) =
  match lib with
  | External pkg -> pkg.name
  | Internal (dir, lib) ->
    match lib.public with
    | Some x -> x.name
    | None ->
      let scope = internal_name_scope t ~dir in
      match scope.scope.name with
      | None -> lib.name ^ "@"
      | Some s -> lib.name ^ "@" ^ s

let external_scope t =
  { Scope.
    lib_db = t
  ; scope =
      { libs = String_map.empty
      ; scope = Jbuild.Scope.empty
      }
  }

let anonymous_scope t =
  { Scope.
    lib_db = t
  ; scope = internal_name_scope t ~dir:t.anonymous_root
  }

let find_scope_by_name_exn t ~name =
  match Hashtbl.find t.by_scope_name name with
  | None -> die "Invalid scope '%s'" name
  | Some scope -> { Scope.scope ; lib_db = t }
