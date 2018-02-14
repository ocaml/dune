open Import

type t =
  { info : Jbuild.Scope_info.t
  ; db   : Lib.DB.t
  }

let root t = t.info.root
let name t = t.info.name
let info t = t.info
let libs t = t.db

module DB = struct
  type scope = t

  module Scope_name_map = Map.Make(Jbuild.Scope_info.Name)

  type t =
    { by_dir  : (Path.t, scope) Hashtbl.t
    ; by_name : scope Scope_name_map.t
    ; context : string
    }

  let find_by_dir t dir =
    let rec loop d =
      match Hashtbl.find t.by_dir d with
      | Some scope -> scope
      | None ->
        if Path.is_root d || not (Path.is_local d) then
          Sexp.code_error "Scope.DB.find_by_dir got an invalid path"
            [ "dir"    , Path.sexp_of_t dir
            ; "context", Sexp.To_sexp.atom t.context
            ];
        let scope = loop (Path.parent d) in
        Hashtbl.add t.by_dir ~key:d ~data:scope;
        scope
    in
    loop dir

  let find_by_name t name =
    match Scope_name_map.find name t.by_name with
    | Some x -> x
    | None ->
      Sexp.code_error "Scope.DB.find_by_name"
        [ "name"   , Sexp.To_sexp.(option atom) name
        ; "context", Sexp.To_sexp.atom t.context
        ]

  let create ~scopes ~context ~installed_libs internal_libs =
    let scopes_info_by_name =
      List.map scopes ~f:(fun (scope : Jbuild.Scope_info.t) ->
        (scope.name, scope))
      |> Scope_name_map.of_alist
      |> function
      | Ok x -> x
      | Error (_name, scope1, scope2) ->
        let to_sexp (scope : Jbuild.Scope_info.t) =
          Sexp.To_sexp.(pair (option atom) Path.sexp_of_t)
            (scope.name, scope.root)
        in
        Sexp.code_error "Scope.DB.create got two scopes with the same name"
          [ "scope1", to_sexp scope1
          ; "scope2", to_sexp scope2
          ]
    in
    let libs_by_scope_name =
      List.map internal_libs ~f:(fun (dir, (lib : Jbuild.Library.t)) ->
        (lib.scope_name, (dir, lib)))
      |> Scope_name_map.of_alist_multi
    in
    let by_name_cell = ref Scope_name_map.empty in
    let public_libs =
      let public_libs =
        List.filter_map internal_libs ~f:(fun (_dir, lib) ->
          match lib.public with
          | None -> None
          | Some p -> Some (p.name, lib.scope_name))
        |> String_map.of_alist_exn
      in
      Lib.DB.create ()
        ~parent:installed_libs
        ~resolve:(fun name ->
          match String_map.find name public_libs with
          | None -> Error Not_found
          | Some scope_name ->
            let scope =
              Option.value_exn (Scope_name_map.find scope_name !by_name_cell)
            in
            match Lib.DB.find scope.db name with
            | Error _ as res -> res
            | Ok t -> Ok (Proxy t))
        ~all:(fun () -> String_map.keys public_libs)
    in
    let by_name =
      Scope_name_map.merge scopes_info_by_name libs_by_scope_name
        ~f:(fun _name info libs ->
          let info = Option.value_exn info         in
          let libs = Option.value libs ~default:[] in
          let db =
            Lib.DB.create_from_library_stanzas libs ~parent:public_libs
          in
          Some { info; db })
    in
    by_name_cell := by_name;
    let by_dir = Hashtbl.create 1024 in
    Scope_name_map.iter by_name ~f:(fun ~key:_name ~data:scope ->
      Hashtbl.add by_dir ~key:scope.info.root ~data:scope);
    ({ by_name; by_dir; context }, public_libs)
end
