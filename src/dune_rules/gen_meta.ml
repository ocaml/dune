open Import
open Memo.O
open Meta

module Pub_name = struct
  type t =
    | Dot of t * string
    | Id of string

  let of_list = function
    | [] -> assert false
    | x :: l ->
      let rec loop acc l =
        match l with
        | [] -> acc
        | x :: l -> loop (Dot (acc, x)) l
      in
      loop (Id x) l
  ;;

  let of_lib_name s =
    let pkg, xs = Lib_name.split s in
    of_list (Package.Name.to_string pkg :: xs)
  ;;

  let to_list =
    let rec loop acc = function
      | Dot (t, n) -> loop (n :: acc) t
      | Id n -> n :: acc
    in
    fun t -> loop [] t
  ;;

  let to_string t = String.concat ~sep:"." (to_list t)
end

let string_of_deps deps = Lib_name.Set.to_string_list deps |> String.concat ~sep:" "
let rule var predicates action value = Rule { var; predicates; action; value }
let requires ?(preds = []) pkgs = rule "requires" preds Set (string_of_deps pkgs)
let exports pkgs = rule "exports" [] Set (string_of_deps pkgs)

let ppx_runtime_deps ?(preds = []) pkgs =
  rule "ppx_runtime_deps" preds Set (string_of_deps pkgs)
;;

let description s = rule "description" [] Set s
let directory s = rule "directory" [] Set s
let archive preds s = rule "archive" preds Set s
let plugin preds s = rule "plugin" preds Set s

let archives ?(preds = []) lib =
  let info = Lib.info lib in
  let archives = Lib_info.archives info in
  let plugins = Lib_info.plugins info in
  let make ps = String.concat ~sep:" " (List.map ps ~f:Path.basename) in
  [ archive (preds @ [ Pos "byte" ]) (make archives.byte)
  ; archive (preds @ [ Pos "native" ]) (make archives.native)
  ; plugin (preds @ [ Pos "byte" ]) (make plugins.byte)
  ; plugin (preds @ [ Pos "native" ]) (make plugins.native)
  ]
;;

let gen_lib pub_name lib ~version =
  let info = Lib.info lib in
  let synopsis = Lib_info.synopsis info in
  let kind = Lib_info.kind info in
  let desc =
    match synopsis with
    | Some s -> s
    | None ->
      (* CR-someday jdimino: wut? this looks old *)
      (match (pub_name : Pub_name.t) with
       | Dot (p, "runtime-lib") -> sprintf "Runtime library for %s" (Pub_name.to_string p)
       | Dot (p, "expander") -> sprintf "Expander for %s" (Pub_name.to_string p)
       | _ -> "")
  in
  let preds =
    match kind with
    | Normal -> []
    | Ppx_rewriter _ | Ppx_deriver _ -> [ Pos "ppx_driver" ]
  in
  let name lib =
    let name = Lib.name lib in
    match Lib_info.status (Lib.info lib) with
    | Private (_, Some pkg) ->
      Lib_name.mangled (Package.name pkg) (Lib_name.to_local_exn name)
    | _ -> name
  in
  let to_names = Lib_name.Set.of_list_map ~f:name in
  let* lib_deps = Resolve.Memo.read_memo (Lib.requires lib) >>| to_names in
  let* lib_re_exports = Resolve.Memo.read_memo (Lib.re_exports lib) >>| to_names in
  let* ppx_rt_deps =
    Lib.ppx_runtime_deps lib |> Memo.bind ~f:Resolve.read_memo |> Memo.map ~f:to_names
  in
  let+ ppx_runtime_deps_for_deprecated_method =
    (* For the deprecated method, we need to put all the runtime dependencies of
       the transitive closure.

       We need to do this because [ocamlfind ocamlc -package ppx_foo] will not
       look for the transitive dependencies of [foo], and the runtime
       dependencies might be attached to a dependency of [foo] rather than [foo]
       itself.

       Sigh... *)
    let open Resolve.Memo.O in
    Lib.closure [ lib ] ~linking:false
    >>= Resolve.Memo.List.concat_map ~f:Lib.ppx_runtime_deps
    >>| to_names
    |> Resolve.Memo.read_memo
  in
  List.concat
    [ version
    ; [ description desc; requires ~preds lib_deps ]
    ; (if (match Lib.project lib with
           | None -> true
           | Some project -> Dune_project.dune_version project < (3, 17))
          || Lib_name.Set.is_empty lib_re_exports
       then []
       else [ exports lib_re_exports ])
    ; archives ~preds lib
    ; (if Lib_name.Set.is_empty ppx_rt_deps
       then []
       else
         [ Comment "This is what dune uses to find out the runtime dependencies of"
         ; Comment "a preprocessor"
         ; ppx_runtime_deps ppx_rt_deps
         ])
    ; (match kind with
       | Normal -> []
       | Ppx_rewriter _ | Ppx_deriver _ ->
         (* Deprecated ppx method support *)
         let no_ppx_driver = Neg "ppx_driver"
         and no_custom_ppx = Neg "custom_ppx" in
         List.concat
           [ [ Comment
                 "This line makes things transparent for people mixing preprocessors"
             ; Comment "and normal dependencies"
             ; requires ~preds:[ no_ppx_driver ] ppx_runtime_deps_for_deprecated_method
             ]
           ; (match kind with
              | Normal -> assert false
              | Ppx_rewriter _ ->
                [ rule "ppx" [ no_ppx_driver; no_custom_ppx ] Set "./ppx.exe --as-ppx"
                ; rule "library_kind" [] Set "ppx_rewriter"
                ]
              | Ppx_deriver _ ->
                [ rule "requires" [ no_ppx_driver; no_custom_ppx ] Add "ppx_deriving"
                ; rule
                    "ppxopt"
                    [ no_ppx_driver; no_custom_ppx ]
                    Set
                    ("ppx_deriving,package:" ^ Pub_name.to_string pub_name)
                ; rule "library_kind" [] Set "ppx_deriver"
                ])
           ])
    ; (match Lib_info.jsoo_runtime info with
       | [] -> []
       | l ->
         let l = List.map l ~f:Path.basename in
         [ rule "jsoo_runtime" [] Set (String.concat l ~sep:" ") ])
    ; (match Lib_info.wasmoo_runtime info with
       | [] -> []
       | l ->
         let l = List.map l ~f:Path.basename in
         [ rule "wasmoo_runtime" [] Set (String.concat l ~sep:" ") ])
    ]
;;

let gen ~(package : Package.t) ~add_directory_entry entries =
  let open Memo.O in
  let version =
    match Package.version package with
    | None -> []
    | Some s -> [ rule "version" [] Set (Package_version.to_string s) ]
  in
  let+ pkgs =
    Memo.parallel_map entries ~f:(fun (e : Scope.DB.Lib_entry.t) ->
      match e with
      | Library lib ->
        let info = Lib.Local.info lib in
        let pub_name =
          let name = Lib_info.name info in
          Pub_name.of_lib_name name
        in
        (match Pub_name.to_list pub_name with
         | [] -> assert false
         | package :: path ->
           let pub_name =
             match Lib_info.status info with
             | Private (_, None) ->
               (* Not possible b/c we wouldn't be generating a META file for a
                  private library without a package. *)
               assert false
             | Private (_, Some pkg) ->
               assert (path = []);
               let path =
                 Lib_name.Local.mangled_path_under_package
                   (Lib_name.Local.of_string package)
               in
               let pub_name =
                 let name = Package.name pkg in
                 Pub_name.of_list (Package.Name.to_string name :: path)
               in
               pub_name
             | _ -> pub_name
           in
           let+ entries = gen_lib pub_name (Lib.Local.to_lib lib) ~version in
           pub_name, entries)
      | Deprecated_library_name
          { old_name = old_public_name, _; new_public_name = _, new_public_name; _ } ->
        let deps = Lib_name.Set.singleton new_public_name in
        Memo.return
          ( Pub_name.of_lib_name (Public_lib.name old_public_name)
          , version @ [ requires deps; exports deps ] ))
  in
  let pkgs =
    List.map pkgs ~f:(fun (pn, meta) ->
      match Pub_name.to_list pn with
      | [] -> assert false
      | _package :: path -> path, meta)
  in
  let pkgs =
    List.sort pkgs ~compare:(fun (a, _) (b, _) ->
      List.compare ~compare:String.compare a b)
  in
  let rec loop name pkgs =
    let entries, sub_pkgs =
      List.partition_map pkgs ~f:(function
        | [], entries -> Left entries
        | x :: p, entries -> Right (x, (p, entries)))
    in
    let entries = List.concat entries in
    let subs =
      String.Map.of_list_multi sub_pkgs
      |> String.Map.to_list_map ~f:(fun name pkgs ->
        let pkg = loop name pkgs in
        let pkg =
          if add_directory_entry
          then { pkg with entries = directory name :: pkg.entries }
          else pkg
        in
        Package pkg)
    in
    { name = Some (Lib_name.parse_string_exn (Loc.none, name)); entries = entries @ subs }
  in
  loop (Package.Name.to_string (Package.name package)) pkgs
;;
