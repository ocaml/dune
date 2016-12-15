open Import
open Jbuild_types
open Meta
open Build.O

module Pub_name = struct
  type t =
    | Dot of t * string
    | Id  of string

  let parse s =
    match String.split s ~on:'.' with
    | [] -> assert false
    | x :: l ->
      let rec loop acc l =
        match l with
        | [] -> acc
        | x :: l -> loop (Dot (acc, x)) l
      in
      loop (Id x) l

  let rec root = function
    | Dot (t, _) -> root t
    | Id n       -> n

  let to_list =
    let rec loop acc = function
      | Dot (t, n) -> loop (n :: acc) t
      | Id n       -> n :: acc
    in
    fun t -> loop [] t

  let to_string t = String.concat ~sep:"." (to_list t)
end

type item =
  | Lib of Path.t * Pub_name.t * Library.t
  | Obj of Path.t * Pub_name.t * Executables.t

let string_of_deps l =
  String.concat (List.sort l ~cmp:String.compare) ~sep:" "

let rule var predicates action value =
  Rule { var; predicates; action; value }
let requires ?(preds=[]) pkgs =
  rule "requires" preds Set (string_of_deps pkgs)
let requires_add preds pkgs =
  rule "requires" preds Add (string_of_deps pkgs)
let ppx_runtime_deps ?(preds=[]) pkgs =
  rule "ppx_runtime_deps" preds Set (string_of_deps pkgs)
let version     s = rule "version"     []      Set s
let description s = rule "description" []      Set s
let _directory   s = rule "directory"   []      Set s
let archive preds s = rule "archive"   preds Set s
let plugin preds  s = rule "plugin"    preds Set s
let archives ?(preds=[]) name =
  [ archive (preds @ [Pos "byte"  ]) (name ^ ".cma" )
  ; archive (preds @ [Pos "native"]) (name ^ ".cmxa")
  ; plugin  (preds @ [Pos "byte"  ]) (name ^ ".cma" )
  ; plugin  (preds @ [Pos "native"]) (name ^ ".cmxs")
  ]
let exists_if fn = rule "exists_if" [] Set fn

let gen_lib pub_name (lib : Library.t) ~lib_deps ~ppx_runtime_deps:ppx_rt_deps =
  let desc =
    match lib.synopsis with
    | Some s -> s
    | None ->
      match (pub_name : Pub_name.t) with
      | Dot (p, "runtime-lib") -> sprintf "Runtime library for %s" (Pub_name.to_string p)
      | Dot (p, "expander"   ) -> sprintf "Expander for %s"        (Pub_name.to_string p)
      | _ -> ""
  in
  List.concat
    [ [ version "0.1.alpha1"
      ; description desc
      ]
    ; (match lib.kind with
       | Normal ->
         [ requires lib_deps ]
       | Ppx_rewriter | Ppx_type_conv_plugin ->
         match ppx_rt_deps with
         | [] ->
           [ requires lib_deps ]
         | _ ->
           [ requires lib_deps
           ; ppx_runtime_deps ppx_rt_deps
           ; requires_add [Neg "ppx_driver"]                ppx_rt_deps
           ; requires_add [Pos "ppx_driver"; Pos "toploop"] ppx_rt_deps
           ])
    ; archives lib.name
        ~preds:(match lib.kind with
          | Normal -> []
          | Ppx_rewriter | Ppx_type_conv_plugin -> [Pos "ppx_driver"])
    ; [ exists_if (lib.name ^ ".cma") ]
    ; (match lib.kind with
       | Normal -> []
       | Ppx_rewriter ->
         [ rule "ppx" [Neg "ppx_driver"; Neg "custom_ppx"] Set "./as-ppx.exe" ]
       | Ppx_type_conv_plugin ->
         [ rule "ppxopt" [Neg "ppx_driver"; Neg "custom_ppx"] Set
             ("ppx_deriving,package:" ^ Pub_name.to_string pub_name)
         ])
    ; (match lib.js_of_ocaml with
       | None | Some { javascript_files = []; _ } -> []
       | Some { javascript_files = l; _ } ->
         let root = Pub_name.root pub_name in
         [ rule "linkopts" [Pos "javascript"] Set
             (List.map l ~f:(fun fn ->
                sprintf "+%s/%s" root (Filename.basename fn))
              |> String.concat ~sep:" ")
         ]
      )
    ]

let gen_obj _pub_name (exes : Executables.t) ~lib_deps =
  let desc =
    match exes.synopsis with
    | Some s -> s
    | None -> ""
  in
  let obj_files ~ext =
    List.map exes.names ~f:(fun name -> name ^ ext)
    |> String.concat ~sep:" "
  in
  let cmo_files = obj_files ~ext:".cmo" in
  let cmx_files = obj_files ~ext:".cmx" in
  [ version "0.1.alpha1"
  ; description desc
  ; requires lib_deps
  ; archive [Pos "byte"  ] cmo_files
  ; archive [Pos "native"] cmx_files
  ; exists_if cmo_files
  ]

let gen ~package ~stanzas ~lib_deps ~ppx_runtime_deps =
  let items =
    List.concat_map stanzas ~f:(fun (dir, stanzas) ->
      List.filter_map stanzas ~f:(fun stanza ->
        match (stanza : Stanza.t) with
        | Library ({ public_name = Some name; _ } as lib)
          when Findlib.root_package_name name = package ->
          Some (Lib (dir, Pub_name.parse name, lib))
        | Executables ({ object_public_name = Some name; _ } as exes)
          when Findlib.root_package_name name = package ->
          Some (Obj (dir, Pub_name.parse name, exes))
        | _ ->
          None))
  in
  Build.all
    (List.map items ~f:(function
       | Lib (dir, pub_name, lib) ->
         Build.fanout
           (lib_deps ~dir         (Stanza.Library lib))
           (ppx_runtime_deps ~dir (Stanza.Library lib))
         >>^ fun (lib_deps, ppx_runtime_deps) ->
         (pub_name, gen_lib pub_name lib ~lib_deps ~ppx_runtime_deps)
       | Obj (dir, pub_name, exes) ->
         lib_deps ~dir (Stanza.Executables exes)
         >>^ fun lib_deps ->
         (pub_name, gen_obj pub_name exes ~lib_deps)))
  >>^ fun pkgs ->
  let pkgs =
    List.map pkgs ~f:(fun (pn, meta) ->
      match Pub_name.to_list pn with
      | [] -> assert false
      | _package :: path -> (path, meta))
  in
  let pkgs = List.sort pkgs ~cmp:(fun (a, _) (b, _) -> compare a b) in
  let rec loop name pkgs =
    let entries, sub_pkgs =
      List.partition_map pkgs ~f:(function
        | ([]    , entries) -> Inl entries
        | (x :: p, entries) -> Inr (x, (p, entries)))
    in
    let entries = List.concat entries in
    let subs =
      String_map.of_alist_multi sub_pkgs
      |> String_map.bindings
      |> List.map ~f:(fun (name, pkgs) -> Package (loop name pkgs))
    in
    { name
    ; entries = entries @ subs
    }
  in
  loop package pkgs
