open Import
open Memo.O

(* Encoded representation of a set of library names + scope *)
module Key : sig
  type encoded = Digest.t

  module Decoded : sig
    type t = private
      { pps : Lib_name.t list
      ; scope_dir : Path.Source.t
      ; opaque_packages : Package.Name.t Lib_name.Map.t
      }

    val of_libs : context:Context_name.t -> Lib.t list -> scope:Scope.t -> t Memo.t
  end

  val encode : Decoded.t -> encoded
  val decode : encoded -> Decoded.t
end = struct
  type encoded = Digest.t

  module Decoded = struct
    type t =
      { pps : Lib_name.t list
      ; scope_dir : Path.Source.t
      ; opaque_packages : Package.Name.t Lib_name.Map.t
      }

    let equal x y =
      List.equal Lib_name.equal x.pps y.pps
      && Path.Source.equal x.scope_dir y.scope_dir
      && Lib_name.Map.equal ~equal:Package.Name.equal x.opaque_packages y.opaque_packages
    ;;

    let to_string { pps; scope_dir; opaque_packages } =
      let s =
        List.map pps ~f:(fun name ->
          match Lib_name.Map.find opaque_packages name with
          | None -> Lib_name.to_string name
          | Some package ->
            sprintf
              "%s (package %s)"
              (Lib_name.to_string name)
              (Package.Name.to_string package))
        |> String.enumerate_and
      in
      sprintf "%s (in scope: %s)" s (Path.Source.to_string_maybe_quoted scope_dir)
    ;;

    let of_libs ~context libs ~scope:consumer_scope =
      let* opaque_packages =
        Memo.List.filter_map libs ~f:(fun lib ->
          match Lib_info.package (Lib.info lib) with
          | None -> Memo.return None
          | Some name ->
            let+ package = Pkg_rules.find_opaque context name in
            (match package with
             | Some (root, _)
               when Path.is_descendant
                      (Lib_info.src_dir (Lib.info lib))
                      ~of_:(Path.build root) -> Some (Lib.name lib, name)
             | None | Some _ -> None))
        >>| Lib_name.Map.of_list_reduce ~f:(fun package _ -> package)
      in
      let pps =
        (let compare a b = Lib_name.compare (Lib.name a) (Lib.name b) in
         List.sort libs ~compare)
        |> List.map ~f:Lib.name
      in
      let+ scope_dir =
        match Lib.L.project_root libs with
        | Some dir -> Memo.return (Path.Source.of_local dir)
        | None ->
          let+ loaded_project =
            Partitions.find_loaded_project ~dir:(Scope.root consumer_scope)
          in
          (match Loaded_project.owner loaded_project with
           | None -> Path.Source.root
           | Some _ -> Path.Source.of_local (Loaded_project.source_root loaded_project))
      in
      { pps; scope_dir; opaque_packages }
    ;;
  end

  let reverse_table : (Digest.t, Decoded.t) Table.t = Table.create (module Digest) 128

  let encode ({ Decoded.pps; scope_dir; opaque_packages } as x) =
    let y =
      Digest.repr
        Repr.(
          triple
            (list Lib_name.repr)
            Path.Source.repr
            (list (pair Lib_name.repr Package.Name.repr)))
        (pps, scope_dir, Lib_name.Map.to_list opaque_packages)
    in
    match Table.find reverse_table y with
    | None ->
      Table.set reverse_table y x;
      y
    | Some x' ->
      if Decoded.equal x x'
      then y
      else
        User_error.raise
          [ Pp.textf "Hash collision between set of ppx drivers:"
          ; Pp.textf "- cache : %s" (Decoded.to_string x')
          ; Pp.textf "- fetch : %s" (Decoded.to_string x)
          ]
  ;;

  let decode y =
    match Table.find reverse_table y with
    | Some x -> x
    | None ->
      User_error.raise
        [ Pp.textf
            "I don't know what ppx rewriters set %s correspond to."
            (Digest.to_string y)
        ]
  ;;
end

let ppx_exe_path root ~key = Path.Build.relative root (".ppx/" ^ key ^ "/ppx.exe")

let ppx_driver_exe (ctx : Context.t) ~scope libs =
  (* Make sure to compile ppx.exe for the compiling host. See: #2252, #2286 and
     #3698 *)
  let* host = Context.host ctx in
  let* decoded = Key.Decoded.of_libs ~context:(Context.name host) libs ~scope in
  let key = Digest.to_string (Key.encode decoded) in
  let+ loaded_project =
    Partitions.find_loaded_project
      ~dir:(Path.Build.append_source (Context.build_dir host) decoded.scope_dir)
  in
  ppx_exe_path (Loaded_project.partition_root loaded_project) ~key
;;

let get_ppx_exe ctx ~scope pps =
  let open Resolve.Memo.O in
  let* libs = Lib.DB.resolve_pps (Scope.libs scope) pps in
  ppx_driver_exe ctx ~scope libs |> Resolve.Memo.lift_memo
;;
