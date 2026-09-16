open Import
open Memo.O

(* Encoded representation of a set of library names + scope *)
module Key : sig
  type encoded = Digest.t

  module Decoded : sig
    type t = private
      { pps : Lib_name.t list
      ; project_root : Path.Source.t option
      ; opaque_packages : Package.Name.t Lib_name.Map.t
      }

    val of_libs : context:Context_name.t -> Lib.t list -> t Memo.t
  end

  val encode : Decoded.t -> encoded
  val decode : encoded -> Decoded.t
end = struct
  type encoded = Digest.t

  module Decoded = struct
    type t =
      { pps : Lib_name.t list
      ; project_root : Path.Source.t option
      ; opaque_packages : Package.Name.t Lib_name.Map.t
      }

    let equal x y =
      List.equal Lib_name.equal x.pps y.pps
      && Option.equal Path.Source.equal x.project_root y.project_root
      && Lib_name.Map.equal ~equal:Package.Name.equal x.opaque_packages y.opaque_packages
    ;;

    let to_string { pps; project_root; opaque_packages } =
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
      match project_root with
      | None -> s
      | Some dir ->
        sprintf "%s (in project: %s)" s (Path.Source.to_string_maybe_quoted dir)
    ;;

    let of_libs ~context libs =
      let+ opaque_packages =
        Memo.List.filter_map libs ~f:(fun lib ->
          match Lib_info.package (Lib.info lib) with
          | None -> Memo.return None
          | Some name ->
            let+ package = Opam_package.find context name in
            (match package with
             | Some package
               when Path.is_descendant
                      (Lib_info.src_dir (Lib.info lib))
                      ~of_:(Path.build package.paths.target_dir) ->
               Some (Lib.name lib, name)
             | None | Some _ -> None))
        >>| Lib_name.Map.of_list_reduce ~f:(fun package _ -> package)
      in
      let pps =
        (let compare a b = Lib_name.compare (Lib.name a) (Lib.name b) in
         List.sort libs ~compare)
        |> List.map ~f:Lib.name
      in
      let project =
        List.fold_left libs ~init:None ~f:(fun acc lib ->
          let scope_for_key =
            let info = Lib.info lib in
            let status = Lib_info.status info in
            match status with
            | Private (scope_name, _) -> Some scope_name
            | Installed_private _ | Public _ | Installed _ -> None
          in
          Option.merge acc scope_for_key ~f:(fun a b ->
            assert (Dune_project.equal a b);
            a))
      in
      { pps; project_root = Option.map project ~f:Dune_project.root; opaque_packages }
    ;;
  end

  let reverse_table : (Digest.t, Decoded.t) Table.t = Table.create (module Digest) 128

  let encode ({ Decoded.pps; project_root; opaque_packages } as x) =
    let y =
      if Lib_name.Map.is_empty opaque_packages
      then
        Digest.repr
          Repr.(pair (list Lib_name.repr) (option Path.Source.repr))
          (pps, project_root)
      else
        Digest.repr
          Repr.(
            triple
              (list Lib_name.repr)
              (option Path.Source.repr)
              (list (pair Lib_name.repr Package.Name.repr)))
          (pps, project_root, Lib_name.Map.to_list opaque_packages)
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

let ppx_exe_path (ctx : Build_context.t) ~key =
  Path.Build.relative ctx.build_dir (".ppx/" ^ key ^ "/ppx.exe")
;;

let ppx_driver_exe (ctx : Context.t) libs =
  (* Make sure to compile ppx.exe for the compiling host. See: #2252, #2286 and
     #3698 *)
  let* ctx = Context.host ctx in
  let+ key = Key.Decoded.of_libs ~context:(Context.name ctx) libs in
  let key = Key.encode key |> Digest.to_string in
  ppx_exe_path (Context.build_context ctx) ~key
;;

let get_ppx_exe ctx ~scope pps =
  let open Resolve.Memo.O in
  let* libs = Lib.DB.resolve_pps (Scope.libs scope) pps in
  ppx_driver_exe ctx libs |> Resolve.Memo.lift_memo
;;
