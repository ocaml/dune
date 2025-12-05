open Import
open Memo.O

(* Encoded representation of a set of library names + scope *)
module Key : sig
  type encoded = Digest.t

  module Decoded : sig
    type t = private
      { pps : Lib_name.t list
      ; project_root : Path.Source.t option
      }

    val of_libs : Lib.t list -> t
  end

  val encode : Decoded.t -> encoded
end = struct
  type encoded = Digest.t

  module Decoded = struct
    type t =
      { pps : Lib_name.t list
      ; project_root : Path.Source.t option
      }

    let equal x y =
      List.equal Lib_name.equal x.pps y.pps
      && Option.equal Path.Source.equal x.project_root y.project_root
    ;;

    let to_string { pps; project_root } =
      let s = String.enumerate_and (List.map pps ~f:Lib_name.to_string) in
      match project_root with
      | None -> s
      | Some dir ->
        sprintf "%s (in project: %s)" s (Path.Source.to_string_maybe_quoted dir)
    ;;

    let of_libs libs =
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
            | Installed_private | Public _ | Installed -> None
          in
          Option.merge acc scope_for_key ~f:(fun a b ->
            assert (Dune_project.equal a b);
            a))
      in
      { pps; project_root = Option.map project ~f:Dune_project.root }
    ;;
  end

  let reverse_table : (Digest.t, Decoded.t) Table.t = Table.create (module Digest) 128

  let encode ({ Decoded.pps; project_root } as x) =
    let y = Digest.generic (pps, project_root) in
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
end

let ppx_exe_path (ctx : Build_context.t) ~key =
  Path.Build.relative ctx.build_dir (".ppx/" ^ key ^ "/ppx.exe")
;;

let ppx_driver_exe (ctx : Context.t) libs =
  let key = Digest.to_string (Key.Decoded.of_libs libs |> Key.encode) in
  Context.host ctx >>| Context.build_context >>| ppx_exe_path ~key
;;

let get_ppx_exe ctx ~scope pps =
  let open Resolve.Memo.O in
  let* libs = Lib.DB.resolve_pps (Scope.libs scope) pps in
  ppx_driver_exe ctx libs |> Resolve.Memo.lift_memo
;;
