(** Target types for odoc artifacts *)

open Import

module Doc_mode = struct
  type t =
    | Local_only
    | Full

  let subdir = function
    | Local_only -> ""
    | Full -> "full"
  ;;

  let html_subdir = function
    | Local_only -> "_html"
    | Full -> "_html_full"
  ;;

  let json_subdir = function
    | Local_only -> "_json"
    | Full -> "_json_full"
  ;;

  let all = [ Local_only; Full ]
end

type page =
  { name : string
  ; pkg_libs : Lib.t list
  }

type mod_ =
  { visible : bool
  ; module_name : Module_name.t
  }

type _ t =
  | Lib : Package.Name.t * Lib.t -> mod_ t
  | Private_lib : string * Lib.t -> mod_ t
  | Pkg : Package.Name.t -> page t
  | Toplevel : Doc_mode.t -> page t

type any = Any : 'a t -> any

let compare_any (Any t1) (Any t2) =
  (* Assign a rank to each target type for ordering *)
  let rank : type a. a t -> int = function
    | Pkg _ -> 0
    | Lib _ -> 1
    | Private_lib _ -> 2
    | Toplevel _ -> 3
  in
  let r1 = rank t1 in
  let r2 = rank t2 in
  (* First compare by rank (target type) *)
  match Int.compare r1 r2 with
  | Eq ->
    (* Same rank, so same target type - compare by contents *)
    (match t1, t2 with
     | Pkg p1, Pkg p2 -> Package.Name.compare p1 p2
     | Lib (_, l1), Lib (_, l2) -> Lib_name.compare (Lib.name l1) (Lib.name l2)
     | Private_lib (_, l1), Private_lib (_, l2) ->
       Lib_name.compare (Lib.name l1) (Lib.name l2)
     | Toplevel m1, Toplevel m2 ->
       (* Compare modes: Local_only < Full *)
       (match m1, m2 with
        | Doc_mode.Local_only, Doc_mode.Local_only -> Ordering.Eq
        | Doc_mode.Local_only, Doc_mode.Full -> Ordering.Lt
        | Doc_mode.Full, Doc_mode.Local_only -> Ordering.Gt
        | Doc_mode.Full, Doc_mode.Full -> Ordering.Eq)
     | _ -> assert false (* Ranks are equal, so types must match *))
  | ordering -> ordering
;;

(* Convert a Lib.t to a target using Package_discovery for installed libraries.
   For local libraries, uses Lib_info.package (which is accurate for local libs).
   For installed libraries, uses Package_discovery (which is accurate for installed libs). *)
let target_of_lib pkg_discovery (lib : Lib.t) =
  match Lib.Local.of_lib lib with
  | Some local_lib ->
    (* Local library - check if it has a package using Lib_info *)
    let lib_info = Lib.info lib in
    (match Lib_info.package lib_info with
     | Some pkg -> Memo.return (Lib (pkg, lib))
     | None ->
       (* Private library without a package *)
       let lib_unique_name = Odoc_scope.lib_unique_name local_lib in
       Memo.return (Private_lib (lib_unique_name, lib)))
  | None ->
    (* Installed library - use Package_discovery to get correct package *)
    (match Package_discovery.package_of_library pkg_discovery lib with
     | Some pkg -> Memo.return (Lib (pkg, lib))
     | None ->
       (* Installed library without a package - treat as private lib *)
       let lib_unique_name = Lib_name.to_string (Lib.name lib) in
       Memo.return (Private_lib (lib_unique_name, lib)))
;;
