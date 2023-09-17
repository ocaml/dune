open Dune_site_private

let path_sep = path_sep

module Location = struct
  type t = string
end

let dirs : (string * Dune_section.t, string) Hashtbl.t = Hashtbl.create 10

(* multi-bindings first is the one with least priority *)

let () =
  match Sys.getenv_opt dune_dir_locations_env_var with
  | None -> ()
  | Some s ->
    (match decode_dune_dir_locations s with
     | None ->
       invalid_arg (Printf.sprintf "Invalid value %s=%S" dune_dir_locations_env_var s)
     | Some entries ->
       List.iter
         (fun { package; section; dir } -> Hashtbl.add dirs (package, section) dir)
         entries)
;;

(* Parse the replacement format described in [artifact_substitution.ml]. *)
let eval s =
  let len = String.length s in
  if s.[0] = '='
  then (
    let colon_pos = String.index_from s 1 ':' in
    let vlen = int_of_string (String.sub s 1 (colon_pos - 1)) in
    (* This [min] is because the value might have been truncated if it was too
       large *)
    let vlen = min vlen (len - colon_pos - 1) in
    Some (String.sub s (colon_pos + 1) vlen))
  else None
[@@inline never]
;;

let get_dir ~package ~section = Hashtbl.find_all dirs (package, section)

module Hardcoded_ocaml_path = struct
  type t =
    | None
    | Relocatable
    | Hardcoded of string list
    | Findlib_config of string

  let t =
    lazy
      (match eval Dune_site_data.hardcoded_ocamlpath with
       | None -> None
       | Some "relocatable" -> Relocatable
       | Some s ->
         let l = String.split_on_char '\000' s in
         (match l with
          | "hardcoded" :: l -> Hardcoded l
          | [ "findlibconfig"; p ] -> Findlib_config p
          | _ -> invalid_arg "dune error: hardcoded_ocamlpath parsing error"))
  ;;
end

let relocatable =
  lazy
    (match Lazy.force Hardcoded_ocaml_path.t with
     | Relocatable -> true
     | _ -> false)
;;

let prefix =
  lazy
    (let path = Sys.executable_name in
     let bin = Filename.dirname path in
     let prefix = Filename.dirname bin in
     prefix)
;;

let relocate_if_needed path =
  if Lazy.force relocatable then Filename.concat (Lazy.force prefix) path else path
;;

let site ~package ~section ~suffix ~encoded =
  let dirs = get_dir ~package ~section in
  let dirs =
    match eval encoded with
    | None -> dirs
    | Some d -> relocate_if_needed d :: dirs
  in
  List.rev_map (fun dir -> Filename.concat dir suffix) dirs
[@@inline never]
;;

let sourceroot local =
  match eval local with
  | Some "" -> None
  | Some _ as x -> x
  | None ->
    (* None if the binary is executed from _build but not by dune, which should
       not happen *)
    Sys.getenv_opt dune_sourceroot_env_var
;;

let ocamlpath =
  lazy
    (let env =
       match Sys.getenv_opt "OCAMLPATH" with
       | None -> []
       | Some x -> String.split_on_char path_sep x
     in
     let static =
       match Lazy.force Hardcoded_ocaml_path.t with
       | Hardcoded_ocaml_path.None ->
         String.split_on_char path_sep (Sys.getenv dune_ocaml_hardcoded_env_var)
       | Hardcoded_ocaml_path.Relocatable -> [ Filename.concat (Lazy.force prefix) "lib" ]
       | Hardcoded_ocaml_path.Hardcoded l -> l
       | Hardcoded_ocaml_path.Findlib_config _ -> assert false
     in
     env @ static)
;;

let stdlib =
  lazy
    (match eval Dune_site_data.stdlib_dir with
     | None -> Sys.getenv dune_ocaml_stdlib_env_var
     | Some s -> s)
;;
