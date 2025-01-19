(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamStd.Op

let log fmt = OpamConsole.log "PACKAGE" fmt
let slog = OpamConsole.slog

module Version = struct

  type version_style =
    | Triple (* E.g "1.2.3" *)
    | V_triple (* E.g "v1.2.3" *)

  let version_style_equal a b = match (a, b) with
  | Triple, Triple | V_triple, V_triple -> true
  | _ -> false

  (* Alternative reprentation for some common patterns of version number that
     can be efficiently compared. This is micro-optimized because solving
     dependencies requires comparing a large number of package versions. *)
  type version_small =
    { int : Int64.t
    ; style : version_style
    ; has_suffix : bool
    }

  type version =
    { raw : string
    ; small : version_small option
    }

  type t = version

  let to_string x = x.raw

  let small_of_string s =
    let parts =
      String.split_on_char '+' s
      |> List.concat_map (String.split_on_char '~')
      |> List.concat_map (String.split_on_char '-')
    in
    let has_suffix = List.length parts > 1 in
    match String.split_on_char '.' (List.nth parts 0) with
    | [] -> None
    | major :: rest ->
        let style, major =
          if String.starts_with major ~prefix:"v"
           then V_triple, String.sub major 1 (String.length major - 1)
           else Triple, major
         in
         let major_opt = int_of_string_opt major in
         let minor_opt, patch_opt = match rest with
         | [] -> Some 0, Some 0
         | minor :: rest ->
             let minor_opt = int_of_string_opt minor in
             let patch_opt = match rest with
               | [] -> Some 0
               | [ patch ] -> int_of_string_opt patch
               | _ ->
                   None
             in
             minor_opt, patch_opt
         in
         match major_opt, minor_opt, patch_opt with
         | Some major, Some minor, Some patch ->
             let max = 65535 in
             if major <= max && minor <= max && patch <= max
             then
               (* If each part of the version number can fit in a 16-bit
                  integer pack them into a single 64-bit int so they can be
                  efficiently compared. *)
               let int =
                 (Int64.shift_left (Int64.of_int major) 32)
                 |> Int64.logor (Int64.shift_left (Int64.of_int minor) 16)
                 |> Int64.logor (Int64.of_int patch)
               in
               Some { style; int; has_suffix }
             else None
         | _ -> None

  let of_string x =
    if String.length x = 0 then failwith "Package version can't be empty";
    String.iter (function
        | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' | '+' | '.' | '~' -> ()
        | c ->
          failwith
            (Printf.sprintf "Invalid character '%c' in package version %S" c x))
      x;
    { raw = x
    ; small = small_of_string x
    }

  let default = of_string "dev"

  let small_compare a b =
    if version_style_equal a.style b.style then
      let c = Int64.compare a.int b.int in
      if c == 0 then
        (* Only consider the suffix if the numeric part of both versions is the same. *)
        match a.has_suffix, b.has_suffix with
        | false, false -> Some 0
        | false, true -> Some (-1)
        | true, false -> Some 1
        | true, true ->
            (* If both versions have suffixes then run the full comparison. *)
            None
      else
        Some c
    else
      None

  let compare a b =
    match a.small, b.small with
    | Some small_a, Some small_b -> (
      match small_compare small_a small_b with
      | Some c -> c
      | None -> OpamVersionCompare.compare a.raw b.raw)
    | _ -> OpamVersionCompare.compare a.raw b.raw

  let equal v1 v2 =
    compare v1 v2 = 0

  let to_json x =
    `String (to_string x)
  let of_json = function
    | `String x -> (try Some (of_string x) with _ -> None)
    | _ -> None

  module O = struct
    type t = version
    let to_string = to_string
    let compare = compare
    let to_json = to_json
    let of_json = of_json
  end

  module Set = OpamStd.Set.Make(O)

  module Map = OpamStd.Map.Make(O)

end

module Name = struct

  type t = string

  let to_string x = x

  let of_string x =
    match
      OpamStd.String.fold_left (fun acc c ->
          if acc = Some false then acc else match c with
            | 'a'..'z' | 'A'..'Z' -> Some true
            | '0'..'9' | '-' | '_' | '+' -> acc
            | _ -> Some false)
        None x
    with
    | Some false ->
      failwith
        (Printf.sprintf "Invalid character in package name %S" x)
    | None ->
      failwith
        (Printf.sprintf "Package name %S should contain at least one letter" x)
    | Some true ->
      x

  let compare = OpamStd.String.compare_case

  let equal n1 n2 =
    compare n1 n2 = 0

  let to_json x = `String x
  let of_json = function
    | `String s -> (try Some (of_string s) with _ -> None)
    | _ -> None

  module O = struct
    type t = string
    let to_string = to_string
    let compare = compare
    let to_json = to_json
    let of_json = of_json
  end

  module Set = OpamStd.Set.Make(O)

  module Map = OpamStd.Map.Make(O)

end

type t = {
  name   : Name.t;
  version: Version.t;
}

let create name version = { name; version }

let name_to_string t = Name.to_string t.name
let version_to_string t = Version.to_string t.version

let name t = t.name

let version t = t.version

let sep = '.'

let of_string_opt s =
  if OpamStd.String.contains_char s ' ' ||
     OpamStd.String.contains_char s '\n' then
    None
  else match OpamStd.String.cut_at s sep with
    | None        -> None
    | Some (n, v) ->
      try Some { name = Name.of_string n; version = Version.of_string v }
      with Failure _ -> None

let of_string s = match of_string_opt s with
  | Some x -> x
  | None   -> failwith "OpamPackage.of_string"

let to_string t =
  match Version.to_string t.version with
  | "" -> Name.to_string t.name
  | _ -> Printf.sprintf "%s%c%s" (Name.to_string t.name) sep (Version.to_string t.version)

let compare nv1 nv2 =
  match Name.compare nv1.name nv2.name with
  | 0 -> Version.compare nv1.version nv2.version
  | i -> i

let hash nv = Hashtbl.hash nv

let equal nv1 nv2 =
  compare nv1 nv2 = 0

let to_json nv =
  `O [ ("name", Name.to_json (name nv));
       ("version", Version.to_json (version nv));
     ]
let of_json = function
  | `O dict ->
    (try
       let open OpamStd.Option.Op in
       Name.of_json (OpamStd.List.assoc String.equal "name" dict)
       >>= fun name ->
       Version.of_json (OpamStd.List.assoc String.equal "version" dict)
       >>= fun version -> Some {name; version}
     with Not_found -> None)
  | _ -> None

module O = struct
  type tmp = t
  type t = tmp
  let compare p1 p2 =
    let r = Name.compare p1.name p2.name in
    if r = 0 then Version.compare p1.version p2.version else r
  let to_string = to_string
  let to_json = to_json
  let of_json = of_json
end

module Set = OpamStd.Set.Make (O)

module Map = OpamStd.Map.Make (O)

let to_map nv =
  Set.fold (fun nv map ->
      let name = name nv in
      let version = version nv in
      try Name.Map.add name
            (Version.Set.add version (Name.Map.find name map)) map
      with Not_found -> Name.Map.add name (Version.Set.singleton version) map
    ) nv Name.Map.empty

let of_map nvm =
  Name.Map.fold (fun n -> Version.Set.fold (fun v -> Set.add (create n v)))
    nvm Set.empty

let keys map =
  Map.fold (fun nv _ set -> Set.add nv set) map Set.empty

(* $DIR/$NAME.$VERSION/ *)
let of_dirname f =
  f
  |> OpamFilename.basename_dir
  |> OpamFilename.Base.to_string
  |> of_string_opt

(* $DIR/$NAME.$VERSION/opam *)
let of_filename f =
  if OpamFilename.basename f = OpamFilename.Base.of_string "opam" then
    of_dirname (OpamFilename.dirname f)
  else if OpamFilename.check_suffix f ".opam" then
    of_string_opt OpamFilename.(Base.to_string (basename (chop_extension f)))
  else
    None

(* $NAME.$VERSION+opam.tar.gz *)
let of_archive f =
  let base = OpamFilename.basename f in
  match OpamStd.String.cut_at (OpamFilename.Base.to_string base) '+' with
  | None       -> None
  | Some (s,_) -> of_string_opt s

let list dir =
  log "list %a" (slog OpamFilename.Dir.to_string) dir;
  if OpamFilename.exists_dir dir then (
    let files = OpamFilename.rec_files dir in
    List.fold_left (fun set f ->
        match of_filename f with
        | None   -> set
        | Some p ->
          if not (Set.mem p set) then Set.add p set
          else
            let suffix = Filename.concat (to_string p) "opam" in
            let files = List.filter (OpamFilename.ends_with suffix) files in
            OpamConsole.error_and_exit `File_error
              "Multiple definition of package %s in %s:\n%s"
              (to_string p) (OpamFilename.Dir.to_string dir)
              (OpamStd.Format.itemize ~bullet:"" OpamFilename.to_string files);
      ) Set.empty files
  ) else
    Set.empty

let prefixes repodir =
  log "prefixes %a" (slog OpamFilename.Dir.to_string) repodir;
  if OpamFilename.exists_dir repodir then (
    let files = OpamFilename.rec_files repodir in
    List.fold_left (fun map f ->
        match of_filename f with
        | None   -> map
        | Some p ->
          let pkgdir = OpamFilename.dirname_dir (OpamFilename.dirname f) in
          let prefix =
            match OpamFilename.remove_prefix_dir repodir pkgdir with
            | "" -> None
            | p  -> Some p
          in
          Map.add p prefix map
      ) Map.empty files
  ) else
    Map.empty

let versions_of_packages nvset =
  Set.fold
    (fun nv vset -> Version.Set.add (version nv) vset)
    nvset
    Version.Set.empty

let has_name nvset n =
  Set.exists (fun nv -> name nv = n) nvset

let names_of_packages nvset =
  Set.fold
    (fun nv vset -> Name.Set.add (name nv) vset)
    nvset
    Name.Set.empty

let package_of_name_aux empty split filter nv n =
  if n = "" then empty else
  let inf = {name = String.sub n 0 (String.length n - 1); version= Version.of_string ""} in
  let sup = {name = n^"\000"; version = Version.of_string ""} in
  let _, _, nv = split inf nv in
  let nv, _, _ = split sup nv in
  filter nv

let packages_of_name nv n =
  package_of_name_aux Set.empty Set.split
    (Set.filter (fun nv -> nv.name = n))
    nv n

let packages_of_name_map nv n =
  package_of_name_aux Map.empty Map.split
    (Map.filter (fun nv _ -> nv.name = n))
    nv n

let package_of_name nvset n =
  Set.choose (packages_of_name nvset n)

let package_of_name_opt nvset n =
  try Some (package_of_name nvset n) with Not_found -> None

let packages_of_names nvset nameset =
  Name.Set.fold
    (fun name acc ->
       Set.union acc (packages_of_name nvset name))
    nameset Set.empty

let versions_of_name packages n =
  versions_of_packages
    (packages_of_name packages n)

let filter_name_out packages name =
  Set.diff packages (packages_of_name packages name)

let max_version set name =
  let versions = versions_of_name set name in
  let version = Version.Set.max_elt versions in
  create name version
