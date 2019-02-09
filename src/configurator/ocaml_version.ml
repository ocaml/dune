(* Copyright (c) 2017 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
*)

type t = { major: int; minor: int; patch: int option; extra: string option }
let v ?patch ?extra major minor = { major; minor; patch; extra }

let major { major; _ } = major
let minor { minor; _ } = minor
let patch { patch; _ } = patch
let extra { extra; _ } = extra

let to_string ?(sep='+') =
  function
  | {major;minor;patch=None;extra=None} -> Printf.sprintf "%d.%02d" major minor
  | {major;minor;patch=Some patch;extra=None} -> Printf.sprintf "%d.%02d.%d" major minor patch
  | {major;minor;patch=Some patch;extra=Some extra} -> Printf.sprintf "%d.%02d.%d%c%s" major minor patch sep extra
  | {major;minor;patch=None;extra=Some extra} -> Printf.sprintf "%d.%02d%c%s" major minor sep extra

let parse s =
  try Scanf.sscanf s "%d.%d.%d+%s" (fun major minor patch extra -> v ~patch ~extra major minor)
  with End_of_file | Scanf.Scan_failure _ -> begin
      try Scanf.sscanf s "%d.%d+%s" (fun major minor extra -> v ~extra major minor)
      with End_of_file | Scanf.Scan_failure _ -> begin
          try Scanf.sscanf s "%d.%d.%d" (fun major minor patch -> v ~patch major minor)
          with End_of_file | Scanf.Scan_failure _ -> begin
              Scanf.sscanf s "%d.%d" (fun major minor -> v major minor)
            end
        end
    end

let of_string s =
  try Ok (parse s) with
  | _ -> Error (`Msg (Printf.sprintf "Unable to parse OCaml version '%s'" s))

let of_string_exn s =
  try parse s with
  | _ -> raise (Invalid_argument (Printf.sprintf "Unable to parse OCaml version '%s'" s))

let pp ppf v = Format.pp_print_string ppf (to_string v)

let ( ++ ) x fn =
  match x with
  | 0 -> fn ()
  | r -> r

let compare {major; minor; patch; extra} a =
  compare major a.major ++ fun () ->
    compare minor a.minor ++ fun () ->
      compare patch a.patch ++ fun () ->
        compare extra a.extra

let sys_version = of_string_exn Sys.ocaml_version

let with_variant t extra = { t with extra }
let without_variant t = { t with extra=None }
let with_patch t patch = { t with patch }
let without_patch t = { t with patch=None }
let with_just_major_and_minor t = { t with patch=None; extra=None }

module Releases = struct
  let v4_00_0 = of_string_exn "4.00.0"
  let v4_00_1 = of_string_exn "4.00.1"
  let v4_00 = v4_00_1

  let v4_01_0 = of_string_exn "4.01.0"
  let v4_01 = v4_01_0

  let v4_02_0 = of_string_exn "4.02.0"
  let v4_02_1 = of_string_exn "4.02.1"
  let v4_02_2 = of_string_exn "4.02.2"
  let v4_02_3 = of_string_exn "4.02.3"
  let v4_02 = v4_02_3

  let v4_03_0 = of_string_exn "4.03.0"
  let v4_03 = v4_03_0

  let v4_04_0 = of_string_exn "4.04.0"
  let v4_04_1 = of_string_exn "4.04.1"
  let v4_04_2 = of_string_exn "4.04.2"
  let v4_04 = v4_04_2

  let v4_05_0 = of_string_exn "4.05.0"
  let v4_05 = v4_05_0

  let v4_06_0 = of_string_exn "4.06.0"
  let v4_06_1 = of_string_exn "4.06.1"
  let v4_06 = v4_06_1

  let v4_07_0 = of_string_exn "4.07.0"
  let v4_07_1 = of_string_exn "4.07.1"
  let v4_07 = v4_07_1

  let v4_08_0 = of_string_exn "4.08.0"
  let v4_08 = v4_08_0

  let v4_09_0 = of_string_exn "4.09.0"
  let v4_09 = v4_09_0

  let all_patches = [
    v4_00_1; v4_01_0; v4_02_0; v4_02_1; v4_02_2;
    v4_02_3; v4_03_0; v4_04_0; v4_04_1; v4_04_2;
    v4_05_0; v4_06_0; v4_06_1; v4_07_0; v4_07_1;
    v4_08_0; v4_09_0 ]

  let all = [ v4_00; v4_01; v4_02; v4_03; v4_04;
              v4_05; v4_06; v4_07; v4_08; v4_09 ]

  let recent = [ v4_02; v4_03; v4_04; v4_05; v4_06; v4_07 ]

  let latest = v4_07

  let dev = [ v4_08; v4_09 ]

  let is_dev t =
    let t = with_just_major_and_minor t in
    let dev = List.map with_just_major_and_minor dev in
    List.mem t dev

  let recent_with_dev = List.concat [recent; dev]

end

type arch = [`X86_64 | `Aarch64 | `Ppc64le | `Aarch32 ]
let arches = [ `X86_64; `Aarch64; `Ppc64le; `Aarch32 ]

let string_of_arch = function
  | `Aarch64 -> "arm64"
  | `Aarch32 -> "arm32v7"
  | `X86_64 -> "amd64"
  | `Ppc64le -> "ppc64le"

let arch_of_string = function
  | "arm64" | "aarch64" -> Ok `Aarch64
  | "amd64" | "x86_64" -> Ok `X86_64
  | "arm32" | "arm32v7" | "aarch32" -> Ok `Aarch32
  | "ppc64le" -> Ok `Ppc64le
  | arch -> Error (`Msg ("Unknown architecture " ^ arch))

let arch_of_string_exn a =
  match arch_of_string a with
  | Ok a -> a
  | Error (`Msg m) -> raise (Invalid_argument m)

module Since = struct
  let bytes = Releases.v4_03_0

  let arch (a:arch) =
    match a with
    | `Aarch32 -> Releases.v4_06_0
    | `Aarch64 -> Releases.v4_05_0
    | `Ppc64le -> Releases.v4_06_0
    | `X86_64 -> Releases.v4_00_0 (* TODO obviously earlier *)
end

module Has = struct

  let bytes v =
    match compare Since.bytes v with
    |(-1) | 0 -> true
    |_ -> false

  let arch (a:arch) v =
    match compare (Since.arch a) v with
    |(-1) | 0 -> true
    |_ -> false
end

module Configure_options = struct
  type o = [ `Afl | `Flambda | `Default_unsafe_string | `Force_safe_string | `Frame_pointer ]

  let to_description t = 
    match t with
    | `Afl -> "AFL (fuzzing) support"
    | `Flambda -> "flambda inlining"
    | `Default_unsafe_string -> "default to unsafe strings"
    | `Force_safe_string -> "force safe string mode"
    | `Frame_pointer -> "frame pointer"

  let to_string t =
    match t with
    | `Afl -> "afl"
    | `Flambda -> "flambda"
    | `Default_unsafe_string -> "default-unsafe-string"
    | `Force_safe_string -> "force-safe-string"
    | `Frame_pointer -> "fp"

  let to_configure_flag t =
    match t with
    | `Afl -> "-afl-instrument"
    | `Flambda -> "-flambda"
    | `Default_unsafe_string -> "-default-unsafe-string"
    | `Force_safe_string -> "-force-safe-string"
    | `Frame_pointer -> "-with-frame-pointer"

end

let compiler_variants arch {major; minor; _} =
    match major,minor,arch with
    | 4,9,`X86_64 -> [[]]
    | 4,9,_ -> [[]]
    | 4,8,`X86_64 -> [[]; [`Afl]; [`Flambda]; [`Frame_pointer]; [`Frame_pointer;`Flambda]; [`Default_unsafe_string]]
    | 4,8,_ -> [[]; [`Afl]; [`Flambda];[`Default_unsafe_string]]
    | 4,7,_ -> [[]; [`Afl]; [`Flambda]; [`Default_unsafe_string]; [`Force_safe_string]]
    | 4,6,_ -> [[]; [`Afl]; [`Flambda]; [`Default_unsafe_string]; [`Force_safe_string]]
    | 4,5,_ -> [[]; [`Afl]; [`Flambda]]
    | 4,4,_ -> [[]; [`Flambda]]
    | 4,3,_ -> [[]; [`Flambda]]
    | _ -> [[]]

module Sources = struct
  let trunk = Releases.v4_08

  let git_tag ({major; minor; patch; _ } as ov) =
    match major, minor, patch with
    | major, minor, _ when major = trunk.major && minor = trunk.minor -> "trunk"
    | _ -> to_string (with_variant ov None)
end

module Opam = struct

  module V2 = struct
    let name t =
      match t.extra with
      | Some extra when Releases.is_dev t -> Printf.sprintf "ocaml-variants.%s+trunk+%s" (to_string (without_variant t)) extra
      | Some _ -> "ocaml-variants." ^ (to_string t)
      | None when Releases.is_dev t -> Printf.sprintf "ocaml-variants.%s+trunk" (to_string t)
      | None -> "ocaml-base-compiler." ^ (to_string t)

    let variant_switch t vs =
      match vs with
      | [] -> with_variant t None
      | vs ->
        let v = String.concat "+" (List.map Configure_options.to_string vs) in
        with_variant t (Some v)

    let switches arch t =
      compiler_variants arch t |>
      List.map (fun vs -> variant_switch t vs)
  end
end
