(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2019 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamStd.Option.Op

let command_output c =
  match List.filter (fun s -> String.trim s <> "")
          (OpamSystem.read_command_output c)
  with
  | [""] -> None
  | [s] -> Some s
  | _ -> None
  | exception (OpamSystem.Process_error _ | OpamSystem.Command_not_found _) ->
    None

let norm s = if s = "" then None else Some (String.lowercase_ascii s)

let normalise_arch raw =
  match String.lowercase_ascii raw with
  | "x86" | "i386" | "i486" | "i586" | "i686" -> "x86_32"
  | "x86_64" | "amd64" -> "x86_64"
  | "powerpc" | "ppc" | "ppcle" -> "ppc32"
  | "ppc64" | "ppc64le" -> "ppc64"
  | "aarch64_be" | "aarch64" -> "arm64"
  | a when a = "armv8b" || a = "armv8l" || List.exists (fun prefix -> OpamStd.String.starts_with ~prefix a)
        ["armv5"; "armv6"; "earmv6"; "armv7"; "earmv7"] -> "arm32"
  | s -> s

let poll_arch () =
  let raw = match Sys.os_type with
    | "Unix" | "Cygwin" -> OpamStd.Sys.uname "-m"
    | "Win32" ->
      begin match OpamStubs.getArchitecture () with
      | OpamStubs.AMD64 -> Some "x86_64"
      | ARM -> Some "arm32"
      | ARM64 -> Some "arm64"
      | IA64 -> Some "ia64"
      | Intel -> Some "x86_32"
      | Unknown -> None
      end
    | _ -> None
  in
  match raw with
  | None | Some "" -> None
  | Some a -> Some (normalise_arch a)
let arch = Lazy.from_fun poll_arch

let normalise_os raw =
  match String.lowercase_ascii raw with
  | "darwin" | "osx" -> "macos"
  | s -> s

let poll_os () =
  let raw =
    match Sys.os_type with
    | "Unix" -> OpamStd.Sys.uname "-s"
    | s -> norm s
  in
  match raw with
  | None | Some "" -> None
  | Some s -> Some (normalise_os s)
let os = Lazy.from_fun poll_os

let os_release_field =
  let os_release_file = lazy (
    List.find Sys.file_exists ["/etc/os-release"; "/usr/lib/os-release"] |>
    OpamProcess.read_lines |>
    OpamStd.List.filter_map (fun s ->
        try
          Scanf.sscanf s "%s@= %s" (fun x v ->
              let contents =
                try Scanf.sscanf v "\"%s@\"" (fun s -> s)
                with Scanf.Scan_failure _ | End_of_file -> v
              in
              Some (x, contents))
        with Scanf.Scan_failure _ | End_of_file -> None)
  ) in
  fun f ->
    try Some (OpamStd.List.assoc String.equal f (Lazy.force os_release_file))
    with Not_found -> None

let is_android, android_release =
  let prop = lazy (command_output ["getprop"; "ro.build.version.release"]) in
  (fun () -> Lazy.force prop <> None),
  (fun () -> Lazy.force prop)

let poll_os_distribution () =
  let lazy os = os in
  match os with
  | Some "macos" as macos ->
    if OpamSystem.resolve_command "brew" <> None then Some "homebrew"
    else if OpamSystem.resolve_command "port" <> None then Some "macports"
    else macos
  | Some "linux" as linux ->
    (if is_android () then Some "android" else
     os_release_field "ID" >>= norm >>+ fun () ->
     command_output ["lsb_release"; "-i"; "-s"] >>= norm >>+ fun () ->
     let release_file =
       List.find_opt Sys.file_exists ["/etc/redhat-release";
                                      "/etc/centos-release";
                                      "/etc/gentoo-release";
                                      "/etc/issue"]
     in
     match OpamStd.Option.map OpamProcess.read_lines release_file with
     | None |  Some [] -> linux
     | Some (s::_) ->
       try Scanf.sscanf s " %s " norm
       with Scanf.Scan_failure _ -> linux)
  | Some "win32" ->
    (* If the user provides a Cygwin installation in PATH, by default we'll use
       it. Note that this is _not_ done for MSYS2. *)
    let cygwin =
      OpamSystem.resolve_command "cygcheck"
      >>| Filename.dirname
      |> (fun cygbin -> OpamStd.Sys.is_cygwin_cygcheck ~cygbin)
    in
    if cygwin then Some "cygwin" else os
  | os -> os
let os_distribution = Lazy.from_fun poll_os_distribution

let poll_os_version () =
  let lazy os = os in
  match os with
  | Some "linux" ->
    android_release () >>= norm >>+ fun () ->
    command_output ["lsb_release"; "-s"; "-r"] >>= norm >>+ fun () ->
    os_release_field "VERSION_ID" >>= norm
  | Some "macos" ->
    command_output ["sw_vers"; "-productVersion"] >>= norm
  | Some "win32" ->
    let (major, minor, build, _) = OpamStubs.getWindowsVersion () in
    OpamStd.Option.some @@ Printf.sprintf "%d.%d.%d" major minor build
  | Some "cygwin" ->
    (try
       command_output ["cmd"; "/C"; "ver"] >>= fun s ->
       Scanf.sscanf s "%_s@[ Version %s@]" norm
     with Scanf.Scan_failure _ | End_of_file -> None)
  | Some "freebsd" ->
    OpamStd.Sys.uname "-U" >>= norm
  | _ ->
    OpamStd.Sys.uname "-r" >>= norm
let os_version = Lazy.from_fun poll_os_version

let poll_os_family () =
  let lazy os = os in
  match os with
  | Some "linux" ->
    (os_release_field "ID_LIKE" >>= fun s ->
     Scanf.sscanf s " %s" norm (* first word *))
    ++ Lazy.force os_distribution
  | Some ("freebsd" | "openbsd" | "netbsd" | "dragonfly") -> Some "bsd"
  | Some ("win32" | "cygwin") -> Some "windows"
  | _ -> Lazy.force os_distribution
let os_family = Lazy.from_fun poll_os_family

let variables =
  List.map
    (fun (n, v) ->
       OpamVariable.of_string n,
       OpamCompat.Lazy.map (OpamStd.Option.map (fun v -> OpamTypes.S v)) v)
    [
      "arch", arch;
      "os", os;
      "os-distribution", os_distribution;
      "os-version", os_version;
      "os-family", os_family;
    ]

let cores =
  let v = Lazy.from_fun OpamSystem.cpu_count in
  fun () -> Lazy.force v

(* Exported functions *)
let resolve_or_poll var poll env =
  match OpamVariable.Full.read_from_env (OpamVariable.Full.of_string var) with
  | Some (S c) -> Some c
  | _ ->
    match OpamVariable.Map.find_opt (OpamVariable.of_string var) env with
    | Some (lazy (Some (OpamTypes.S c)), _) -> Some c
    | _ -> Lazy.force poll

let arch = resolve_or_poll "arch" arch
let os = resolve_or_poll "os" os
let os_distribution = resolve_or_poll "os-distribution" os_distribution
let os_version = resolve_or_poll "os-version" os_version
let os_family = resolve_or_poll "os-family" os_family

let to_string env =
  let open OpamStd.Option.Op in
  Printf.sprintf "arch=%s os=%s os-distribution=%s os-version=%s"
    (arch env +! "unknown")
    (os env +! "unknown")
    (os_distribution env +! "unknown")
    (os_version env +! "unknown")
