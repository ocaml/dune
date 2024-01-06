open Import
open Fiber.O

let norm = function
  | "" -> None
  | s -> Some (String.lowercase s)
;;

let path_of_string s = s |> Path.External.of_string |> Path.external_

let normalise_arch raw =
  match String.lowercase_ascii raw with
  | "x86" | "i386" | "i486" | "i586" | "i686" -> "x86_32"
  | "x86_64" | "amd64" -> "x86_64"
  | "powerpc" | "ppc" | "ppcle" -> "ppc32"
  | "ppc64" | "ppc64le" -> "ppc64"
  | "aarch64_be" | "aarch64" -> "arm64"
  | a
    when a = "armv8b"
         || a = "armv8l"
         || List.exists
              ~f:(fun prefix -> String.is_prefix ~prefix a)
              [ "armv5"; "armv6"; "earmv6"; "armv7"; "earmv7" ] -> "arm32"
  | s -> s
;;

let normalise_os raw =
  match String.lowercase raw with
  | "darwin" | "osx" -> "macos"
  | s -> s
;;

let run_capture_line ~path ~prog ~args =
  match Bin.which ~path prog with
  | None -> Fiber.return None
  | Some prog -> Process.run_capture_line ~display:Quiet Strict prog args >>| norm
;;

let uname ~path args = run_capture_line ~path ~prog:"uname" ~args
let lsb_release ~path args = run_capture_line ~path ~prog:"lsb_release" ~args

let arch ~path =
  (match Sys.os_type with
   | "Unix" | "Cygwin" -> uname ~path [ "-m" ]
   | "Win32" ->
     Fiber.return
     @@
       (match OpamStubs.getArchitecture () with
       | OpamStubs.AMD64 -> Some "x86_64"
       | ARM -> Some "arm32"
       | ARM64 -> Some "arm64"
       | IA64 -> Some "ia64"
       | Intel -> Some "x86_32"
       | Unknown -> None)
   | _ -> Fiber.return None)
  >>| function
  | None | Some "" -> None
  | Some a -> Some (normalise_arch a)
;;

let os ~path =
  (match Sys.os_type with
   | "Unix" -> uname ~path [ "-s" ]
   | s -> Fiber.return (norm s))
  >>| function
  | None | Some "" -> None
  | Some s -> Some (normalise_os s)
;;

let android_release ~path =
  run_capture_line ~path ~prog:"getprop" ~args:[ "ro.build.version.release" ]
;;

let is_android ~path =
  let+ prop = android_release ~path in
  prop <> None
;;

let os_release_field field =
  match
    [ "/etc/os-release"; "/usr/lib/os-release" ]
    |> List.map ~f:path_of_string
    |> List.find ~f:Path.exists
  with
  | None -> Fiber.return None
  | Some release_file ->
    let mappings =
      Io.lines_of_file release_file
      |> List.filter_map ~f:(fun line ->
        match Scanf.sscanf line "%s@= %s" (fun k v -> k, v) with
        | Error _ -> None
        | Ok (key, v) ->
          Some
            ( key
            , match Scanf.sscanf v "\"%s@\"" Fun.id with
              | Error _ -> v
              | Ok contents -> contents ))
    in
    Fiber.return @@ List.assoc mappings field
;;

let os_version ~path =
  os ~path
  >>= function
  | Some "linux" ->
    android_release ~path
    >>= (function
     | Some android -> Fiber.return @@ norm android
     | None ->
       lsb_release ~path [ "-s"; "-r" ]
       >>= (function
        | Some lsb -> Fiber.return @@ norm lsb
        | None -> os_release_field "VERSION_ID" >>| Option.bind ~f:norm))
  | Some "macos" ->
    run_capture_line ~path ~prog:"sw_vers" ~args:[ "-productVersion" ]
    >>| Option.bind ~f:norm
  | Some "win32" ->
    let major, minor, build, _ = OpamStubs.getWindowsVersion () in
    OpamStd.Option.some @@ Printf.sprintf "%d.%d.%d" major minor build |> Fiber.return
  | Some "cygwin" ->
    let+ cmd = run_capture_line ~path ~prog:"cmd" ~args:[ "/C"; "ver" ] in
    Option.bind cmd ~f:(fun s ->
      match Scanf.sscanf s "%_s@[ Version %s@]" Fun.id with
      | Ok s -> norm s
      | Error _ -> None)
  | Some "freebsd" -> uname ~path [ "-U" ] >>| Option.bind ~f:norm
  | _ -> uname ~path [ "-r" ] >>| Option.bind ~f:norm
;;

let os_distribution ~path =
  os ~path
  >>= function
  | Some "macos" as macos ->
    Fiber.return
    @@
    if Bin.which ~path "brew" <> None
    then Some "homebrew"
    else if Bin.which ~path "port" <> None
    then Some "macports"
    else macos
  | Some "linux" as linux ->
    is_android ~path
    >>= (function
     | true -> Fiber.return @@ Some "android"
     | false ->
       os_release_field "ID"
       >>= (function
        | Some os_release_field -> Fiber.return @@ norm os_release_field
        | None ->
          lsb_release ~path [ "-i"; "-s" ]
          >>| (function
           | Some lsb_release -> norm lsb_release
           | None ->
             (match
                [ "/etc/redhat-release"
                ; "/etc/centos-release"
                ; "/etc/gentoo-release"
                ; "/etc/issue"
                ]
                |> List.map ~f:path_of_string
                |> List.find ~f:Path.exists
              with
              | None -> linux
              | Some release_file ->
                (match Io.lines_of_file release_file with
                 | [] -> linux
                 | s :: _ ->
                   (match Scanf.sscanf s " %s " Fun.id with
                    | Error _ -> linux
                    | Ok s -> norm s))))))
  | os -> Fiber.return os
;;

let os_family ~path =
  os ~path
  >>= function
  | Some "linux" ->
    os_release_field "ID_LIKE"
    >>= (function
     | None -> os_distribution ~path
     | Some s ->
       (* first word *)
       (match Scanf.sscanf s " %s" Fun.id with
        | Error _ -> os_distribution ~path
        | Ok s -> Fiber.return @@ norm s))
  | Some ("freebsd" | "openbsd" | "netbsd" | "dragonfly") -> Fiber.return @@ Some "bsd"
  | Some ("win32" | "cygwin") -> Fiber.return @@ Some "windows"
  | _ -> os_distribution ~path
;;

let sys_ocaml_version ~path =
  match Bin.which "ocamlc" ~path with
  | None -> Fiber.return None
  | Some ocamlc ->
    Process.run_capture_line ~display:Quiet Strict ocamlc [ "-vnum" ] >>| Option.some
;;

let solver_env_from_current_system ~path =
  let entry k f =
    let+ v = f ~path in
    k, Option.map v ~f:Variable_value.string
  in
  (* TODO this will rerun `uname` multiple times with the same arguments
     unless it is memoized *)
  Fiber.all
    [ entry Variable_name.arch arch
    ; entry Variable_name.os os
    ; entry Variable_name.os_version os_version
    ; entry Variable_name.os_distribution os_distribution
    ; entry Variable_name.os_family os_family
    ; entry Variable_name.sys_ocaml_version sys_ocaml_version
    ]
  >>| List.fold_left ~init:Solver_env.empty ~f:(fun solver_env (var, data) ->
    match data with
    | Some value -> Solver_env.set solver_env var value
    | None -> solver_env)
;;
