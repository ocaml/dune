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
  let prog = Bin.which ~path prog in
  match prog with
  | None -> Fiber.return None
  | Some prog ->
    let+ res = Process.run_capture_line ~display:Quiet Strict prog args in
    norm res
;;

let uname ~path args = run_capture_line ~path ~prog:"uname" ~args
let lsb_release ~path args = run_capture_line ~path ~prog:"lsb_release" ~args

let arch ~path =
  let+ raw =
    match Sys.os_type with
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
    | _ -> Fiber.return None
  in
  match raw with
  | None | Some "" -> None
  | Some a -> Some (normalise_arch a)
;;

let os ~path =
  let+ raw =
    match Sys.os_type with
    | "Unix" -> uname ~path [ "-s" ]
    | s -> Fiber.return (norm s)
  in
  match raw with
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
  let candidates =
    [ "/etc/os-release"; "/usr/lib/os-release" ] |> List.map ~f:path_of_string
  in
  match List.find ~f:Path.exists candidates with
  | None -> Fiber.return None
  | Some release_file ->
    let lines = Io.lines_of_file release_file in
    let mappings =
      List.filter_map lines ~f:(fun line ->
        match Scanf.sscanf line "%s@= %s" (fun k v -> k, v) with
        | Error _ -> None
        | Ok (key, v) ->
          (match Scanf.sscanf v "\"%s@\"" Fun.id with
           | Error _ -> Some (key, v)
           | Ok contents -> Some (key, contents)))
    in
    Fiber.return @@ List.assoc mappings field
;;

let os_version ~path =
  let* os = os ~path in
  match os with
  | Some "linux" ->
    let* prop = android_release ~path in
    (match prop with
     | Some android -> Fiber.return @@ norm android
     | None ->
       let* release = lsb_release ~path [ "-s"; "-r" ] in
       (match release with
        | Some lsb -> Fiber.return @@ norm lsb
        | None ->
          let+ version_id = os_release_field "VERSION_ID" in
          Option.bind version_id ~f:norm))
  | Some "macos" ->
    let+ sw_vers = run_capture_line ~path ~prog:"sw_vers" ~args:[ "-productVersion" ] in
    Option.bind sw_vers ~f:norm
  | Some "win32" ->
    let major, minor, build, _ = OpamStubs.getWindowsVersion () in
    OpamStd.Option.some @@ Printf.sprintf "%d.%d.%d" major minor build |> Fiber.return
  | Some "cygwin" ->
    let+ cmd = run_capture_line ~path ~prog:"cmd" ~args:[ "/C"; "ver" ] in
    Option.bind cmd ~f:(fun s ->
      match Scanf.sscanf s "%_s@[ Version %s@]" Fun.id with
      | Ok s -> norm s
      | Error _ -> None)
  | Some "freebsd" ->
    let+ uname = uname ~path [ "-U" ] in
    Option.bind uname ~f:norm
  | _ ->
    let+ uname = uname ~path [ "-r" ] in
    Option.bind uname ~f:norm
;;

let os_distribution ~path =
  let* os = os ~path in
  match os with
  | Some "macos" as macos ->
    if Bin.which ~path "brew" <> None
    then Fiber.return @@ Some "homebrew"
    else if Bin.which ~path "port" <> None
    then Fiber.return @@ Some "macports"
    else Fiber.return macos
  | Some "linux" as linux ->
    let* is_android = is_android ~path in
    (match is_android with
     | true -> Fiber.return @@ Some "android"
     | false ->
       let* id = os_release_field "ID" in
       (match id with
        | Some os_release_field -> Fiber.return @@ norm os_release_field
        | None ->
          let+ lsb_release = lsb_release ~path [ "-i"; "-s" ] in
          (match lsb_release with
           | Some lsb_release -> norm lsb_release
           | None ->
             let candidates =
               [ "/etc/redhat-release"
               ; "/etc/centos-release"
               ; "/etc/gentoo-release"
               ; "/etc/issue"
               ]
               |> List.map ~f:path_of_string
             in
             (match List.find ~f:Path.exists candidates with
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
  let* os = os ~path in
  match os with
  | Some "linux" ->
    let* id_like = os_release_field "ID_LIKE" in
    (match id_like with
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

let sys_bindings ~path =
  let entry k f =
    let+ v = f ~path in
    k, v
  in
  (* TODO this will rerun `uname` multiple times with the same arguments
     unless it is memoized *)
  let+ mappings =
    Fiber.all
      [ entry `Arch arch
      ; entry `Os os
      ; entry `Os_version os_version
      ; entry `Os_distribution os_distribution
      ; entry `Os_family os_family
      ]
  in
  List.fold_left
    ~init:Solver_env.Variable.Sys.Bindings.empty
    ~f:(fun sys_bindings (var, data) ->
      match data with
      | Some value -> Solver_env.Variable.Sys.Bindings.set sys_bindings var value
      | None -> sys_bindings)
    mappings
;;
