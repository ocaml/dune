open Import
open Fiber.O

type t =
  { arch : string option Fiber.t
  ; os : string option Fiber.t
  ; os_version : string option Fiber.t
  ; os_distribution : string option Fiber.t
  ; os_family : string option Fiber.t
  ; sys_ocaml_version : string option Fiber.t
  }

let apply_or_skip_empty f = function
  | None | Some "" -> None
  | Some s -> Some (f s)
;;

let norm = function
  | "" -> None
  | s -> Some (String.lowercase s)
;;

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

module Config_override_variables = struct
  let string_option_config name =
    let config =
      Dune_config.Config.make ~name ~of_string:(fun s -> Ok (Some s)) ~default:None
    in
    fun () -> Dune_config.Config.get config
  ;;

  let os = string_option_config "os"
  let arch = string_option_config "arch"
end

(* CR-rgrinberg: do we need to call [uname] for every single option? Can't we
   call [uname -a] and extract everything from there *)
let uname ~path args = run_capture_line ~path ~prog:"uname" ~args
let lsb_release ~path args = run_capture_line ~path ~prog:"lsb_release" ~args

let arch ~path =
  match Config_override_variables.arch () with
  | Some arch -> Fiber.return (Some arch)
  | None ->
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
    >>| apply_or_skip_empty normalise_arch
;;

let os ~path =
  match Config_override_variables.os () with
  | Some os -> Fiber.return (Some os)
  | None ->
    (match Sys.os_type with
     | "Unix" -> uname ~path [ "-s" ]
     | s -> Fiber.return (norm s))
    >>| apply_or_skip_empty normalise_os
;;

let maybe_read_lines p =
  match Io.String_path.lines_of_file p with
  | s -> Some s
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> None
;;

let os_release_fields () =
  match
    List.find_map [ "/etc/os-release"; "/usr/lib/os-release" ] ~f:maybe_read_lines
  with
  | None -> []
  | Some release_lines ->
    List.filter_map release_lines ~f:(fun line ->
      match Scanf.sscanf line "%s@= %s" (fun k v -> k, v) with
      | Error _ -> None
      | Ok (key, v) ->
        Some
          ( key
          , match Scanf.sscanf v "\"%s@\"" Fun.id with
            | Error _ -> v
            | Ok contents -> contents ))
;;

let os_version ~android_release ~os ~os_release_fields ~path =
  os
  >>= function
  | Some "linux" ->
    android_release
    >>= (function
     | Some android -> Fiber.return @@ norm android
     | None ->
       lsb_release ~path [ "-s"; "-r" ]
       >>| (function
        | Some lsb -> norm lsb
        | None ->
          List.assoc (Lazy.force os_release_fields) "VERSION_ID" |> Option.bind ~f:norm))
  | Some "macos" ->
    run_capture_line ~path ~prog:"sw_vers" ~args:[ "-productVersion" ]
    >>| Option.bind ~f:norm
  | Some "win32" ->
    let major, minor, build, _ = OpamStubs.getWindowsVersion () in
    Some (Printf.sprintf "%d.%d.%d" major minor build) |> Fiber.return
  | Some "cygwin" ->
    run_capture_line ~path ~prog:"cmd" ~args:[ "/C"; "ver" ]
    >>| Option.bind ~f:(fun s ->
      match Scanf.sscanf s "%_s@[ Version %s@]" Fun.id with
      | Ok s -> norm s
      | Error _ -> None)
  | Some "freebsd" -> uname ~path [ "-U" ] >>| Option.bind ~f:norm
  | _ -> uname ~path [ "-r" ] >>| Option.bind ~f:norm
;;

let os_distribution ~os ~android_release ~os_release_fields ~path =
  os
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
    android_release
    >>= (function
     | Some _ -> Fiber.return @@ Some "android"
     | None ->
       (match List.assoc (Lazy.force os_release_fields) "ID" with
        | Some os_release_field -> Fiber.return @@ norm os_release_field
        | None ->
          lsb_release ~path [ "-i"; "-s" ]
          >>| (function
           | Some lsb_release -> norm lsb_release
           | None ->
             (match
                List.find_map
                  ~f:maybe_read_lines
                  [ "/etc/redhat-release"
                  ; "/etc/centos-release"
                  ; "/etc/gentoo-release"
                  ; "/etc/issue"
                  ]
              with
              | None | Some [] -> linux
              | Some (s :: _) ->
                (match Scanf.sscanf s " %s " Fun.id with
                 | Error _ -> linux
                 | Ok s -> norm s)))))
  | os -> Fiber.return os
;;

let os_family ~os_distribution ~os_release_fields ~os =
  os
  >>= function
  | Some ("freebsd" | "openbsd" | "netbsd" | "dragonfly") -> Fiber.return @@ Some "bsd"
  | Some ("win32" | "cygwin") -> Fiber.return @@ Some "windows"
  | Some "linux" ->
    (match List.assoc (Lazy.force os_release_fields) "ID_LIKE" with
     | None -> os_distribution
     | Some s ->
       (* first word *)
       (match Scanf.sscanf s " %s" Fun.id with
        | Error _ -> os_distribution
        | Ok s -> Fiber.return @@ norm s))
  | _ -> os_distribution
;;

let make_lazy f = Fiber_lazy.create f |> Fiber_lazy.force

let make ~path =
  let arch = make_lazy (fun () -> arch ~path) in
  let os = make_lazy (fun () -> os ~path) in
  let os_release_fields = lazy (os_release_fields ()) in
  let android_release =
    make_lazy (fun () ->
      run_capture_line ~path ~prog:"getprop" ~args:[ "ro.build.version.release" ])
  in
  let os_version =
    make_lazy (fun () -> os_version ~android_release ~os_release_fields ~os ~path)
  in
  let os_distribution =
    make_lazy (fun () -> os_distribution ~android_release ~os_release_fields ~os ~path)
  in
  let os_family =
    make_lazy (fun () -> os_family ~os_release_fields ~os_distribution ~os)
  in
  let sys_ocaml_version =
    make_lazy (fun () -> run_capture_line ~path ~prog:"ocamlc" ~args:[ "-vnum" ])
  in
  { arch; os; os_version; os_distribution; os_family; sys_ocaml_version }
;;

let arch t = t.arch
let os t = t.os
let os_version t = t.os_version
let os_distribution t = t.os_distribution
let os_family t = t.os_family
let sys_ocaml_version t = t.sys_ocaml_version

let solver_env_from_current_system t =
  let entry k f =
    let+ v = f t in
    k, Option.map v ~f:Variable_value.string
  in
  (* TODO this will rerun `uname` multiple times with the same arguments
     unless it is memoized *)
  Fiber.all
    [ entry Package_variable_name.arch arch
    ; entry Package_variable_name.os os
    ; entry Package_variable_name.os_version os_version
    ; entry Package_variable_name.os_distribution os_distribution
    ; entry Package_variable_name.os_family os_family
    ; entry Package_variable_name.sys_ocaml_version sys_ocaml_version
    ]
  >>| List.fold_left ~init:Solver_env.empty ~f:(fun solver_env (var, data) ->
    match data with
    | Some value -> Solver_env.set solver_env var value
    | None -> solver_env)
;;
