open Stdune
module Re = Dune_re

let commands = Table.create (module String) 10

let register name of_args run =
  Table.add_exn commands name (fun args ->
    let t = of_args args in
    run t)
;;

(* Doesn't follow the symlinks! *)
module Stat = struct
  type data =
    | Hardlinks
    | Permissions
    | Size
    | Kind

  type t =
    { file : Path.t
    ; data : data
    }

  let data_of_string = function
    | "size" -> Size
    | "hardlinks" -> Hardlinks
    | "permissions" -> Permissions
    | "kind" -> Kind
    | s ->
      raise
        (Arg.Bad
           (sprintf "%s is invalid. hardlinks, permissions are only valid options" s))
  ;;

  let pp_stats data (stats : Unix.stats) =
    match data with
    | Size -> Int.to_string stats.st_size
    | Hardlinks -> Int.to_string stats.st_nlink
    | Permissions -> sprintf "%o" stats.st_perm
    | Kind -> sprintf "%s" (File_kind.to_string_hum stats.st_kind)
  ;;

  let name = "stat"

  let of_args = function
    | [ data; file ] ->
      let data = data_of_string data in
      let file = Path.of_filename_relative_to_initial_cwd file in
      { file; data }
    | _ -> raise (Arg.Bad (sprintf "2 arguments must be provided"))
  ;;

  let run { file; data } =
    let stats = Path.lstat_exn file in
    print_endline (pp_stats data stats)
  ;;

  let () = register name of_args run
end

module Wait_for_fs_clock_to_advance = struct
  let name = "wait-for-fs-clock-to-advance"

  let of_args = function
    | [] -> ()
    | _ -> raise (Arg.Bad ("Usage: dune_cmd " ^ name))
  ;;

  let run () =
    let fn = "." ^ name ^ ".tmp" in
    let fstime () =
      Unix.close (Unix.openfile fn [ O_WRONLY; O_CREAT; O_TRUNC ] 0o644);
      let t = (Unix.stat fn).st_ctime in
      Unix.unlink fn;
      t
    in
    let t = fstime () in
    while fstime () <= t do
      Unix.sleepf 0.01
    done
  ;;

  let () = register name of_args run
end

module Cat = struct
  let name = "cat"

  let of_args = function
    | [ file ] -> file
    | _ -> raise (Arg.Bad "Usage: dune_cmd cat <file>")
  ;;

  let run p = print_string (Io.String_path.read_file p)
  let () = register name of_args run
end

module Exists = struct
  type t = Path of Path.t

  let name = "exists"

  let of_args = function
    | [ path ] -> Path (Path.of_filename_relative_to_initial_cwd path)
    | _ -> raise (Arg.Bad "Usage: dune_cmd exists <path>")
  ;;

  let run (Path path) = print_string (Path.exists path |> Bool.to_string)
  let () = register name of_args run
end

module Expand_lines = struct
  let name = "expand_lines"

  let of_args = function
    | [] -> ()
    | _ -> raise (Arg.Bad ("Usage: dune_cmd " ^ name))
  ;;

  let run () =
    let re = Re.compile (Re.str "\\n") in
    set_binary_mode_in stdin true;
    set_binary_mode_out stdout true;
    let rec loop () =
      match input_line stdin with
      | exception End_of_file -> ()
      | s ->
        print_endline (Re.replace_string ~all:true re s ~by:"\n");
        loop ()
    in
    loop ()
  ;;

  let () = register name of_args run
end

module Sanitizer = struct
  module Configurator = Configurator.V1

  let make_ext_replace config =
    let tbl =
      List.filter_map
        [ "ext_exe"; "ext_dll"; "ext_asm"; "ext_lib"; "ext_obj" ]
        ~f:(fun var ->
          match Configurator.ocaml_config_var config var with
          | Some "" -> None
          | Some s -> Some (s, "$" ^ var)
          | None ->
            (match var, Configurator.ocaml_config_var config "system" with
             | "ext_exe", Some "Win32" -> Some (".exe", var)
             | _ -> None))
    in
    let re =
      Re.(
        compile
          (seq [ diff any (char '/'); alt (List.map tbl ~f:(fun (s, _) -> str s)); eow ]))
    in
    let map = String.Map.of_list_reduce tbl ~f:(fun _ x -> x) in
    fun s ->
      Re.replace re s ~f:(fun g ->
        let s = Re.Group.get g 0 in
        sprintf "%c%s" s.[0] (String.Map.find_exn map (String.drop s 1)))
  ;;

  let name = "sanitize"

  let of_args = function
    | [] -> ()
    | _ -> raise (Arg.Bad "Usage: dune_cmd sanitize takes no arguments")
  ;;

  let run () =
    let config = Configurator.create "sanitizer" in
    let sanitize = make_ext_replace config in
    let rec loop () =
      match input_line stdin with
      | exception End_of_file -> ()
      | line ->
        print_endline (sanitize line);
        loop ()
    in
    loop ()
  ;;

  let () = register name of_args run
end

module Count_lines = struct
  type t =
    | Stdin
    | File of Path.t

  let name = "count-lines"

  let count_lines ic =
    let rec loop n =
      match input_line ic with
      | exception End_of_file -> n
      | _line -> loop (n + 1)
    in
    loop 0
  ;;

  let of_args = function
    | [] -> Stdin
    | [ file ] -> File (Path.of_filename_relative_to_initial_cwd file)
    | _ -> raise (Arg.Bad "Usage: dune_cmd count-lines <file>")
  ;;

  let run t =
    let n =
      match t with
      | Stdin -> count_lines stdin
      | File p -> Io.with_file_in p ~binary:false ~f:count_lines
    in
    Printf.printf "%d\n%!" n
  ;;

  let () = register name of_args run
end

module Override_on = struct
  module Configurator = Configurator.V1

  type t =
    { system_to_override_on : string
    ; desired_output : string
    }

  let name = "override-on"

  let copy_stdin () =
    let rec loop () =
      match input_line stdin with
      | exception End_of_file -> ()
      | line ->
        print_endline line;
        loop ()
    in
    loop ()
  ;;

  let of_args = function
    | [ system_to_override_on; desired_output ] ->
      { system_to_override_on; desired_output }
    | _ ->
      raise
        (Arg.Bad "Usage: dune_cmd override-on <system-to-override-on> <desired-output>")
  ;;

  let run { system_to_override_on; desired_output } =
    let config = Configurator.create "override-on" in
    match Configurator.ocaml_config_var config "system" with
    | Some system when String.equal system system_to_override_on ->
      print_endline desired_output
    | _ -> copy_stdin ()
  ;;

  let () = register name of_args run
end

module Rewrite_path = struct
  let name = "rewrite-path"

  let of_args = function
    | [ path ] -> path
    | _ -> raise (Arg.Bad "Usage: dune_cmd rewrite-path <path>")
  ;;

  let run path =
    match Build_path_prefix_map.decode_map (Sys.getenv "BUILD_PATH_PREFIX_MAP") with
    | Error msg -> failwith msg
    | Ok map -> print_string (Build_path_prefix_map.rewrite map path)
  ;;

  let () = register name of_args run
end

module Find_by_contents = struct
  let name = "find-file-by-contents-regexp"

  let of_args = function
    | [ path; contents_regexp ] -> path, Str.regexp contents_regexp
    | _ -> raise (Arg.Bad "Usage: dune_cmd find-files-by-contents-regexp <path> <regexp>")
  ;;

  let rec find_files ~dir regexp : _ list =
    List.concat_map
      (List.sort (Sys.readdir dir |> Array.to_list) ~compare:String.compare)
      ~f:(fun name ->
        let path = Filename.concat dir name in
        let stats = Unix.stat path in
        match stats.st_kind with
        | S_DIR -> find_files ~dir:path regexp
        | S_REG ->
          let s = Io.String_path.read_file path in
          if Str.string_match regexp s 0 then [ Printf.sprintf "%s\n" path ] else []
        | _other -> [])
  ;;

  let run (dir, regexp) =
    match find_files ~dir regexp with
    | [] ->
      Format.eprintf "No files found matching pattern@.%!";
      exit 1
    | [ res ] -> Printf.printf "%s\n" res
    | _ :: _ as files ->
      Format.eprintf "Multiple files found matching pattern@.%!";
      List.iter files ~f:(fun file -> Printf.printf "%s\n%!" file);
      exit 1
  ;;

  let () = register name of_args run
end

module Wait_for_file_to_appear = struct
  type t = { file : Path.t }

  let name = "wait-for-file-to-appear"

  let of_args = function
    | [ file ] ->
      let file = Path.of_filename_relative_to_initial_cwd file in
      { file }
    | _ -> raise (Arg.Bad (sprintf "1 argument must be provided"))
  ;;

  let run { file } =
    while not (Path.exists file) do
      Unix.sleepf 0.01
    done
  ;;

  let () = register name of_args run
end

let () =
  let name, args =
    match Array.to_list Sys.argv with
    | _ :: name :: args -> name, args
    | [] -> assert false
    | [ _ ] ->
      Format.eprintf "No arguments passed@.%!";
      exit 1
  in
  match Table.find commands name with
  | None ->
    Format.eprintf "No command %S name found" name;
    exit 1
  | Some run -> run args
;;
