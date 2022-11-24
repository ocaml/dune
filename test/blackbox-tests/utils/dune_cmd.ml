open Stdune
module Re = Dune_re

let ( let+ ) x k = Cmdliner.Term.(const k $ x)

let ( and+ ) a b = Cmdliner.Term.(const (fun x y -> (x, y)) $ a $ b)

let string_arg n = Cmdliner.Arg.(required & pos n (some string) None & info [])

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
           (sprintf
              "%s is invalid. hardlinks, permissions are only valid options" s))

  let pp_stats data (stats : Unix.stats) =
    match data with
    | Size -> Int.to_string stats.st_size
    | Hardlinks -> Int.to_string stats.st_nlink
    | Permissions -> sprintf "%o" stats.st_perm
    | Kind -> sprintf "%s" (File_kind.to_string_hum stats.st_kind)

  let name = "stat"

  let args =
    let+ data_s = string_arg 0
    and+ file_s = string_arg 1 in
    let data = data_of_string data_s in
    let file = Path.of_filename_relative_to_initial_cwd file_s in
    { file; data }

  let term =
    let+ { file; data } = args in
    let stats = Path.lstat_exn file in
    print_endline (pp_stats data stats)

  let info = Cmdliner.Cmd.info name

  let cmd = Cmdliner.Cmd.v info term
end

module Wait_for_fs_clock_to_advance = struct
  let name = "wait-for-fs-clock-to-advance"

  let term =
    let+ () = Cmdliner.Term.const () in
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

  let info = Cmdliner.Cmd.info name

  let cmd = Cmdliner.Cmd.v info term
end

module Cat = struct
  let term =
    let+ p = string_arg 0 in
    print_string (Io.String_path.read_file p)

  let info = Cmdliner.Cmd.info "cat"

  let cmd = Cmdliner.Cmd.v info term
end

module Exists = struct
  let term =
    let+ path_s = string_arg 0 in
    let path = Path.of_filename_relative_to_initial_cwd path_s in
    print_string (Path.exists path |> Bool.to_string)

  let info = Cmdliner.Cmd.info "exists"

  let cmd = Cmdliner.Cmd.v info term
end

module Expand_lines = struct
  let term =
    let+ () = Cmdliner.Term.const () in
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

  let info = Cmdliner.Cmd.info "expand_lines"

  let cmd = Cmdliner.Cmd.v info term
end

module Sanitizer = struct
  module Configurator = Configurator.V1

  let make_ext_replace config =
    let tbl =
      List.filter_map [ "ext_exe"; "ext_dll"; "ext_asm"; "ext_lib"; "ext_obj" ]
        ~f:(fun var ->
          match Configurator.ocaml_config_var config var with
          | Some "" -> None
          | Some s -> Some (s, "$" ^ var)
          | None -> (
            match (var, Configurator.ocaml_config_var config "system") with
            | "ext_exe", Some "Win32" -> Some (".exe", var)
            | _ -> None))
    in
    let re =
      Re.(
        compile
          (seq
             [ diff any (char '/')
             ; alt (List.map tbl ~f:(fun (s, _) -> str s))
             ; eow
             ]))
    in
    let map = String.Map.of_list_reduce tbl ~f:(fun _ x -> x) in
    fun s ->
      Re.replace re s ~f:(fun g ->
          let s = Re.Group.get g 0 in
          sprintf "%c%s" s.[0] (String.Map.find_exn map (String.drop s 1)))

  let term =
    let+ () = Cmdliner.Term.const () in
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

  let info = Cmdliner.Cmd.info "sanitize"

  let cmd = Cmdliner.Cmd.v info term
end

module Count_lines = struct
  type t =
    | Stdin
    | File of Path.t

  let count_lines ic =
    let rec loop n =
      match input_line ic with
      | exception End_of_file -> n
      | _line -> loop (n + 1)
    in
    loop 0

  let kind =
    let+ file_opt = Cmdliner.Arg.(value & pos 0 (some string) None & info []) in
    match file_opt with
    | None -> Stdin
    | Some file -> File (Path.of_filename_relative_to_initial_cwd file)

  let term =
    let+ t = kind in
    let n =
      match t with
      | Stdin -> count_lines stdin
      | File p -> Io.with_file_in p ~binary:false ~f:count_lines
    in
    Printf.printf "%d\n%!" n

  let info = Cmdliner.Cmd.info "count-lines"

  let cmd = Cmdliner.Cmd.v info term
end

module Override_on = struct
  module Configurator = Configurator.V1

  let copy_stdin () =
    let rec loop () =
      match input_line stdin with
      | exception End_of_file -> ()
      | line ->
        print_endline line;
        loop ()
    in
    loop ()

  let term =
    let+ system_to_override_on = string_arg 0
    and+ desired_output = string_arg 1 in
    let config = Configurator.create "override-on" in
    match Configurator.ocaml_config_var config "system" with
    | Some system when String.equal system system_to_override_on ->
      print_endline desired_output
    | _ -> copy_stdin ()

  let info = Cmdliner.Cmd.info "override-on"

  let cmd = Cmdliner.Cmd.v info term
end

module Rewrite_path = struct
  let term =
    let+ path = string_arg 0 in
    match
      Build_path_prefix_map.decode_map (Sys.getenv "BUILD_PATH_PREFIX_MAP")
    with
    | Error msg -> failwith msg
    | Ok map -> print_string (Build_path_prefix_map.rewrite map path)

  let info = Cmdliner.Cmd.info "rewrite-path"

  let cmd = Cmdliner.Cmd.v info term
end

module Find_by_contents = struct
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
          if Str.string_match regexp s 0 then [ Printf.sprintf "%s\n" path ]
          else []
        | _other -> [])

  let term =
    let+ dir = string_arg 0
    and+ regexp_s = string_arg 1 in
    let regexp = Str.regexp regexp_s in
    match find_files ~dir regexp with
    | [] ->
      Format.eprintf "No files found matching pattern@.%!";
      exit 1
    | [ res ] -> Printf.printf "%s\n" res
    | _ :: _ as files ->
      Format.eprintf "Multiple files found matching pattern@.%!";
      List.iter files ~f:(fun file -> Printf.printf "%s\n%!" file);
      exit 1

  let info = Cmdliner.Cmd.info "find-file-by-contents-regexp"

  let cmd = Cmdliner.Cmd.v info term
end

module Wait_for_file_to_appear = struct
  let term =
    let+ s = string_arg 0 in
    let file = Path.of_filename_relative_to_initial_cwd s in
    while not (Path.exists file) do
      Unix.sleepf 0.01
    done

  let info = Cmdliner.Cmd.info "wait-for-file-to-appear"

  let cmd = Cmdliner.Cmd.v info term
end

let info = Cmdliner.Cmd.info "dune-cmd"

let cmd =
  Cmdliner.Cmd.group info
    [ Stat.cmd
    ; Wait_for_fs_clock_to_advance.cmd
    ; Cat.cmd
    ; Exists.cmd
    ; Expand_lines.cmd
    ; Sanitizer.cmd
    ; Count_lines.cmd
    ; Override_on.cmd
    ; Rewrite_path.cmd
    ; Find_by_contents.cmd
    ; Wait_for_file_to_appear.cmd
    ]

let () = Cmdliner.Cmd.eval cmd |> Stdlib.exit
