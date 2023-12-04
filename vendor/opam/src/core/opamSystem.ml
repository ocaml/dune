(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type install_warning =
  [ `Add_exe | `Install_dll | `Install_script | `Install_unknown
  | `Cygwin | `Msys2 | `Tainted of [`Msys2 | `Cygwin] | `Cygwin_libraries ]
type install_warning_fn = string -> install_warning -> unit

exception Process_error of OpamProcess.result
exception Internal_error of string
exception Command_not_found of string
exception File_not_found of string
exception Permission_denied of string

let log ?level fmt = OpamConsole.log "SYSTEM" ?level fmt
let slog = OpamConsole.slog

let internal_error fmt =
  Printf.ksprintf (fun str ->
    log "error: %s" str;
    raise (Internal_error str)
  ) fmt

let process_error r =
  if r.OpamProcess.r_signal = Some Sys.sigint then raise Sys.Break
  else raise (Process_error r)

let raise_on_process_error r =
  if OpamProcess.is_failure r then raise (Process_error r)

let command_not_found cmd =
  raise (Command_not_found cmd)

let permission_denied cmd =
  raise (Permission_denied cmd)

module Sys2 = struct
  (* same as [Sys.is_directory] except for symlinks, which returns always [false]. *)
  let is_directory file =
    try Unix.( (lstat file).st_kind = S_DIR )
    with Unix.Unix_error _ as e -> raise (Sys_error (Printexc.to_string e))
end

let file_or_symlink_exists f =
  try ignore (Unix.lstat f); true
  with Unix.Unix_error (Unix.ENOENT, _, _) -> false

let (/) = Filename.concat

let temp_basename prefix =
  Printf.sprintf "%s-%d-%06x" prefix (OpamStubs.getpid ()) (Random.int 0xFFFFFF)

let rec mk_temp_dir ?(prefix="opam") () =
  let s = Filename.get_temp_dir_name () / temp_basename prefix in
  if Sys.file_exists s then
    mk_temp_dir ()
  else
    s

let safe_mkdir dir =
  try
    log "mkdir %s" dir;
    Unix.mkdir dir 0o755
  with
    Unix.Unix_error(Unix.EEXIST,_,_) -> ()

let mkdir dir =
  let rec aux dir =
    if not (Sys.file_exists dir) then begin
      aux (Filename.dirname dir);
      safe_mkdir dir;
    end in
  aux dir

let get_files dirname =
  let dir = Unix.opendir dirname in
  let rec aux files =
    match Unix.readdir dir with
    | "." | ".." -> aux files
    | file -> aux (file :: files)
    | exception End_of_file -> files
  in
  let files = aux [] in
  Unix.closedir dir;
  files

let log_for_file_management () =
  OpamCoreConfig.(!r.debug_level) >= 4

(* From stdune/src/fpath.ml *)
let win32_unlink fn =
  try Unix.unlink fn
  with Unix.Unix_error (Unix.EACCES, _, _) as e -> (
    try
      (* Try removing the read-only attribute *)
      Unix.chmod fn 0o666;
      Unix.unlink fn
    with _ -> raise e)

let remove_file_t ?(with_log=true) file =
  if
    try ignore (Unix.lstat file); true with Unix.Unix_error _ -> false
  then (
    try
      if with_log || log_for_file_management () then
        log "rm %s" file;
      if Sys.win32 then
        win32_unlink file
      else
        Unix.unlink file
    with Unix.Unix_error _ as e ->
      internal_error "Cannot remove %s (%s)." file (Printexc.to_string e)
  )

let rec remove_dir_t dir =
  let files = get_files dir in
  List.iter (fun file ->
      let file = Filename.concat dir file in
      match Unix.lstat file with
      | {Unix.st_kind = Unix.S_DIR; _} ->
        remove_dir_t file
      | {Unix.st_kind = Unix.(S_REG | S_LNK | S_CHR | S_BLK | S_FIFO | S_SOCK); _} ->
        remove_file_t ~with_log:false file
    ) files;
  Unix.rmdir dir

let remove_file = remove_file_t ~with_log:true

let remove_dir dir =
  log "rmdir %s" dir;
  if Sys.file_exists dir then begin
    if Sys.is_directory dir then
      remove_dir_t dir
    else
      remove_file dir
  end

let temp_files = Hashtbl.create 1024
let logs_cleaner =
  let to_clean = ref OpamStd.String.Set.empty in
  OpamStd.Sys.at_exit
    (fun () ->
       OpamStd.String.Set.iter (fun f ->
           try
             Unix.unlink f;
             (* Only log the item if unlink succeeded *)
             log "logs_cleaner: rm: %s" f
           with Unix.Unix_error _ -> ())
         !to_clean;
       if OpamCoreConfig.(!r.log_dir = default.log_dir) then
         try Unix.rmdir OpamCoreConfig.(default.log_dir)
         with Unix.Unix_error _ -> ());
  fun tmp_dir ->
    if OpamCoreConfig.(!r.keep_log_dir) then
      to_clean := OpamStd.String.Set.remove tmp_dir !to_clean
    else
      to_clean := OpamStd.String.Set.add tmp_dir !to_clean

let rec temp_file ?(auto_clean=true) ?dir prefix =
  let temp_dir = match dir with
    | None   -> OpamCoreConfig.(!r.log_dir)
    | Some d -> d in
  mkdir temp_dir;
  let file = temp_dir / temp_basename prefix in
  if Hashtbl.mem temp_files file then
    temp_file ~auto_clean ?dir prefix
  else (
    Hashtbl.add temp_files file true;
    if auto_clean then logs_cleaner file;
    file
  )

let remove_file file =
  if
    try ignore (Unix.lstat file); true with Unix.Unix_error _ -> false
  then (
    log "rm %s" file;
    try
      try Unix.unlink file
      with Unix.Unix_error(EACCES, _, _) when Sys.win32 ->
        (* Attempt to remove the read-only bit on Windows *)
        Unix.chmod file 0o666;
        Unix.unlink file
    with Unix.Unix_error _ as e ->
      internal_error "Cannot remove %s (%s)." file (Printexc.to_string e)
  )

let string_of_channel ic =
  let n = 32768 in
  let s = Bytes.create n in
  let b = Buffer.create 1024 in
  let rec iter ic b s =
    let nread =
      try input ic s 0 n
      with End_of_file -> 0 in
    if nread > 0 then (
      Buffer.add_subbytes b s 0 nread;
      iter ic b s
    ) in
  iter ic b s;
  Buffer.contents b

let read file =
  let ic =
    try open_in_bin file
    with Sys_error _ -> raise (File_not_found file) in
  Unix.lockf (Unix.descr_of_in_channel ic) Unix.F_RLOCK 0;
  let s = string_of_channel ic in
  close_in ic;
  s

let write file contents =
  mkdir (Filename.dirname file);
  let oc =
    try open_out_bin file
    with Sys_error _ -> raise (File_not_found file)
  in
  Unix.lockf (Unix.descr_of_out_channel oc) Unix.F_LOCK 0;
  output_string oc contents;
  close_out oc

let setup_copy ?(chmod = fun x -> x) ~src ~dst () =
  let ic = open_in_bin src in
  try
    let perm =
      (Unix.fstat (Unix.descr_of_in_channel ic)).st_perm |> chmod
    in
    let () =
      try if Unix.((lstat dst).st_kind <> S_REG) then
            remove_file dst
      with Unix.Unix_error(ENOENT, _, _) -> ()
    in
    let fd =
      let flags = Unix.[ O_WRONLY; O_CREAT; O_TRUNC ] in
      try Unix.openfile dst flags perm
      with Unix.Unix_error(EACCES, _, _) when Sys.win32 ->
        (* Attempt to remove the read-only bit on Windows *)
        begin
          try Unix.chmod dst 0o666
          with Unix.Unix_error(_, _, _) -> ()
        end;
        Unix.openfile dst flags perm
    in
    try
      if Unix.((fstat fd).st_perm) <> perm then
        Unix.fchmod fd perm;
      (ic, Unix.out_channel_of_descr fd)
    with exn ->
      OpamStd.Exn.finalise exn (fun () -> Unix.close fd)
  with exn ->
    OpamStd.Exn.finalise exn (fun () -> close_in ic)

let copy_channels =
  let buf_len = 4096 in
  let buf = Bytes.create buf_len in
  let rec loop ic oc =
    match input ic buf 0 buf_len with
    | 0 -> ()
    | n ->
      output oc buf 0 n;
      loop ic oc
  in
  loop

let copy_file_aux ?chmod ~src ~dst () =
  let close_channels ic oc =
    OpamStd.Exn.finally (fun () -> close_in ic) (fun () -> close_out oc) in
  try
    let ic, oc = setup_copy ?chmod ~src ~dst () in
    OpamStd.Exn.finally (fun () -> close_channels ic oc)
      (fun () -> copy_channels ic oc);
  with Unix.Unix_error _ as e ->
    (* Remove the partial destination file, if any. *)
    (try Unix.unlink dst with Unix.Unix_error _ -> ());
    internal_error "Cannot copy %s to %s (%s)." src dst (Printexc.to_string e)

let chdir dir =
  try Unix.chdir dir
  with Unix.Unix_error _ -> raise (File_not_found dir)

let in_dir dir fn =
  let reset_cwd =
    let cwd =
      try Some (Sys.getcwd ())
      with Sys_error _ -> None in
    fun () ->
      match cwd with
      | None     -> ()
      | Some cwd -> try chdir cwd with File_not_found _ -> () in
  chdir dir;
  try
    let r = fn () in
    reset_cwd ();
    r
  with e ->
    OpamStd.Exn.finalise e reset_cwd

let list kind dir =
  try
    in_dir dir (fun () ->
      let d = Sys.readdir (Sys.getcwd ()) in
      let d = Array.to_list d in
      let l = List.filter kind d in
      List.map (Filename.concat dir) (List.sort compare l)
    )
  with File_not_found _ -> []

let ls dir = list (fun _ -> true) dir

let files_with_links =
  list (fun f -> try not (Sys.is_directory f) with Sys_error _ -> false)

let files_all_not_dir =
    list (fun f -> try not (Sys2.is_directory f) with Sys_error _ -> false)

let directories_strict =
  list (fun f -> try Sys2.is_directory f with Sys_error _ -> false)

let directories_with_links =
  list (fun f -> try Sys.is_directory f with Sys_error _ -> false)

let rec_files dir =
  let rec aux accu dir =
    let d = directories_with_links dir in
    let f = files_with_links dir in
    List.fold_left aux (f @ accu) d in
  aux [] dir

let files dir =
  files_with_links dir

let rec_dirs dir =
  let rec aux accu dir =
    let d = directories_with_links dir in
    List.fold_left aux (d @ accu) d in
  aux [] dir

let dirs dir =
  directories_with_links dir

let dir_is_empty dir =
  try in_dir dir (fun () -> Sys.readdir (Sys.getcwd ()) = [||])
  with File_not_found _ -> false

let with_tmp_dir fn =
  let dir = mk_temp_dir () in
  try
    mkdir dir;
    let e = fn dir in
    remove_dir dir;
    e
  with e ->
    OpamStd.Exn.finalise e @@ fun () ->
    remove_dir dir

let in_tmp_dir fn =
  with_tmp_dir @@ fun dir ->
    in_dir dir fn

let with_tmp_dir_job fjob =
  let dir = mk_temp_dir () in
  mkdir dir;
  OpamProcess.Job.finally (fun () -> remove_dir dir) (fun () -> fjob dir)

let remove file =
  if (try Sys2.is_directory file with Sys_error _ -> false) then
    remove_dir file
  else
    remove_file file

let real_path p =
  (* if Filename.is_relative p then *)
    match (try Some (Sys.is_directory p) with Sys_error _ -> None) with
    | None ->
      let rec resolve dir =
        if Sys.file_exists dir then OpamCompat.Unix.realpath dir else
        let parent = Filename.dirname dir in
        if dir = parent then dir
        else Filename.concat (resolve parent) (Filename.basename dir)
      in
      let p =
        if Filename.is_relative p then Filename.concat (Sys.getcwd ()) p
        else p
      in
      resolve p
    | Some true -> OpamCompat.Unix.realpath p
    | Some false ->
      let dir = OpamCompat.Unix.realpath (Filename.dirname p) in
      match Filename.basename p with
      | "." -> dir
      | base -> dir / base
  (* else p *)

type command = string list

let env_var env var =
  let len = Array.length env in
  let f = if Sys.win32 then String.uppercase_ascii else fun x -> x in
  let prefix = f var^"=" in
  let pfxlen = String.length prefix in
  let rec aux i =
    if i >= len then "" else
    let s = env.(i) in
    if OpamStd.String.starts_with ~prefix (f s) then
      String.sub s pfxlen (String.length s - pfxlen)
    else aux (i+1)
  in
  aux 0

let forward_to_back =
  if Sys.win32 then
    String.map (function '/' -> '\\' | c -> c)
  else
    fun x -> x

let back_to_forward =
  if Sys.win32 then
    String.map (function '\\' -> '/' | c -> c)
  else
    fun x -> x

(* OCaml 4.05.0 no longer follows the updated PATH to resolve commands. This
   makes unqualified commands absolute as a workaround. *)
let t_resolve_command =
  let is_external_cmd name =
    let name = forward_to_back name in
    OpamStd.String.contains_char name Filename.dir_sep.[0]
  in
  let check_perms =
    if Sys.win32 then fun f ->
      try (Unix.stat f).Unix.st_kind = Unix.S_REG
      with e -> OpamStd.Exn.fatal e; false
    else fun f ->
      try
        let open Unix in
        let uid = geteuid () in
        let groups = OpamStd.IntSet.of_list (getegid () :: Array.to_list (getgroups ())) in
        let {st_uid; st_gid; st_perm; _} = stat f in
        let mask =
          if uid = st_uid then
            0o100
          else if OpamStd.IntSet.mem st_gid groups then
            0o010
          else
            0o001
        in
        if (st_perm land mask) <> 0 then
          true
        else
          match OpamACL.get_acl_executable_info f st_uid with
          | None -> false
          | Some [] -> true
          | Some gids -> OpamStd.IntSet.(not (is_empty (inter (of_list gids) groups)))
      with e -> OpamStd.Exn.fatal e; false
  in
  let resolve ?dir env name =
    if not (Filename.is_relative name) then begin
      (* absolute path *)
      if not (Sys.file_exists name) then `Not_found
      else if not (check_perms name) then `Denied
      else `Cmd name
    end else if is_external_cmd name then begin
      (* relative path *)
      let cmd = match dir with
        | None -> name
        | Some d -> Filename.concat d name
      in
      if not (Sys.file_exists cmd) then `Not_found
      else if not (check_perms cmd) then `Denied
      else `Cmd cmd
    end else
    (* bare command, lookup in PATH *)
    (* Following the shell sematics for looking up PATH, programs with the
       expected name but not the right permissions are skipped silently.
       Therefore, only two outcomes are possible in that case, [`Cmd ..] or
       [`Not_found]. *)
    let path = OpamStd.Sys.split_path_variable (env_var env "PATH") in
    let name =
      if Sys.win32 && not (Filename.check_suffix name ".exe") then
        name ^ ".exe"
      else name
    in
    let possibles = OpamStd.List.filter_map (fun path ->
        let candidate = Filename.concat path name in
        if Sys.file_exists candidate then Some candidate else None) path
    in
    match List.find check_perms possibles with
    | cmdname -> `Cmd cmdname
    | exception Not_found ->
      if possibles = [] then
        `Not_found
      else
        `Denied
  in
  fun ?env ?dir name ->
    let env = match env with None -> OpamProcess.default_env () | Some e -> e in
    resolve env ?dir name

let resolve_command ?env ?dir name =
  match t_resolve_command ?env ?dir name with
  | `Cmd cmd -> Some cmd
  | `Denied | `Not_found -> None

let apply_cygpath name =
  (* XXX Deeper bug, looking in the cygvoke code (see OpamProcess.create) *)
  match resolve_command "cygpath" with
  | Some cygpath ->
    let r =
      OpamProcess.run
        (OpamProcess.command ~name:(temp_file "command")
           ~allow_stdin:false ~verbose:false cygpath ["--"; name])
    in
    OpamProcess.cleanup ~force:true r;
    if OpamProcess.is_success r then
      match r.OpamProcess.r_stdout with
      | l::_ -> l
      | _ -> ""
    else
      OpamConsole.error_and_exit `Internal_error "Could not apply cygpath to %s" name
  | None ->
    OpamConsole.error_and_exit `Internal_error "Could not apply cygpath to %s" name

let get_cygpath_function =
  if Sys.win32 then
    fun ~command ->
      lazy (
        if OpamStd.Option.map_default
            (OpamStd.Sys.is_cygwin_variant
               ~cygbin:(OpamCoreConfig.(!r.cygbin)))
               false
               (resolve_command command) then
          apply_cygpath
        else fun x -> x
      )
  else
    let f = Lazy.from_val (fun x -> x) in
    fun ~command:_ -> f

let apply_cygpath_path_transform path =
  let r =
    OpamProcess.run
      (OpamProcess.command ~name:(temp_file "command") ~verbose:false "cygpath" ["--path"; "--"; path])
  in
  OpamProcess.cleanup ~force:true r;
  if OpamProcess.is_success r then
    List.hd r.OpamProcess.r_stdout
  else
    OpamConsole.error_and_exit `Internal_error "Could not apply cygpath --path to %s" path

let get_cygpath_path_transform =
  (* We are running in a functioning Cygwin or MSYS2 environment if and only
     if `cygpath` is in the PATH. *)
  if Sys.win32 then
    lazy (
      match resolve_command "cygpath" with
      | Some _ -> apply_cygpath_path_transform
      | None -> fun x -> x)
  else
    Lazy.from_val (fun x -> x)

let runs = ref []
let print_stats () =
  match !runs with
  | [] -> ()
  | l  ->
    OpamConsole.msg "%d external processes called:\n%s"
      (List.length l) (OpamStd.Format.itemize ~bullet:"  " (String.concat " ") l)

let log_file ?dir name = temp_file ?dir (OpamStd.Option.default "log" name)

let make_command
    ?verbose ?env ?name ?text ?metadata ?allow_stdin ?stdout
    ?dir ?(resolve_path=true)
    cmd args =
  let env = match env with None -> OpamProcess.default_env () | Some e -> e in
  let name = log_file name in
  let verbose =
    OpamStd.Option.default OpamCoreConfig.(!r.verbose_level >= 2) verbose
  in
  let full_cmd =
    if resolve_path then t_resolve_command ~env ?dir cmd
    else `Cmd cmd
  in
  match full_cmd with
  | `Cmd cmd ->
    OpamProcess.command
      ~env ~name ?text ~verbose ?metadata ?allow_stdin ?stdout ?dir
      cmd args
  | `Not_found -> command_not_found cmd
  | `Denied -> permission_denied cmd

let run_process
    ?verbose ?env ~name ?metadata ?stdout ?allow_stdin command =
  let env = match env with None -> OpamProcess.default_env () | Some e -> e in
  let chrono = OpamConsole.timer () in
  runs := command :: !runs;
  match command with
  | []          -> invalid_arg "run_process"
  | cmd :: args ->
    match t_resolve_command ~env cmd with
    | `Cmd full_cmd ->
      let verbose = match verbose with
        | None   -> OpamCoreConfig.(!r.verbose_level) >= 2
        | Some b -> b in

      let r =
        OpamProcess.run
          (OpamProcess.command
             ~env ~name ~verbose ?metadata ?allow_stdin ?stdout
             full_cmd args)
      in
      let str = String.concat " " (cmd :: args) in
      log ~level:2 "[%a] (in %.3fs) %s"
        (OpamConsole.slog Filename.basename) name
        (chrono ()) str;
      r
    | `Not_found -> command_not_found cmd
    | `Denied -> permission_denied cmd

let command ?verbose ?env ?name ?metadata ?allow_stdin cmd =
  let name = log_file name in
  let r = run_process ?verbose ?env ~name ?metadata ?allow_stdin cmd in
  OpamProcess.cleanup r;
  raise_on_process_error r

let commands ?verbose ?env ?name ?metadata ?(keep_going=false) commands =
  let name = log_file name in
  let run = run_process ?verbose ?env ~name ?metadata in
  let command r0 c =
    match r0, keep_going with
    | (`Error _ | `Exception _), false -> r0
    | _ ->
      let r1 = try
          let r = run c in
          if OpamProcess.is_success r then `Successful r else `Error r
        with Command_not_found _ as e -> `Exception e
      in
      match r0 with `Start | `Successful _ -> r1 | _ -> r0
  in
  match List.fold_left command `Start commands with
  | `Start -> ()
  | `Successful r -> OpamProcess.cleanup r
  | `Error e -> process_error e
  | `Exception e -> raise e

let read_command_output ?verbose ?env ?metadata ?allow_stdin
    ?(ignore_stderr=false) cmd =
  let name = log_file None in
  let stdout = name ^ (if ignore_stderr then ".stdout" else ".out") in
  let r =
    run_process ?verbose ?env ~name ?metadata ?allow_stdin
      ~stdout
      cmd
  in
  OpamProcess.cleanup r;
  raise_on_process_error r;
  r.OpamProcess.r_stdout

let verbose_for_base_commands () =
  OpamCoreConfig.(!r.verbose_level) >= 3

let copy_file_t ?(with_log=true) src dst =
  if (try Sys.is_directory src
      with Sys_error _ -> raise (File_not_found src))
  then internal_error "Cannot copy %s: it is a directory." src;
  if (try Sys.is_directory dst with Sys_error _ -> false)
  then internal_error "Cannot copy to %s: it is a directory." dst;
  if file_or_symlink_exists dst
  then remove_file dst;
  mkdir (Filename.dirname dst);
  if with_log || log_for_file_management () then
    log "copy %s -> %s" src dst;
  copy_file_aux ~src ~dst ()

let rec link_t ?(with_log=true) src dst =
  mkdir (Filename.dirname dst);
  if file_or_symlink_exists dst then
    remove_file dst;
  try
    if with_log || log_for_file_management () then
      log "ln -s %s %s" src dst;
    Unix.symlink src dst
  with Unix.Unix_error (Unix.EXDEV, _, _) ->
    (* Fall back to copy if symlinks are not supported *)
    let src =
      if Filename.is_relative src then Filename.dirname dst / src
      else src
    in
    if Sys.is_directory src then
      copy_dir_t src dst
    else
      copy_file_t src dst

and copy_dir_t ?(with_log=true) src dst =
  if with_log || log_for_file_management () then
    log "copydir %s -> %s" src dst;
  let files = get_files src in
  mkdir dst;
  let with_log = false in
  List.iter (fun file ->
      let src = Filename.concat src file in
      let dst = Filename.concat dst file in
      match Unix.lstat src with
      | {Unix.st_kind = Unix.S_REG; _} ->
        copy_file_t ~with_log src dst
      | {Unix.st_kind = Unix.S_DIR; _} ->
        copy_dir_t ~with_log src dst
      | {Unix.st_kind = Unix.S_LNK; _} ->
        let src = Unix.readlink src in
        link_t ~with_log src dst
      | {Unix.st_kind = Unix.S_CHR; _} ->
        failwith (Printf.sprintf "Copying character devices (%s) is unsupported" src)
      | {Unix.st_kind = Unix.S_BLK; _} ->
        failwith (Printf.sprintf "Copying block devices (%s) is unsupported" src)
      | {Unix.st_kind = Unix.S_FIFO; _} ->
        failwith (Printf.sprintf "Copying named pipes (%s) is unsupported" src)
      | {Unix.st_kind = Unix.S_SOCK; _} ->
        failwith (Printf.sprintf "Copying sockets (%s) is unsupported" src)
    ) files

let copy_dir = copy_dir_t ~with_log:true
let copy_file = copy_file_t ~with_log:true

let mv src dst =
  if file_or_symlink_exists dst then remove_file dst;
  mkdir (Filename.dirname dst);
  log "mv %s -> %s" src dst;
  try
    Unix.rename src dst
  with
  | Unix.Unix_error(Unix.EXDEV, _, _) ->
    let with_log = false in
    if Sys.is_directory src
    then (copy_dir_t ~with_log src dst; remove_dir_t src)
    else (copy_file_t ~with_log src dst; remove_file_t ~with_log src)

let is_exec file =
  let stat = Unix.stat file in
  stat.Unix.st_kind = Unix.S_REG &&
  stat.Unix.st_perm land 0o111 <> 0

let file_is_empty f = Unix.((stat f).st_size = 0)

let classify_executable file =
  let c = open_in file in
  (* On a 32-bit system, this could fail for a PE image with a 2GB+ DOS header =-o *)
  let input_int_little c =
    let b1 = input_byte c in
    let b2 = input_byte c in
    let b3 = input_byte c in
    let b4 = input_byte c in
    b1 lor (b2 lsl 8) lor (b3 lsl 16) lor (b4 lsl 24) in
  let input_short_little c =
    let b1 = input_byte c in
    let b2 = input_byte c in
    b1 lor (b2 lsl 8) in
  set_binary_mode_in c true;
  try
    match really_input_string c 2 with
      "#!" ->
        close_in c;
        `Script
    | "MZ" ->
        let is_pe =
          try
            (* Offset to PE header at 0x3c (but we've already read two bytes) *)
            ignore (really_input_string c 0x3a);
            ignore (really_input_string c (input_int_little c - 0x40));
            let magic = really_input_string c 4 in
            magic = "PE\000\000"
          with End_of_file ->
            close_in c;
            false in
        if is_pe then
          try
            let arch =
              (* NB It's not necessary to determine PE/PE+ headers for x64/x86 determination *)
              match input_short_little c with
                0x8664 ->
                  `x86_64
              | 0x14c ->
                  `x86
              | _ ->
                  raise End_of_file
            in
            ignore (really_input_string c 14);
            let size_of_opt_header = input_short_little c in
            let characteristics = input_short_little c in
            (* Executable images must have a PE "optional" header and be marked executable *)
            (* Could also validate IMAGE_FILE_32BIT_MACHINE (0x100) for x86 and IMAGE_FILE_LARGE_ADDRESS_AWARE (0x20) for x64 *)
            if size_of_opt_header <= 0 || characteristics land 0x2 = 0 then
              raise End_of_file;
            close_in c;
            if characteristics land 0x2000 <> 0 then
              `Dll arch
            else
              `Exe arch
          with End_of_file ->
            close_in c;
            `Unknown
        else
          `Exe `i386
    | _ ->
        close_in c;
        `Unknown
  with End_of_file ->
    close_in c;
    `Unknown

let default_install_warning dst = function
  | `Add_exe ->
    OpamConsole.warning "Automatically adding .exe to %s" dst
  | `Install_dll ->
    (* TODO Installation of .dll to bin is unfortunate, but not sure if it
       should be a warning *)
    ()
  | `Install_script ->
    (* TODO Generate a .cmd wrapper (and warn about it - they're not perfect) *)
    OpamConsole.warning "%s is a script; the command won't be available" dst;
  | `Install_unknown ->
    (* TODO Installation of a non-executable file is unexpected, but not sure
       if it should be a warning/error *)
    ()
  | `Cygwin ->
    OpamConsole.warning "%s is a Cygwin-linked executable" dst
  | `Msys2 ->
    OpamConsole.warning "%s is a MSYS2-linked executable" dst
  | `Tainted `Cygwin ->
    OpamConsole.warning
      "%s is an executable which links to a Cygwin-linked library" dst
  | `Tainted `Msys2 ->
    OpamConsole.warning
      "%s is an executable which links to a MSYS2-linked library" dst
  | `Cygwin_libraries ->
    OpamConsole.warning
      "%s links with a Cygwin-compiled DLL (almost certainly a packaging \
       or environment error)" dst

let install ?(warning=default_install_warning) ?exec src dst =
  if Sys.is_directory src then
    internal_error "Cannot install %s: it is a directory." src;
  if (try Sys.is_directory dst with Sys_error _ -> false) then
    internal_error "Cannot install to %s: it is a directory." dst;
  mkdir (Filename.dirname dst);
  let exec = match exec with
    | Some e -> e
    | None -> is_exec src in
  let perm = if exec then 0o755 else 0o644 in
  log "install %s -> %s (%o)" src dst perm;
  if Sys.win32 then
    if exec then begin
      let (dst, cygcheck) =
        match classify_executable src with
          `Exe _ ->
            if not (Filename.check_suffix dst ".exe") && not (Filename.check_suffix dst ".dll") then begin
              warning dst `Add_exe;
              (dst ^ ".exe", true)
            end else
              (dst, true)
        | `Dll _ ->
            warning dst `Install_dll;
            (dst, true)
        | `Script ->
            warning dst `Install_script;
            (dst, false)
        | `Unknown ->
            warning dst `Install_unknown;
            (dst, false)
      in
      copy_file_aux ~src ~dst ();
      if cygcheck then
        match OpamStd.Sys.get_windows_executable_variant
                ~cygbin:OpamCoreConfig.(!r.cygbin) dst with
        | `Native -> ()
        | (`Cygwin | `Msys2 | `Tainted _) as code -> warning dst code
    end else
      copy_file_aux ~src ~dst ()
  else
    copy_file_aux ~chmod:(fun _ -> perm) ~src ~dst ()

let cpu_count () =
  try
    let ans =
      let open OpamStd in
      match Sys.os () with
      | Sys.Win32 -> [Env.get "NUMBER_OF_PROCESSORS"]
      | Sys.FreeBSD -> read_command_output ~verbose:(verbose_for_base_commands ())
                         ["sysctl"; "-n"; "hw.ncpu"]
      | _ -> read_command_output ~verbose:(verbose_for_base_commands ())
               ["getconf"; "_NPROCESSORS_ONLN"]
    in
    int_of_string (List.hd ans)
  with Not_found | Process_error _ | Failure _ -> 1

open OpamProcess.Job.Op

module Tar = struct

  type extract =
    | Bzip2
    | Gzip
    | Lzma
    | Xz

  let extract_command = function
    | Bzip2 -> "bzip2"
    | Gzip -> "gzip"
    | Lzma -> "lzma"
    | Xz -> "xz"

  let extract_option = function
    | Bzip2 -> 'j'
    | Gzip -> 'z'
    | Lzma -> 'Y'
    | Xz -> 'J'

  let extensions =
    [ [ "tar.gz" ; "tgz" ], Gzip
    ; [ "tar.bz2" ; "tbz" ], Bzip2
    ; [ "tar.xz" ; "txz" ], Xz
    ; [ "tar.lzma" ; "tlz" ], Lzma
    ]

  let guess_type f =
    try
      let ic = open_in f in
      let c1 = input_char ic in
      let c2 = input_char ic in
      close_in ic;
      match c1, c2 with
      | '\031', '\139' -> Some Gzip
      | 'B'   , 'Z'    -> Some Bzip2
      | '\xfd', '\x37' -> Some Xz
      | '\x5d', '\x00' -> Some Lzma
      | _              -> None
    with Sys_error _ -> None

  let match_ext file ext =
    List.exists (Filename.check_suffix file) ext

  let get_type file =
    let ext =
      List.fold_left
        (fun acc (ext, t) -> match acc with
           | Some t -> Some t
           | None   ->
             if match_ext file ext
             then Some t
             else None)
        None
        extensions in
    if Sys.file_exists file then guess_type file
    else ext

  let is_archive file =
    get_type file <> None

  let check_extract file =
    OpamStd.Option.Op.(
      get_type file >>= fun typ ->
      let cmd = extract_command typ in
      let res = resolve_command cmd <> None in
      if not res then
        Some (Printf.sprintf "Tar needs %s to extract the archive" cmd)
      else None)

  let tar_cmd = lazy (
    match OpamStd.Sys.os () with
    | OpamStd.Sys.OpenBSD -> "gtar"
    | _ -> "tar"
  )

  let cygpath_tar = lazy (
    Lazy.force (get_cygpath_function ~command:(Lazy.force tar_cmd))
  )

  let extract_command =
    fun file ->
      OpamStd.Option.Op.(
        get_type file >>| fun typ ->
        let f = Lazy.force cygpath_tar in
        let tar_cmd = Lazy.force tar_cmd in
        let command c dir =
          make_command tar_cmd [ Printf.sprintf "xf%c" c ; f file; "-C" ; f dir ]
        in
        command (extract_option typ))

  let compress_command =
    fun file dir ->
      let f = Lazy.force cygpath_tar in
      let tar_cmd = Lazy.force tar_cmd in
      make_command tar_cmd [
        "cfz"; f file;
        "-C" ; f (Filename.dirname dir);
        f (Filename.basename dir)
      ]

end

module Zip = struct

  let is_archive f =
    if Sys.file_exists f then
      try
        let ic = open_in f in
        let c1 = input_char ic in
        let c2 = input_char ic in
        let c3 = input_char ic in
        let c4 = input_char ic in
        close_in ic;
        match c1, c2, c3, c4 with
        | '\x50', '\x4b', '\x03', '\x04' -> true
        | _ -> false
      with Sys_error _ | End_of_file -> false
    else
      Filename.check_suffix f "zip"

  let extract_command file =
    Some (fun dir -> make_command "unzip" [ file; "-d"; dir ])
end

let is_archive file =
  Tar.is_archive file || Zip.is_archive file

let extract_command file =
  if Zip.is_archive file then Zip.extract_command file
  else Tar.extract_command file

let make_tar_gz_job ~dir file =
  let tmpfile = file ^ ".tmp" in
  remove_file tmpfile;
  Tar.compress_command tmpfile dir @@> fun r ->
  OpamProcess.cleanup r;
  if OpamProcess.is_success r then
    (mv tmpfile file; Done None)
  else
    (remove_file tmpfile; Done (Some (Process_error r)))

let extract_job ~dir file =
  if not (Sys.file_exists file) then
    Done (Some (File_not_found file))
  else
  with_tmp_dir_job @@ fun tmp_dir ->
  match extract_command file with
  | None   ->
    Done (Some (Failure ("Unknown archive type: "^file)))
  | Some cmd ->
    cmd tmp_dir @@> fun r ->
    if not (OpamProcess.is_success r) then
      if Zip.is_archive file then
        Done (Some (Process_error r))
      else match Tar.check_extract file with
        | None -> Done (Some (Process_error r))
        | Some s -> Done (Some (Failure s))
    else if try not (Sys.is_directory dir) with Sys_error _ -> false then
      internal_error "Extracting the archive would overwrite %s." dir
    else
    let flist =
      OpamStd.Op.(
        files_all_not_dir tmp_dir |>
        List.filter (not @* OpamStd.String.contains ~sub:"pax_global_header"))
    in
    match flist with
    | [] ->
      begin match directories_strict tmp_dir with
        | [x] ->
          (try
             mkdir (Filename.dirname dir);
             copy_dir x dir;
             Done None
           with e -> OpamStd.Exn.fatal e; Done (Some e))
        | _ ->
          internal_error "The archive %S contains multiple root directories."
            file
      end
    | _   ->
      mkdir (Filename.dirname dir);
      try copy_dir tmp_dir dir; Done None
      with e -> OpamStd.Exn.fatal e; Done (Some e)

let extract ~dir file =
  match OpamProcess.Job.run (extract_job ~dir file) with
  | Some e -> raise e
  | None -> ()

let extract_in_job ~dir file =
  OpamProcess.Job.catch (fun e -> Done (Some e)) @@ fun () ->
  mkdir dir;
  match extract_command file with
  | None -> internal_error "%s is not a valid tar or zip archive." file
  | Some cmd ->
    cmd dir @@> fun r ->
    if not (OpamProcess.is_success r) then
      if Zip.is_archive file then
        Done (Some (Process_error r))
      else match Tar.check_extract file with
        | None ->
          Done (Some (Failure
                        (Printf.sprintf "Failed to extract archive %s: %s" file
                           (OpamProcess.result_summary r))))
        | Some s -> Done (Some (Failure s))
    else Done None

let extract_in ~dir file =
  match OpamProcess.Job.run (extract_in_job ~dir file) with
  | Some e -> raise e
  | None -> ()

let link src dst =
  let fallback () =
    (* Fall back to copy if symlinks are not supported *)
    let src =
      if Filename.is_relative src then Filename.dirname dst / src
      else src
    in
    if Sys.is_directory src then
      copy_dir src dst
    else
      copy_file src dst
  in
  mkdir (Filename.dirname dst);
  if file_or_symlink_exists dst then (
    (* TODO: move this into remove_file *)
    if Sys.win32 then Unix.chmod dst 0o640;
    remove_file dst
  );
  if Unix.has_symlink () then
    try
      log "ln -s %s %s" src dst;
      Unix.symlink src dst
    with Unix.Unix_error (Unix.EXDEV, _, _) ->
      fallback ()
  else (
    log "copy %s -> %s" src dst;
    fallback ()
  )

type actual_lock_flag = [ `Lock_read | `Lock_write ]
type lock_flag = [ `Lock_none | actual_lock_flag ]

type lock = {
  mutable fd: Unix.file_descr option;
  file: string;
  mutable kind: lock_flag;
}

exception Locked

let unix_lock_op ~dontblock = function
  | `Lock_read -> if dontblock then Unix.F_TRLOCK else Unix.F_RLOCK
  | `Lock_write ->
    if OpamCoreConfig.(!r.safe_mode) then
      OpamConsole.error_and_exit `Locked "Write lock attempt in safe mode"
    else
    if dontblock then Unix.F_TLOCK else Unix.F_LOCK

let string_of_lock_kind = function
  | `Lock_none -> "none"
  | `Lock_read -> "read"
  | `Lock_write -> "write"

let locks = Hashtbl.create 16

let release_all_locks () =
  Hashtbl.iter (fun fd _ -> Unix.close fd) locks;
  Hashtbl.clear locks

let rec flock_update
  : 'a. ([< lock_flag ] as 'a) -> ?dontblock:bool -> lock -> unit
  = fun flag ?(dontblock=OpamCoreConfig.(!r.safe_mode)) lock ->
  log "LOCK %s (%a => %a)" ~level:2 lock.file
    (slog string_of_lock_kind) (lock.kind)
    (slog string_of_lock_kind) flag;
  if lock.kind = (flag :> lock_flag) then ()
  else
  match flag, lock with
  | `Lock_none, { fd = Some fd; kind = (`Lock_read | `Lock_write); _ } ->
    Hashtbl.remove locks fd;
    Unix.close fd; (* implies Unix.lockf fd Unix.F_ULOCK 0 *)
    lock.kind <- (flag :> lock_flag);
    lock.fd <- None
  | (`Lock_read | `Lock_write), { fd = None; kind = `Lock_none; file } ->
    let new_lock = flock flag ~dontblock file in
    lock.kind <- (flag :> lock_flag);
    lock.fd <- new_lock.fd
  | `Lock_write, { fd = Some fd; file; kind = `Lock_read } ->
    Unix.close fd; (* fd needs read-write reopen *)
    let new_lock = flock flag ~dontblock file in
    lock.kind <- (flag :> lock_flag);
    lock.fd <- new_lock.fd
  | (`Lock_read | `Lock_write) as flag, { fd = Some fd; file; kind } ->
    (* Write locks are not recursive on Windows, so only call lockf if necessary *)
    if kind <> flag then
      (try
         (* Locks can't be promoted (or demoted) on Windows - see PR#7264 *)
         if Sys.win32 && kind <> `Lock_none then
           Unix.(lockf fd F_ULOCK 0);
         Unix.lockf fd (unix_lock_op ~dontblock:true flag) 0
       with Unix.Unix_error (Unix.EAGAIN,_,_)
          | Unix.Unix_error (Unix.EACCES,_,_) ->
         if dontblock then
           OpamConsole.error_and_exit `Locked
             "Another process has locked %s and non blocking mode enabled"
             file;
         OpamConsole.formatted_msg
           "Another process has locked %s, waiting (%s to abort)... "
           file (if Sys.win32 then "CTRL+C" else "C-c");
         let rec lock_w_ignore_sig () =
           try Unix.lockf fd (unix_lock_op ~dontblock:false flag) 0;
           with Sys.Break as e -> (OpamConsole.msg "\n"; raise e)
              | Unix.Unix_error (Unix.EINTR,_,_) -> lock_w_ignore_sig ()
         in lock_w_ignore_sig ();
         OpamConsole.msg "lock acquired.\n");
    lock.kind <- (flag :> lock_flag)
  | _ -> assert false

and flock: 'a. ([< lock_flag ] as 'a) -> ?dontblock:bool -> string -> lock =
  fun flag ?dontblock file ->
  match flag with
  | `Lock_none -> { fd = None; file; kind = `Lock_none }
  | `Lock_write when OpamCoreConfig.(!r.safe_mode) ->
    OpamConsole.error_and_exit `Locked "Write lock attempt in safe mode";
  | flag ->
    mkdir (Filename.dirname file);
    let rdflag = if (flag :> lock_flag) = `Lock_write then Unix.O_RDWR else Unix.O_RDONLY in
    let fd = Unix.openfile file Unix.([O_CREAT; O_CLOEXEC; O_SHARE_DELETE; rdflag]) 0o666 in
    Hashtbl.add locks fd ();
    let lock = { fd = Some fd; file; kind = `Lock_none } in
    flock_update flag ?dontblock lock;
    lock

let funlock lock = flock_update `Lock_none lock

let get_lock_flag lock = lock.kind

let get_lock_fd lock =
  match lock.fd with
    Some fd -> fd
  | None -> raise Not_found

let lock_max flag1 flag2 = match flag1, flag2 with
  | `Lock_write, _ | _, `Lock_write -> `Lock_write
  | `Lock_read, _ | _, `Lock_read -> `Lock_read
  | `Lock_none, `Lock_none -> `Lock_none

let lock_none = {
  fd = None;
  file = "";
  kind = `Lock_none;
}

let lock_isatleast flag lock =
  lock_max flag lock.kind = lock.kind

let get_eol_encoding file =
  let ch =
    try open_in_bin file
    with Sys_error _ -> raise (File_not_found file)
  in
  let has_cr line =
    let length = String.length line in
    length > 0 && line.[length - 1] = '\r'
  in
  let last_char ch = seek_in ch (in_channel_length ch - 1); input_char ch in
  let rec read_lines cr line =
    let has_cr = has_cr line in
    match input_line ch with
    | line ->
        if has_cr = cr then
          read_lines cr line
        else begin
          close_in ch;
          None
        end
    | exception End_of_file ->
        let result =
          if cr = has_cr then
            Some cr
          else
            if cr && last_char ch <> '\n' then
              Some true
            else
              None
        in
        close_in ch;
        result
  in
  match input_line ch with
  | line_one ->
      let has_cr = has_cr line_one in
      begin match input_line ch with
      | line_two ->
          read_lines has_cr line_two
      | exception End_of_file ->
          let result =
            if last_char ch = '\n' then
              Some has_cr
            else
              None
          in
          close_in ch;
          result
      end
  | exception End_of_file ->
      close_in ch;
      None

let translate_patch ~dir orig corrected =
  (* It's unnecessarily complicated to infer whether the entire file is CRLF
     encoded and also the status of individual files, so accept scanning the
     file three times instead of two. *)
  let log ?level fmt = OpamConsole.log "PATCH" ?level fmt in
  let strip_cr = get_eol_encoding orig = Some true in
  let ch =
    try open_in_bin orig
    with Sys_error _ -> raise (File_not_found orig)
  in
  (* CRLF detection with patching can be more complicated than that used here,
     especially in the presence of files with mixed LF/CRLF endings. The
     processing done here aims to allow patching to succeed on files which are
     wholly encoded CRLF or LF against patches which may have been translated to
     be the opposite.

     The resulting patch will *always* have LF line endings for the patch
     metadata (headers, chunk locations, etc.) but uses either CRLF or LF
     depending on the target file. Endings in the patch are always preserved for
     new files. The benefit of always using LF endings for the metadata is that
     patch's "Stripping trailing CRs from patch" behaviour won't be triggered.

     There are various patch formats, though only the Unified and Context
     formats allow multiple files to be patched. I tired of trying to get
     sufficient documented detail of Context diffs to be able to parse them
     without resorting to reverse-engineering code. It is unusual to see them
     these days, so for now opam just emits a warning if a Context diff file is
     encountered and does no processing to it.

     There are various semantic aspects of Unified diffs which are not handled
     (at least at present) by this function which are documented in the code
     with the marker "Weakness". *)
  let process_chunk_header result line =
    match OpamStd.String.split line ' ' with
    | "@@"::a::b::"@@"::_ ->
        (* Weakness: for a new file [a] should always be -0,0 (not checked) *)
        let l_a = String.length a in
        let l_b = String.length b in
        if l_a > 1 && l_b > 1 && a.[0] = '-' && b.[0] = '+' then
          try
            let f (_, v) = int_of_string v in
            let neg =
              OpamStd.String.cut_at (String.sub a 1 (l_a - 1)) ','
                      |> OpamStd.Option.map_default f 1
            in
            let pos =
              OpamStd.String.cut_at (String.sub b 1 (l_b - 1)) ','
                      |> OpamStd.Option.map_default f 1
            in
            result neg pos
          with e ->
            OpamStd.Exn.fatal e;
            (* TODO Should display some kind of re-sync warning *)
            `Header
        else
          (* TODO Should display some kind of re-sync warning *)
          `Header
    | _ ->
        (* TODO Should display some kind of warning that there were no chunks *)
        `Header
  in
  let process_state_transition next_state state transforms =
    match (state, next_state) with
    | (`Processing _, `Processing _) ->
        transforms
    | (`Processing (_, target, crlf, patch_crlf, chunks, _), _) ->
        let compute_transform patch_crlf =
          (* Emit the patch *)
          let transform =
            match (crlf, patch_crlf) with
            | (None, _)
            | (_, None) ->
                log ~level:3 "CRLF adaptation skipped for %s" target;
                None
            | (Some crlf, Some patch_crlf) ->
                if crlf = patch_crlf then begin
                  log ~level:3 "No CRLF adaptation necessary for %s" target;
                  None
                end else if crlf then begin
                  log ~level:3 "Adding \\r to patch chunks for %s" target;
                  Some true
                end else begin
                  log ~level:3 "Stripping \\r to patch chunks for %s" target;
                  Some false
                end
          in
          let record_transform transform =
            let augment_record (first_line, last_line) =
              (first_line, last_line, transform)
            in
            List.rev_append (List.rev_map augment_record chunks) transforms
          in
          OpamStd.Option.map_default record_transform transforms transform
        in
        OpamStd.Option.map_default compute_transform transforms patch_crlf
     | _ ->
        transforms
  in
  let rec fold_lines state n transforms =
    match input_line ch with
    | line ->
        let line =
          if strip_cr then
            String.sub line 0 (String.length line - 1)
          else
            line
        in
        let length = String.length line in
        let next_state =
          match state with
          | `Header ->
            begin
              match (if length > 4 then String.sub line 0 4 else "") with
              | "--- " ->
                  (* Start of a unified diff header. *)
                  let file =
                    let file = String.sub line 4 (length - 4) in
                    let open OpamStd in
                    Option.map_default fst file (String.cut_at file '\t')
                  in
                  (* Weakness: new files are also marked with a time-stamp at
                               the start of the epoch, however it's localised,
                               making it a bit tricky to identify! New files are
                               also identified by their absence on disk, so this
                               weakness isn't particularly critical. *)
                  if file = "/dev/null" then
                    `NewHeader
                  else
                    let target =
                      OpamStd.String.cut_at (back_to_forward file) '/'
                      |> OpamStd.Option.map_default snd file
                      |> Filename.concat dir
                    in
                    if Sys.file_exists target then
                      let crlf = get_eol_encoding target in
                      `Patching (file, crlf)
                    else
                      `NewHeader
              | "*** " ->
                  OpamConsole.warning "File %s uses context diffs which are \
                                       less portable; consider using unified \
                                       diffs" orig;
                  `SkipFile
              | _ ->
                  (* Headers will contain other lines, which are ignored (e.g.
                     the diff command which generated the diff, or Git commit
                     messages) *)
                  `Header
            end
          | `NewHeader ->
              if (if length > 4 then String.sub line 0 4 else "") = "+++ " then
                `New
              else
                (* TODO Should display some kind of re-sync warning *)
                `Header
          | `New ->
              process_chunk_header (fun neg pos -> `NewChunk (neg, pos))
                                   line
          | `NewChunk (neg, pos) ->
              (* Weakness: new files should only have + lines *)
              let neg =
                if line = "" || line.[0] = ' ' || line.[0] = '-' then
                  neg - 1
                else
                  neg
              in
              let pos =
                if line = "" || line.[0] = ' ' || line.[0] = '+' then
                  pos - 1
                else
                  pos
              in
              if neg = 0 && pos = 0 then
                `New
              else
                (* Weakness: there should only be one chunk for a new file *)
                `NewChunk (neg, pos)
          | `Patching (orig, crlf) ->
              if (if length > 4 then String.sub line 0 4 else "") = "+++ " then
                let file =
                  let file = String.sub line 4 (length - 4) in
                  let open OpamStd in
                  Option.map_default fst file (String.cut_at file '\t')
                in
                `Processing (orig, file, crlf, None, [], `Head)
              else
                `Header
          | `Processing (orig, target, crlf, patch_crlf, chunks, `Head) ->
              if line = "\\ No newline at end of file" then
                (* If the no eol-at-eof indicator is found, never add \r to
                   final chunk line *)
                let chunks =
                  match chunks with
                  | (a, b)::chunks ->
                      (a, b - 1)::chunks
                  | _ ->
                      chunks
                in
                `Processing (orig, target, crlf, patch_crlf, chunks, `Head)
             else
                process_chunk_header
                  (fun neg pos ->
                     `Processing (orig, target, crlf, patch_crlf, chunks,
                                  `Chunk (succ n, neg, pos)))
                  line
          | `Processing (orig, target, crlf, patch_crlf, chunks,
                         `Chunk (first_line, neg, pos)) ->
              let neg =
                if line = "" || line.[0] = ' ' || line.[0] = '-' then
                  neg - 1
                else
                  neg
              in
              let pos =
                if line = "" || line.[0] = ' ' || line.[0] = '+' then
                  pos - 1
                else
                  pos
              in
              let patch_crlf =
                let has_cr = (length > 0 && line.[length - 1] = '\r') in
                match patch_crlf with
                | None ->
                    Some (Some has_cr)
                | Some (Some think_cr) when think_cr <> has_cr ->
                    log ~level:2 "Patch adaptation disabled for %s: \
                                  mixed endings or binary file" target;
                    Some None
                | _ ->
                    patch_crlf
              in
              if neg = 0 && pos = 0 then
                let chunks = (first_line, n)::chunks in
                `Processing (orig, target, crlf, patch_crlf, chunks, `Head)
              else
                `Processing (orig, target, crlf, patch_crlf, chunks,
                             `Chunk (first_line, neg, pos))
          | `SkipFile ->
              `SkipFile
        in
        if next_state = `SkipFile then
          []
        else
          process_state_transition next_state state transforms
            |> fold_lines next_state (succ n)
    | exception End_of_file ->
        process_state_transition `Header state transforms |> List.rev
  in
  let transforms = fold_lines `Header 1 [] in
  if transforms = [] then
    copy_file orig corrected
  else begin
    seek_in ch 0;
    let ch_out =
      try open_out_bin corrected
      with Sys_error _ ->
        close_in ch;
        raise (File_not_found corrected)
    in
    let (normal, add_cr, strip_cr) =
      let strip n s = String.sub s 0 (String.length s - n) in
      let id x = x in
      if strip_cr then
        (strip 1, id, strip 2)
      else
        (id, (fun s -> s ^ "\r"), strip 1)
    in
    if OpamConsole.debug () then
      let log_transform (first_line, last_line, add_cr) =
         let indicator = if add_cr then '+' else '-' in
         log ~level:3 "Transform %d-%d %c\\r" first_line last_line indicator
      in
      List.iter log_transform transforms;
    let rec fold_lines n transforms =
      match input_line ch with
      | line ->
          let (f, transforms) =
            match transforms with
            | (first_line, last_line, add_cr_to_chunks)::next_transforms ->
                let transforms =
                  if n = last_line then
                    next_transforms
                  else
                    transforms
                in
                let f =
                  if n >= first_line then
                    if add_cr_to_chunks then
                      add_cr
                    else
                      strip_cr
                  else
                    normal
                in
                (f, transforms)
             | [] ->
                 (normal, [])
          in
          output_string ch_out (f line);
          output_char ch_out '\n';
          fold_lines (succ n) transforms
      | exception End_of_file ->
          close_out ch_out
    in
    fold_lines 1 transforms
  end;
  close_in ch

let patch ?(preprocess=true) ~dir p =
  if not (Sys.file_exists p) then
    (OpamConsole.error "Patch file %S not found." p;
     raise Not_found);
  let p' =
    if preprocess then
      let p' = temp_file ~auto_clean:false "processed-patch" in
      translate_patch ~dir p p';
      p'
    else
      p
  in
  let patch_cmd =
    match OpamStd.Sys.os () with
    | OpamStd.Sys.OpenBSD
    | OpamStd.Sys.FreeBSD -> "gpatch"
    | _ -> "patch"
  in
  make_command ~name:"patch" ~dir patch_cmd ["-p1"; "-i"; p'] @@> fun r ->
    if not (OpamConsole.debug ()) then Sys.remove p';
    if OpamProcess.is_success r then Done None
    else Done (Some (Process_error r))

let register_printer () =
  Printexc.register_printer (function
    | Process_error r     -> Some (OpamProcess.result_summary r)
    | Internal_error m    -> Some m
    | Command_not_found c -> Some (Printf.sprintf "%S: command not found." c)
    | Permission_denied c -> Some (Printf.sprintf "%S: permission denied." c)
    | Sys.Break           -> Some "User interruption"
    | Unix.Unix_error (e, fn, msg) ->
      let msg = if msg = "" then "" else " on " ^ msg in
      let error = Printf.sprintf "%s: %S failed%s: %s"
          Sys.executable_name fn msg (Unix.error_message e) in
      Some error
    | _ -> None
  )

let init () =
  register_printer ();
  Sys.catch_break true;
  try Sys.set_signal Sys.sigpipe (Sys.Signal_handle (fun _ -> ()))
  with Invalid_argument _ -> ()

let () =
  OpamProcess.set_resolve_command resolve_command
