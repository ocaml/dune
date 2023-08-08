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

module Re = Dune_re

let log ?level fmt =
  OpamConsole.log "PROC" ?level fmt

let default_env =
  let f () = lazy (
    match OpamCoreConfig.(!r.cygbin) with
    | Some cygbin -> OpamStd.Env.cyg_env cygbin
    | None -> OpamStd.Env.raw_env ()
  ) in
 fun () -> Lazy.force (f ())

let cygwin_create_process_env prog args env fd1 fd2 fd3 =
  (*
   * Unix.create_process_env correctly converts arguments to a command line for
   * native Windows execution, but it does not correctly handle Cygwin's quoting
   * requirements.
   *
   * The process followed here is based on an analysis of the sources for the
   * Cygwin DLL (git://sourceware.org/git/newlib-cygwin.git,
   * winsup/cygwin/dcrt0.cc) and a lack of refutation on the Cygwin mailing list
   * in May 2016!
   *
   * In case this seems terminally stupid, it's worth noting that Cygwin's
   * implementation of the exec system calls do not pass argv using the Windows
   * command line, these weird and wonderful rules exist for corner cases and,
   * as here, for invocations from native Windows processes.
   *
   * There are two forms of escaping which can apply, controlled by the CYGWIN
   * environment variable option noglob.
   *
   * If none of the strings in argv contains the double-quote character, then
   * the process should be invoked with the noglob option (to ensure that no
   * characters are unexpectedly expanded). In this mode of escaping, it is
   * necessary to protect any whitespace characters (\r, \n, \t, and space
   * itself) by surrounding sequences of them with double-quotes). Additionally,
   * if any string in argv begins with the @ sign, this should be double-quoted.
   *
   * If any one of the strings in argv does contain a double-quote character,
   * then the process should be invoked with the glob option (this is the
   * default). Every string in argv should have double-quotes put around it. Any
   * double-quote characters within the string should be translated to "'"'".
   *
   * The reason for supporting both mechanisms is that the noglob method has
   * shorter command lines and Windows has an upper limit of 32767 characters
   * for the command line.
   *
   * COMBAK If the command line does exceed 32767 characters, then Cygwin allows
   *        a parameter beginning @ to refer to a file from which to read the
   *        rest of the command line. This support is not implemented at this
   *        time in OPAM.
   *
   * [This stray " is here to terminate a previous part of the comment!]
   *)
  let make_args argv =
    let b = Buffer.create 128 in
    let gen_quote ~quote ~pre ?(post = pre) s =
      log ~level:3 "gen_quote: %S" s;
      Buffer.clear b;
      let l = String.length s in
      let rec f i =
        let j =
          try
            OpamStd.String.find_from (fun c -> try String.index quote c >= 0 with Not_found -> false) s (succ i)
          with Not_found ->
            l in
        Buffer.add_string b (String.sub s i (j - i));
        if j < l then begin
          Buffer.add_string b pre;
          let i = j in
          let j =
            try
              OpamStd.String.find_from (fun c -> try String.index quote c < 0 with Not_found -> true) s (succ i)
            with Not_found ->
              l in
          Buffer.add_string b (String.sub s i (j - i));
          Buffer.add_string b post;
          if j < l then
            f j
          else
            Buffer.contents b
        end else
          Buffer.contents b in
      let r =
        if s = "" then
          "\"\""
        else
          f 0
      in
      log ~level:3 "result: %S" r; r in
    (* Setting noglob is causing some problems for ocamlbuild invoking Cygwin's
       find. The reason for using it is to try to keep command line lengths
       below the maximum, but for now disable the use of noglob. *)
    if true || List.exists (fun s -> try String.index s '"' >= 0 with Not_found -> false) argv then
      ("\"" ^ String.concat "\" \"" (List.map (gen_quote ~quote:"\"" ~pre:"\"'" ~post:"'\"") argv) ^ "\"", false)
    else
      (String.concat " " (List.map (gen_quote ~quote:"\b\r\n " ~pre:"\"") argv), true) in
  let (command_line, no_glob) = make_args (Array.to_list args) in
  log "cygvoke(%sglob): %s" (if no_glob then "no" else "") command_line;
  let env = Array.to_list env in
  let set = ref false in
  let f item =
    let (key, value) =
      match OpamStd.String.cut_at item '=' with
        Some pair -> pair
      | None -> (item, "") in
    match String.lowercase_ascii key with
    | "cygwin" ->
        let () =
          if key = "CYGWIN" then
            set := true in
        let settings = OpamStd.String.split value ' ' in
        let set = ref false in
        let f setting =
          let setting = String.trim setting in
          let setting =
            match OpamStd.String.cut_at setting ':' with
              Some (setting, _) -> setting
            | None -> setting in
          match setting with
            "glob" ->
              if no_glob then begin
                log ~level:2 "Removing glob from %s" key;
                false
              end else begin
                log ~level:2 "Leaving glob in %s" key;
                set := true;
                true
              end
          | "noglob" ->
              if no_glob then begin
                log ~level:2 "Leaving noglob in %s" key;
                set := true;
                true
              end else begin
                log ~level:2 "Removing noglob from %s" key;
                false
              end
          | _ ->
              true in
        let settings = List.filter f settings in
        let settings =
          if not !set && no_glob then begin
            log ~level:2 "Setting noglob in %s" key;
            "noglob"::settings
          end else
            settings in
        if settings = [] then begin
          log ~level:2 "Removing %s completely" key;
          None
        end else
          Some (key ^ "=" ^ String.concat " " settings)
    | "path" ->
        let path_dirs = OpamStd.Sys.split_path_variable value in
        let winsys = Filename.concat (OpamStd.Sys.system ()) "." |> String.lowercase_ascii in
        let rec f prefix suffix = function
        | dir::dirs ->
            let contains_cygpath = Sys.file_exists (Filename.concat dir "cygpath.exe") in
            if suffix = [] then
              if String.lowercase_ascii (Filename.concat dir ".") = winsys then
                f prefix [dir] dirs
              else
                if contains_cygpath then
                  path_dirs
                else
                  f (dir::prefix) [] dirs
            else
              if contains_cygpath then begin
                log ~level:2 "Moving %s to after %s in PATH" dir (List.hd prefix);
                List.rev_append prefix (dir::(List.rev_append suffix dirs))
              end else
                f prefix (dir::suffix) dirs
        | [] -> []
        in
        Some (key ^ "=" ^ String.concat ";" (f [] [] path_dirs))
    | _ ->
        Some item in
  let env = OpamStd.List.filter_map f env in
  let env =
    if !set then
      env
    else
      if no_glob then begin
        log ~level:2 "Adding CYGWIN=noglob";
        "CYGWIN=noglob"::env
      end else
        env in
  OpamStubs.win_create_process prog command_line
                               (Some(String.concat "\000" env ^ "\000"))
                               fd1 fd2 fd3

(** Shell commands *)
type command = {
  cmd: string;
  args: string list;
  cmd_text: string option;
  cmd_dir: string option;
  cmd_env: string array option;
  cmd_stdin: bool option;
  cmd_stdout: string option;
  cmd_verbose: bool option;
  cmd_name: string option;
  cmd_metadata: (string * string) list option;
}

let string_of_command c = String.concat " " (c.cmd::c.args)
let text_of_command c = c.cmd_text
let default_verbose () = OpamCoreConfig.(!r.verbose_level) >= 2
let is_verbose_command c =
  OpamStd.Option.default (default_verbose ()) c.cmd_verbose

let make_command_text ?(color=`green) str ?(args=[]) cmd =
  let summary =
    match
      List.filter (fun s ->
          String.length s > 0 && s.[0] <> '-' &&
          not (String.contains s '/') && not (String.contains s '='))
        args
    with
    | hd::_ -> String.concat " " [cmd; hd]
    | [] -> cmd
  in
  Printf.sprintf "[%s: %s]" (OpamConsole.colorise color str) summary

let command ?env ?verbose ?name ?metadata ?dir ?allow_stdin ?stdout ?text
    cmd args =
  { cmd; args;
    cmd_env=env; cmd_verbose=verbose; cmd_name=name; cmd_metadata=metadata;
    cmd_dir=dir; cmd_stdin=allow_stdin; cmd_stdout=stdout; cmd_text=text; }


(** Running processes *)

type t = {
  p_name   : string;
  p_args   : string list;
  p_pid    : int;
  p_cwd    : string;
  p_time   : float;
  p_stdout : string option;
  p_stderr : string option;
  p_env    : string option;
  p_info   : string option;
  p_metadata: (string * string) list;
  p_verbose: bool;
  p_tmp_files: string list;
}

let output_lines oc lines =
  List.iter (fun line ->
    output_string oc line;
    output_string oc "\n";
    flush oc;
  ) lines;
  output_string oc "\n";
  flush oc

let make_info ?code ?signal
    ~cmd ~args ~cwd ~env_file ~stdout_file ~stderr_file ~metadata () =
  let b = ref [] in
  let home = OpamStd.Sys.home () in
  let print name str =
    let str =
      if OpamStd.String.starts_with ~prefix:home str
      then "~"^OpamStd.String.remove_prefix ~prefix:home str
      else str
    in
    b := (name, str) :: !b
  in
  let print_opt name = function
    | None   -> ()
    | Some s -> print name s in

  List.iter (fun (k,v) -> print k v) metadata;
  print     "path"         cwd;
  print     "command"      (String.concat " " (cmd :: args));
  print_opt "exit-code"    (OpamStd.Option.map string_of_int code);
  print_opt "signalled"    (OpamStd.Option.map string_of_int signal);
  print_opt "env-file"     env_file;
  if stderr_file = stdout_file then
    print_opt "output-file"  stdout_file
  else (
    print_opt "stdout-file"  stdout_file;
    print_opt "stderr-file"  stderr_file;
  );

  List.rev !b

let string_of_info ?(color=`yellow) info =
  let b = Buffer.create 1024 in
  List.iter
    (fun (k,v) -> Printf.bprintf b "%s %-20s %s\n"
        (OpamConsole.colorise color "#")
        (OpamConsole.colorise color k) v) info;
  Buffer.contents b

let resolve_command_fn = ref (fun ?env:_ ?dir:_ _ -> None)
let set_resolve_command =
  let called = ref false in
  fun resolve_command ->
    if !called then invalid_arg "Just what do you think you're doing, Dave?";
    called := true;
    resolve_command_fn := resolve_command
let resolve_command cmd = !resolve_command_fn cmd

let create_process_env =
  if Sys.win32 then
    fun cmd ->
      if OpamStd.Option.map_default
          (OpamStd.Sys.is_cygwin_variant
             ~cygbin:(OpamCoreConfig.(!r.cygbin)))
             false
             (resolve_command cmd) then
        cygwin_create_process_env cmd
      else Unix.create_process_env cmd
  else
    Unix.create_process_env

(** [create cmd args] create a new process to execute the command
    [cmd] with arguments [args]. If [stdout_file] or [stderr_file] are
    set, the channels are redirected to the corresponding files.  The
    outputs are discarded is [verbose] is set to false. The current
    environment can also be overridden if [env] is set. The environment
    which is used to run the process is recorded into [env_file] (if
    set). *)
let create ?info_file ?env_file ?(allow_stdin=not Sys.win32) ?stdout_file ?stderr_file ?env ?(metadata=[]) ?dir
    ~verbose ~tmp_files cmd args =
  let nothing () = () in
  let tee f =
    let flags = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND; Unix.O_SHARE_DELETE] in
    let fd = Unix.openfile f flags 0o644 in
    let close_fd () = Unix.close fd in
    fd, close_fd in
  let oldcwd = Sys.getcwd () in
  let cwd = OpamStd.Option.default oldcwd dir in
  let with_chdir dir =
    match dir with
    | None -> (fun f -> f ())
    | Some dir ->
      Unix.chdir dir;
      OpamStd.Exn.finally (fun () -> Unix.chdir oldcwd)
  in
  with_chdir dir @@ fun () ->
  let stdin_fd,close_stdin =
    if allow_stdin then Unix.stdin, nothing else
    let fd,outfd = Unix.pipe () in
    let close_stdin () = Unix.close fd in
    Unix.close outfd; fd, close_stdin
  in
  let stdout_fd, close_stdout = match stdout_file with
    | None   -> Unix.stdout, nothing
    | Some f -> tee f in
  let stderr_fd, close_stderr = match stderr_file with
    | None   -> Unix.stderr, nothing
    | Some f ->
      if stdout_file = Some f then stdout_fd, nothing
      else tee f
  in
  let env = match env with
    | None   -> default_env ()
    | Some e -> e in
  let time = Unix.gettimeofday () in

  let () =
    (* write the env file before running the command*)
    match env_file with
    | None   -> ()
    | Some f ->
      let chan = open_out f in
      let env = Array.to_list env in
      (* Remove dubious variables *)
      let env =
        List.filter (fun line -> not (OpamStd.String.contains_char line '$'))
          env
      in
      output_lines chan env;
      close_out chan in

  let () =
    (* write the info file *)
    match info_file with
    | None   -> ()
    | Some f ->
      let chan = open_out f in
      let info =
        make_info ~cmd ~args ~cwd ~env_file ~stdout_file ~stderr_file ~metadata () in
      output_string chan (string_of_info info);
      close_out chan in

  let pid =
    let cmd, args =
      if Sys.win32 then
        try
          let actual_command =
            if Sys.file_exists cmd then
              cmd
            else if Sys.file_exists (cmd ^ ".exe") then
              cmd ^ ".exe"
            else
              raise Exit in
          let actual_image, args =
            let c = open_in actual_command in
            set_binary_mode_in c true;
            try
              if really_input_string c 2 = "#!" then begin
                (* The input_line will only fail for a 2-byte file consisting of exactly #! (with no \n), which is acceptable! *)
                let l = String.trim (input_line c) in
                let cmd, arg =
                  try
                    let i = String.index l ' ' in
                    let cmd = Filename.basename (String.trim (String.sub l 0 i)) in
                    let arg = String.trim (String.sub l i (String.length l - i)) in
                    if cmd = "env" then
                      arg, None
                    else
                      cmd, Some arg
                  with Not_found ->
                    Filename.basename l, None in
                close_in c;
                try
                  let cmd = OpamStd.Option.default cmd (resolve_command cmd) in
                  (*Printf.eprintf "Deduced %s => %s to be executed via %s\n%!" cmd actual_command cmd;*)
                  let args = actual_command::args in
                  cmd, OpamStd.Option.map_default (fun arg -> arg::args) args arg
                with Not_found ->
                  (* Script interpreter isn't available - fall back *)
                  raise Exit
              end else begin
                close_in c;
                actual_command, args
              end
            with End_of_file ->
              close_in c;
              (* A two-byte image can't be executable! *)
              raise Exit in
          (*Printf.eprintf "Final deduction: %s -> %s\n%!" cmd actual_image;*)
          actual_image, args
        with Exit ->
          (* Fall back to default behaviour if anything went wrong - almost certainly means a broken package *)
          cmd, args
      else
        cmd, args in
    try
      create_process_env
        cmd
        (Array.of_list (cmd :: args))
        env
        stdin_fd stdout_fd stderr_fd
    with e ->
      close_stdin  ();
      close_stdout ();
      close_stderr ();
      raise e in
  close_stdin  ();
  close_stdout ();
  close_stderr ();
  {
    p_name   = cmd;
    p_args   = args;
    p_pid    = pid;
    p_cwd    = cwd;
    p_time   = time;
    p_stdout = stdout_file;
    p_stderr = stderr_file;
    p_env    = env_file;
    p_info   = info_file;
    p_metadata = metadata;
    p_verbose = verbose;
    p_tmp_files = tmp_files;
  }

type result = {
  r_code     : int;
  r_signal   : int option;
  r_duration : float;
  r_info     : (string * string) list;
  r_stdout   : string list;
  r_stderr   : string list;
  r_cleanup  : string list;
}

let empty_result = {
  r_code = 0;
  r_signal = None;
  r_duration = 0.;
  r_info = [];
  r_stdout = [];
  r_stderr = [];
  r_cleanup = [];
}

(* XXX: the function might block for ever for some channels kinds *)
let read_lines f =
  try
    let ic = open_in f in
    let lines = ref [] in
    begin
      try
        while true do
          let line = input_line ic in
          lines := line :: !lines;
        done
      with End_of_file | Sys_error _ -> ()
    end;
    close_in ic;
    List.rev !lines
  with Sys_error _ -> []

(* Compat function (Windows) *)
let interrupt p =
  if Sys.win32 then Unix.kill p.p_pid Sys.sigkill
  else Unix.kill p.p_pid Sys.sigint

let run_background command =
  let { cmd; args;
        cmd_env=env; cmd_verbose=_; cmd_name=name; cmd_text=_;
        cmd_metadata=metadata; cmd_dir=dir;
        cmd_stdin=allow_stdin; cmd_stdout } =
    command
  in
  let verbose = is_verbose_command command in
  let allow_stdin = OpamStd.Option.default false allow_stdin in
  let env = match env with Some e -> e | None -> default_env () in
  let file ext = match name with
    | None -> None
    | Some n ->
      let d =
        if Filename.is_relative n then
          match dir with
            | Some d -> d
            | None -> OpamCoreConfig.(!r.log_dir)
        else ""
      in
      Some (Filename.concat d (Printf.sprintf "%s.%s" n ext))
  in
  let stdout_file =
    OpamStd.Option.Op.(cmd_stdout >>+ fun () -> file "out")
  in
  let stderr_file =
    if OpamCoreConfig.(!r.merged_output) then file "out" else file "err"
  in
  let env_file    = file "env" in
  let info_file   = file "info" in
  let tmp_files =
    OpamStd.List.filter_some [
      info_file;
      env_file;
      stderr_file;
      if cmd_stdout <> None || stderr_file = stdout_file then None
      else stdout_file;
    ]
  in
  create ~env ?info_file ?env_file ?stdout_file ?stderr_file ~verbose ?metadata
    ~allow_stdin ?dir ~tmp_files cmd args

let dry_run_background c = {
  p_name   = c.cmd;
  p_args   = c.args;
  p_pid    = -1;
  p_cwd    = OpamStd.Option.default (Sys.getcwd ()) c.cmd_dir;
  p_time   = Unix.gettimeofday ();
  p_stdout = None;
  p_stderr = None;
  p_env    = None;
  p_info   = None;
  p_metadata = OpamStd.Option.default [] c.cmd_metadata;
  p_verbose = is_verbose_command c;
  p_tmp_files = [];
}

let verbose_print_cmd p =
  OpamConsole.msg "%s %s %s%s\n"
    (OpamConsole.colorise `yellow "+")
    p.p_name
    (OpamStd.List.concat_map " " (Printf.sprintf "%S") p.p_args)
    (if p.p_cwd = Sys.getcwd () then ""
     else Printf.sprintf " (CWD=%s)" p.p_cwd)

let verbose_print_out =
  let pfx = lazy (OpamConsole.colorise `yellow "- ") in
  fun s ->
    OpamConsole.msg "%s%s\n" (Lazy.force pfx) s

(** Semi-synchronous printing of the output of a command *)
let set_verbose_f, print_verbose_f, isset_verbose_f, stop_verbose_f =
  let verbose_f = ref None in
  let stop () = match !verbose_f with
    | None -> ()
    | Some (ics,_) ->
      List.iter close_in_noerr ics;
      verbose_f := None
  in
  let set files =
    stop ();
    (* implem relies on sigalrm, not implemented on win32.
       This will fall back to buffered output. *)
    if Sys.win32 then () else
    let files = OpamStd.List.sort_nodup compare files in
    let ics =
      List.map
        (open_in_gen [Open_nonblock;Open_rdonly;Open_text;Open_creat] 0o600)
        files
    in
    let f () =
      List.iter (fun ic ->
          try while true do verbose_print_out (input_line ic) done
          with End_of_file -> flush stdout
        ) ics
    in
    verbose_f := Some (ics, f)
  in
  let print () = match !verbose_f with
    | Some (_, f) -> f ()
    | None -> ()
  in
  let isset () = !verbose_f <> None in
  let flush_and_stop () = print (); stop () in
  set, print, isset, flush_and_stop

let set_verbose_process p =
  if p.p_verbose then
    let fs = OpamStd.List.filter_some [p.p_stdout;p.p_stderr] in
    if fs <> [] then (
      verbose_print_cmd p;
      set_verbose_f fs
    )

let exit_status p return =
  let duration = Unix.gettimeofday () -. p.p_time in
  let stdout = OpamStd.Option.default [] (OpamStd.Option.map read_lines p.p_stdout) in
  let stderr = OpamStd.Option.default [] (OpamStd.Option.map read_lines p.p_stderr) in
  let cleanup = p.p_tmp_files in
  let code,signal = match return with
    | Unix.WEXITED r -> Some r, None
    | Unix.WSIGNALED s | Unix.WSTOPPED s -> None, Some s
  in
  if isset_verbose_f () then
    stop_verbose_f ()
  else if p.p_verbose then
    (List.iter verbose_print_out stdout;
     if p.p_stdout <> p.p_stderr then
     List.iter verbose_print_out stderr;
     flush Stdlib.stdout);
  let info =
    make_info ?code ?signal
      ~cmd:p.p_name ~args:p.p_args ~cwd:p.p_cwd ~metadata:p.p_metadata
      ~env_file:p.p_env ~stdout_file:p.p_stdout ~stderr_file:p.p_stderr () in
  {
    r_code     = OpamStd.Option.default 256 code;
    r_signal   = signal;
    r_duration = duration;
    r_info     = info;
    r_stdout   = stdout;
    r_stderr   = stderr;
    r_cleanup  = cleanup;
  }

let safe_wait fallback_pid f x =
  let sh =
    if isset_verbose_f () then
      let hndl _ = print_verbose_f () in
      Some (Sys.signal Sys.sigalrm (Sys.Signal_handle hndl))
    else None
  in
  let cleanup () =
    match sh with
    | Some sh ->
      ignore (Unix.alarm 0); (* cancels the alarm *)
      Sys.set_signal Sys.sigalrm sh
    | None -> ()
  in
  let rec aux () =
    if sh <> None then ignore (Unix.alarm 1);
    match
      try f x with
      | Unix.Unix_error (Unix.EINTR,_,_) -> aux () (* handled signal *)
      | Unix.Unix_error (Unix.ECHILD,_,_) ->
        log "Warn: no child to wait for %d" fallback_pid;
        fallback_pid, Unix.WEXITED 256
      with
      | _, Unix.WSTOPPED _ ->
        (* shouldn't happen as we don't use WUNTRACED *)
        aux ()
      | r -> r
  in
  try let r = aux () in cleanup (); r
  with e -> cleanup (); raise e

let wait p =
  set_verbose_process p;
  let _, return = safe_wait p.p_pid (Unix.waitpid []) p.p_pid in
  exit_status p return

let dontwait p =
  match safe_wait p.p_pid (Unix.waitpid [Unix.WNOHANG]) p.p_pid with
  | 0, _ -> None
  | _, return -> Some (exit_status p return)

let dead_childs = Hashtbl.create 13
let wait_one processes =
  if processes = [] then raise (Invalid_argument "wait_one");
  let p, return =
    try
      let p =
        List.find (fun p -> Hashtbl.mem dead_childs p.p_pid) processes
      in
      let return = Hashtbl.find dead_childs p.p_pid in
      Hashtbl.remove dead_childs p.p_pid;
      p, return
    with Not_found ->
      let rec aux () =
        let pid, return =
          if Sys.win32 then
            (* No Unix.wait on Windows, so use a stub wrapping
               WaitForMultipleObjects *)
            let pids, len =
              let f (l, n) t = (t.p_pid::l, succ n) in
              List.fold_left f ([], 0) processes
            in
            OpamStubs.waitpids pids len
          else
            safe_wait (List.hd processes).p_pid Unix.wait () in
        try
          let p = List.find (fun p -> p.p_pid = pid) processes in
          p, return
        with Not_found ->
          Hashtbl.add dead_childs pid return;
          aux ()
      in
      aux ()
  in
  if p.p_verbose then verbose_print_cmd p;
  p, exit_status p return

let dry_wait_one = function
  | {p_pid = -1; _} as p :: _ ->
    if p.p_verbose then (verbose_print_cmd p; flush stdout);
    p, empty_result
  | _ -> raise (Invalid_argument "dry_wait_one")

let run command =
  let command =
    { command with
      cmd_stdin = OpamStd.Option.Op.(command.cmd_stdin ++ Some (not Sys.win32)) }
  in
  let p = run_background command in
  try wait p with e ->
    match (try dontwait p with _ -> raise e) with
    | None -> (* still running *)
      (try interrupt p with Unix.Unix_error _ -> ());
      raise e
    | _ -> raise e

let is_failure r = r.r_code <> 0 || r.r_signal <> None

let is_success r = not (is_failure r)

let safe_unlink f =
  try
    log ~level:2 "safe_unlink: %s" f;
    Unix.unlink f
  with Unix.Unix_error _ ->
    log ~level:2 "safe_unlink: %s (FAILED)" f

let cleanup ?(force=false) r =
  if force || (not (OpamConsole.debug ()) && is_success r) then
    List.iter safe_unlink r.r_cleanup

let check_success_and_cleanup r =
  List.iter safe_unlink r.r_cleanup;
  is_success r

let log_line_limit = 5 * 80
let truncate_str = "[...]"

(* Truncate long lines *)
let truncate_line str =
  if String.length str <= log_line_limit then
    str
  else
    String.sub str 0 (log_line_limit - String.length truncate_str)
    ^ truncate_str

(* Take the last [n] elements of [l] (trying to keep an unindented header line
   for context, like diff) *)
let truncate l =
  let unindented s =
    String.length s > 0 && s.[0] <> ' ' && s.[0] <> '\t'
  in
  let rec cut n acc = function
    | [] -> acc
    | [x] when n = 0 -> truncate_line x :: acc
    | _ when n = 0 -> truncate_str :: acc
    | x::l when n = 1 ->
      (if unindented x then truncate_str :: truncate_line x :: acc else
       try truncate_line (List.find unindented l) :: truncate_str :: acc
       with Not_found -> truncate_str :: truncate_line x :: acc)
    | x::r -> cut (n-1) (truncate_line x :: acc) r
  in
  let len = OpamCoreConfig.(!r.errlog_length) in
  if len <= 0 then l
  else cut len [] (List.rev l)

let string_of_result ?(color=`yellow) r =
  let b = Buffer.create 2048 in
  let print = Buffer.add_string b in
  let println str =
    print str;
    Buffer.add_char b '\n' in

  print (string_of_info ~color r.r_info);

  if r.r_stdout <> [] then
    if r.r_stderr = r.r_stdout then
      print (OpamConsole.colorise color "### output ###\n")
    else
      print (OpamConsole.colorise color "### stdout ###\n");
  List.iter (fun s ->
      print (OpamConsole.colorise color "# ");
      println s)
    (truncate r.r_stdout);

  if r.r_stderr <> [] && r.r_stderr <> r.r_stdout then (
    print (OpamConsole.colorise color "### stderr ###\n");
    List.iter (fun s ->
        print (OpamConsole.colorise color "# ");
        println s)
      (truncate r.r_stderr)
  );

  Buffer.contents b

let result_summary r =
  Printf.sprintf "%S exited with code %d%s"
    (try OpamStd.List.assoc String.equal "command" r.r_info
     with Not_found -> "command")
    r.r_code
    (if r.r_code = 0 then "" else
     match r.r_stderr, r.r_stdout with
     | [e], _ | [], [e] -> Printf.sprintf " \"%s\"" e
     | [], es | es, _ ->
       try
         Printf.sprintf " \"%s\""
           (List.find
              Re.(execp (compile (seq [ bos; rep (diff any alpha);
                                        no_case (str "error") ])))
              (List.rev es))
       with Not_found -> ""
     | _ -> "")

(* Higher-level interface to allow parallelism *)

module Job = struct
  module Op = struct
    type 'a job = (* Open the variant type *)
      | Done of 'a
      | Run of command * (result -> 'a job)

    (* Parallelise shell commands *)
    let (@@>) command f = Run (command, f)

    (* Sequentialise jobs *)
    let rec (@@+) job1 fjob2 = match job1 with
      | Done x -> fjob2 x
      | Run (cmd,cont) -> Run (cmd, fun r -> cont r @@+ fjob2)

    let (@@|) job f = job @@+ fun x -> Done (f x)
  end

  open Op

  let run =
    let rec aux = function
      | Done x -> x
      | Run (cmd,cont) ->
        OpamStd.Option.iter
          (if OpamConsole.disp_status_line () then
             OpamConsole.status_line "Processing: %s"
           else if OpamConsole.verbose () then
             OpamConsole.msg "%s\n"
           else fun _ -> ())
          (text_of_command cmd);
        let r = run cmd in
        let k =
          try cont r
          with e ->
            cleanup r;
            OpamConsole.clear_status ();
            raise e
        in
        cleanup r;
        OpamConsole.clear_status ();
        aux k
    in
    aux

  let rec dry_run = function
    | Done x -> x
    | Run (_command,cont) ->
      dry_run (cont empty_result)

  let rec catch handler fjob =
    try match fjob () with
      | Done x -> Done x
      | Run (cmd,cont) ->
        Run (cmd, fun r -> catch handler (fun () -> cont r))
    with e -> handler e

  let ignore_errors ~default ?message job =
    catch (fun e ->
        OpamStd.Exn.fatal e;
        OpamStd.Option.iter (OpamConsole.error "%s") message;
        Done default)
      job

  let rec finally fin fjob =
    try match fjob () with
      | Done x -> fin (); Done x
      | Run (cmd,cont) ->
        Run (cmd, fun r -> finally fin (fun () -> cont r))
    with e -> fin (); raise e

  let of_list ?(keep_going=false) l =
    let rec aux err = function
      | [] -> Done err
      | cmd::commands ->
        let cont = fun r ->
          if is_success r then aux err commands
          else if keep_going then
            aux OpamStd.Option.Op.(err ++ Some (cmd,r)) commands
          else Done (Some (cmd,r))
        in
        Run (cmd,cont)
    in
    aux None l

  let of_fun_list ?(keep_going=false) l =
    let rec aux err = function
      | [] -> Done err
      | cmdf::commands ->
        let cmd = cmdf () in
        let cont = fun r ->
          if is_success r then aux err commands
          else if keep_going then
            aux OpamStd.Option.Op.(err ++ Some (cmd,r)) commands
          else Done (Some (cmd,r))
        in
        Run (cmd,cont)
    in
    aux None l

  let seq job start = List.fold_left (@@+) (Done start) job

  let seq_map f l =
    List.fold_left (fun job x ->
        job @@+ fun acc -> f x @@| fun y -> y :: acc)
      (Done []) l
    @@| List.rev

  let rec with_text text = function
    | Done _ as j -> j
    | Run (cmd, cont) ->
      Run ({cmd with cmd_text = Some text}, fun r -> with_text text (cont r))
end

type 'a job = 'a Job.Op.job
