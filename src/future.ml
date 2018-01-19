open Import

type exn_handler = exn -> Printexc.raw_backtrace -> unit

type 'a t = { mutable state : 'a state }

and 'a state =
  | Return of 'a
  | Sleep of 'a handlers
  | Repr of 'a t

and 'a handlers =
  | Empty
  | One    of exn_handler * ('a -> unit)
  | Append of 'a handlers * 'a handlers

let exn_handler = ref (fun exn _ -> reraise exn)

let append h1 h2 =
  match h1, h2 with
  | Empty, _ -> h2
  | _, Empty -> h1
  | _ -> Append (h1, h2)

let rec repr t =
  match t.state with
  | Repr t' -> let t'' = repr t' in if t'' != t' then t.state <- Repr t''; t''
  | _       -> t

let run_handlers handlers x =
  let rec loop handlers acc =
    match handlers with
    | Empty -> continue acc
    | One (handler, f) ->
      exn_handler := handler;
      (try
         f x
       with exn ->
         let bt = Printexc.get_raw_backtrace () in
         handler exn bt);
      continue acc
    | Append (h1, h2) -> loop h1 (h2 :: acc)
  and continue = function
    | [] -> ()
    | h :: acc -> loop h acc
  in
  protectx !exn_handler
    ~finally:(fun saved ->
        exn_handler := saved)
    ~f:(fun _ ->
        loop handlers [])

let connect t1 t2 =
  let t1 = repr t1 and t2 = repr t2 in
  match t1.state with
  | Sleep h1 ->
    if t1 == t2 then
      ()
    else begin
      match t2.state with
      | Repr _ -> assert false
      | Sleep h2 ->
        t2.state <- Repr t1;
        t1.state <- Sleep (append h1 h2)
      | Return x as state2 ->
        t1.state <- state2;
        run_handlers h1 x
    end
  | _ ->
    assert false

let return x = { state = Return x }

let sleeping () = { state = Sleep Empty }

let ( >>= ) t f =
  let t = repr t in
  match t.state with
  | Return v -> f v
  | Sleep handlers ->
    let res = sleeping () in
    t.state <- Sleep (append handlers (One (!exn_handler,
                                            fun x -> connect res (f x))));
    res
  | Repr _ ->
    assert false

let ( >>| ) t f = t >>= fun x -> return (f x)

let with_exn_handler f ~handler =
  let saved = !exn_handler in
  exn_handler := handler;
  match f () with
  | x -> exn_handler := saved; x
  | exception exn ->
    let bt = Printexc.get_raw_backtrace () in
    exn_handler := saved;
    handler exn bt;
    reraise exn

let finalize f ~finally =
  let finalize = lazy(finally ()) in
  with_exn_handler
    (fun () ->
       f () >>| fun x ->
       Lazy.force finalize;
       x)
    ~handler:(fun exn _ ->
      Lazy.force finalize;
      reraise exn)

let both a b =
  a >>= fun a ->
  b >>= fun b ->
  return (a, b)

let create f =
  let t = sleeping () in
  f t;
  t

module Ivar = struct
  type nonrec 'a t = 'a t

  let fill t x =
    let t = repr t in
    match t.state with
    | Repr _ -> assert false
    | Return _ -> failwith "Future.Ivar.fill"
    | Sleep handlers ->
      t.state <- Return x;
      run_handlers handlers x
end

let rec all = function
  | [] -> return []
  | x :: l ->
    x     >>= fun x ->
    all l >>= fun l ->
    return (x :: l)

let rec all_unit = function
  | [] -> return ()
  | x :: l ->
    x >>= fun () ->
    all_unit l

type to_fill = To_fill : 'a Ivar.t * 'a -> to_fill

let to_fill = Queue.create ()

module Mutex = struct
  type t =
    { mutable locked  : bool
    ; mutable waiters : unit Ivar.t Queue.t
    }

  let lock t =
    if t.locked then
      create (fun ivar -> Queue.push ivar t.waiters)
    else begin
      t.locked <- true;
      return ()
    end

  let unlock t =
    assert t.locked;
    if Queue.is_empty t.waiters then
      t.locked <- false
    else
      Queue.push (To_fill (Queue.pop t.waiters, ())) to_fill

  let with_lock t f =
    lock t >>= fun () ->
    finalize f ~finally:(fun () -> unlock t)

  let create () =
    { locked  = false
    ; waiters = Queue.create ()
    }
end

type accepted_codes =
  | These of int list
  | All

let code_is_ok accepted_codes n =
  match accepted_codes with
  | These set -> List.mem n ~set
  | All -> true

type ('a, 'b) failure_mode =
  | Strict : ('a, 'a) failure_mode
  | Accept : accepted_codes -> ('a, ('a, int) result) failure_mode

let accepted_codes : type a b. (a, b) failure_mode -> accepted_codes = function
  | Strict -> These [0]
  | Accept (These codes) -> These (0 :: codes)
  | Accept All -> All

let map_result
  : type a b. (a, b) failure_mode -> int t -> f:(unit -> a) -> b t
  = fun mode future ~f ->
    match mode with
    | Strict   -> future >>| fun _ -> f ()
    | Accept _ ->
      future >>| function
      | 0 -> Ok (f ())
      | n -> Error n

type std_output_to =
  | Terminal
  | File        of string
  | Opened_file of opened_file

and opened_file =
  { filename : string
  ; desc     : opened_file_desc
  ; tail     : bool
  }

and opened_file_desc =
  | Fd      of Unix.file_descr
  | Channel of out_channel

(** Why a Future.t was run *)
type purpose =
  | Internal_job
  | Build_job of Path.t list

type job =
  { prog      : string
  ; args      : string list
  ; dir       : string option
  ; stdout_to : std_output_to
  ; stderr_to : std_output_to
  ; env       : string array option
  ; ivar      : int Ivar.t
  ; ok_codes  : accepted_codes
  ; purpose   : purpose
  }

let to_run : job Queue.t = Queue.create ()

let run_internal ?dir ?(stdout_to=Terminal) ?(stderr_to=Terminal) ?env ~purpose fail_mode prog args =
  let dir =
    match dir with
    | Some "." -> None
    | _ -> dir
  in
  create (fun ivar ->
    Queue.push { prog
               ; args
               ; dir
               ; stdout_to
               ; stderr_to
               ; env
               ; ivar
               ; ok_codes = accepted_codes fail_mode
               ; purpose
               } to_run)

let run ?dir ?stdout_to ?stderr_to ?env ?(purpose=Internal_job) fail_mode prog args =
  map_result fail_mode (run_internal ?dir ?stdout_to ?stderr_to ?env ~purpose fail_mode prog args)
    ~f:ignore

module Temp = struct
  let tmp_files = ref String_set.empty
  let () =
    at_exit (fun () ->
      let fns = !tmp_files in
      tmp_files := String_set.empty;
      String_set.iter fns ~f:(fun fn ->
        try Sys.force_remove fn with _ -> ()))

  let create prefix suffix =
    let fn = Filename.temp_file prefix suffix in
    tmp_files := String_set.add fn !tmp_files;
    fn

  let destroy fn =
    (try Sys.force_remove fn with Sys_error _ -> ());
    tmp_files := String_set.remove fn !tmp_files
end

let run_capture_gen ?dir ?env ?(purpose=Internal_job) fail_mode prog args ~f =
  let fn = Temp.create "jbuild" ".output" in
  map_result fail_mode (run_internal ?dir ~stdout_to:(File fn) ?env ~purpose fail_mode prog args)
    ~f:(fun () ->
      let x = f fn in
      Temp.destroy fn;
      x)

let run_capture       = run_capture_gen ~f:Io.read_file
let run_capture_lines = run_capture_gen ~f:Io.lines_of_file

let run_capture_line ?dir ?env ?(purpose=Internal_job) fail_mode prog args =
  run_capture_gen ?dir ?env ~purpose fail_mode prog args ~f:(fun fn ->
    match Io.lines_of_file fn with
    | [x] -> x
    | l ->
      let cmdline =
        let s = String.concat (prog :: args) ~sep:" " in
        match dir with
        | None -> s
        | Some dir -> sprintf "cd %s && %s" dir s
      in
      match l with
      | [] ->
        die "command returned nothing: %s" cmdline
      | _ ->
        die "command returned too many lines: %s\n%s"
          cmdline (String.concat l ~sep:"\n"))

module Scheduler = struct
  let split_prog s =
    let len = String.length s in
    if len = 0 then
      "", "", ""
    else begin
      let rec find_prog_start i =
        if i < 0 then
          0
        else
          match s.[i] with
          | '\\' | '/' -> (i + 1)
          | _ -> find_prog_start (i - 1)
      in
      let prog_end =
        match s.[len - 1] with
        | '"' -> len - 1
        | _   -> len
      in
      let prog_start = find_prog_start (prog_end - 1) in
      let prog_end =
        match String.index_from s prog_start '.' with
        | exception _ -> prog_end
        | i -> i
      in
      let before = String.sub s ~pos:0 ~len:prog_start in
      let after = String.sub s ~pos:prog_end ~len:(len - prog_end) in
      let prog = String.sub s ~pos:prog_start ~len:(prog_end - prog_start) in
      before, prog, after
    end

  let colorize_prog s =
    let len = String.length s in
    if len = 0 then
      s
    else
      let before, prog, after = split_prog s in
      before ^ Ansi_color.colorize ~key:prog prog ^ after

  let rec colorize_args = function
    | [] -> []
    | "-o" :: fn :: rest ->
      "-o" :: Ansi_color.(apply_string output_filename) fn :: colorize_args rest
    | x :: rest -> x :: colorize_args rest

  let command_line { prog; args; dir; stdout_to; stderr_to; _ } =
    let quote = quote_for_shell in
    let prog = colorize_prog (quote prog) in
    let s = String.concat (prog :: colorize_args (List.map args ~f:quote)) ~sep:" " in
    let s =
      match dir with
      | None -> s
      | Some dir -> sprintf "(cd %s && %s)" dir s
    in
    match stdout_to, stderr_to with
    | (File fn1 | Opened_file { filename = fn1; _ }),
      (File fn2 | Opened_file { filename = fn2; _ }) when fn1 = fn2 ->
      sprintf "%s &> %s" s fn1
    | _ ->
      let s =
        match stdout_to with
        | Terminal -> s
        | File fn | Opened_file { filename = fn; _ } -> sprintf "%s > %s" s fn
      in
      match stderr_to with
      | Terminal -> s
      | File fn | Opened_file { filename = fn; _ } -> sprintf "%s 2> %s" s fn

  let pp_purpose ppf = function
    | Internal_job ->
      Format.fprintf ppf "(internal)"
    | Build_job targets ->
      let rec split_paths targets_acc ctxs_acc = function
        | [] -> List.rev targets_acc, String_set.(elements (of_list ctxs_acc))
        | path :: rest ->
          let add_ctx ctx acc = if ctx = "default" then acc else ctx :: acc in
          match Utils.analyse_target path with
          | Other path ->
            split_paths (Path.to_string path :: targets_acc) ctxs_acc rest
          | Regular (ctx, filename) ->
            split_paths (Path.to_string filename :: targets_acc) (add_ctx ctx ctxs_acc) rest
          | Alias (ctx, name) ->
            split_paths (("alias " ^ Path.to_string name) :: targets_acc) (add_ctx ctx ctxs_acc) rest
      in
      let target_names, contexts = split_paths [] [] targets in
      let target_names_grouped_by_prefix =
        List.map target_names ~f:Filename.split_extension_after_dot
        |> String_map.of_alist_multi
        |> String_map.bindings
      in
      let pp_comma ppf () = Format.fprintf ppf "," in
      let pp_group ppf (prefix, suffixes) =
        match suffixes with
        | [] -> assert false
        | [suffix] -> Format.fprintf ppf "%s%s" prefix suffix
        | _ ->
          Format.fprintf ppf "%s{%a}"
            prefix
            (Format.pp_print_list ~pp_sep:pp_comma Format.pp_print_string)
            suffixes
      in
      let pp_contexts ppf = function
        | [] -> ()
        | ctxs ->
          Format.fprintf ppf " @{<details>[%a]@}"
            (Format.pp_print_list ~pp_sep:pp_comma
               (fun ppf s -> Format.fprintf ppf "%s" s))
            ctxs
      in
      Format.fprintf ppf "%a%a"
        (Format.pp_print_list ~pp_sep:pp_comma pp_group)
        target_names_grouped_by_prefix
        pp_contexts
        contexts;

  type running_job =
    { id              : int
    ; job             : job
    ; pid             : int
    ; output_filename : string option
    ; (* for logs, with ansi colors code always included in the string *)
      command_line    : string
    ; log             : Log.t
    }

  module Running_jobs : sig
    val add : running_job -> unit
    val wait : unit -> running_job * Unix.process_status
    val wait_nonblocking : unit -> (running_job * Unix.process_status) option
    val count : unit -> int
    val all : unit -> running_job list
  end = struct
    let all = Hashtbl.create 128

    let add job = Hashtbl.add all ~key:job.pid ~data:job

    let count () = Hashtbl.length all

    let resolve_and_remove_job pid =
      let job =
        Hashtbl.find_exn all pid ~string_of_key:(sprintf "<pid:%d>")
          ~table_desc:(fun _ -> "<running-jobs>")
      in
      Hashtbl.remove all pid;
      job

    exception Finished of running_job * Unix.process_status

    let wait_nonblocking_win32 () =
      match
        Hashtbl.iter all ~f:(fun ~key:pid ~data:job ->
          let pid, status = Unix.waitpid [WNOHANG] pid in
          if pid <> 0 then
            raise_notrace (Finished (job, status)))
      with
      | () -> None
      | exception (Finished (job, status)) ->
        Hashtbl.remove all job.pid;
        Some (job, status)

    let wait_nonblocking_unix () =
      let pid, status = Unix.waitpid [WNOHANG] (-1) in
      if pid = 0 then
        None
      else
        Some (resolve_and_remove_job pid, status)

    let wait_nonblocking =
      if Sys.win32 then
        wait_nonblocking_win32
      else
        wait_nonblocking_unix

    let rec wait_win32 () =
      match wait_nonblocking_win32 () with
      | None ->
        ignore (Unix.select [] [] [] 0.001);
        wait_win32 ()
      | Some x -> x

    let wait_unix () =
      let pid, status = Unix.wait () in
      (resolve_and_remove_job pid, status)

    let wait =
      if Sys.win32 then
        wait_win32
      else
        wait_unix

    let all () = Hashtbl.fold all ~init:[] ~f:(fun ~key:_ ~data:job acc -> job :: acc)
  end

  let process_done ?(exiting=false) job (status : Unix.process_status) =
    let output =
      match job.output_filename with
      | None -> ""
      | Some fn ->
        let s = Io.read_file fn in
        Temp.destroy fn;
        let len = String.length s in
        if len > 0 && s.[len - 1] <> '\n' then
          s ^ "\n"
        else
          s
    in
    Log.command job.log
      ~command_line:job.command_line
      ~output:output
      ~exit_status:status;
    let _, progname, _ = split_prog job.job.prog in
    match status with
    | WEXITED n when code_is_ok job.job.ok_codes n ->
      if !Clflags.verbose then begin
        if output <> "" then
          Format.eprintf "@{<kwd>Output@}[@{<id>%d@}]:\n%s%!" job.id output;
        if n <> 0 then
          Format.eprintf
            "@{<warning>Warning@}: Command [@{<id>%d@}] exited with code %d, \
             but I'm ignore it, hope that's OK.\n%!" job.id n;
      end else if not exiting && (output <> "" || job.job.purpose <> Internal_job) then
        begin
          Format.eprintf "@{<ok>%12s@} %a@." progname pp_purpose job.job.purpose;
          Format.eprintf "%s%!" output;
        end;
      if not exiting then Ivar.fill job.job.ivar n
    | WEXITED n ->
      if !Clflags.verbose then begin
        Format.eprintf "\n@{<kwd>Command@} [@{<id>%d@}] exited with code %d:\n\
                        @{<prompt>$@} %s\n%s%!"
          job.id n
          (Ansi_color.strip_colors_for_stderr job.command_line)
          (Ansi_color.strip_colors_for_stderr output)
      end else begin
        Format.eprintf "@{<error>%12s@} %a @{<error>(exit %d)@}@."
          progname pp_purpose job.job.purpose n;
        Format.eprintf "@{<details>%s@}@."
          (Ansi_color.strip job.command_line);
        Format.eprintf "%s%!" output;
      end;
      if not exiting then die ""
    | WSIGNALED n ->
      if !Clflags.verbose then begin
        Format.eprintf "\n@{<kwd>Command@} [@{<id>%d@}] got signal %s:\n\
                        @{<prompt>$@} %s\n%s%!"
          job.id (Utils.signal_name n)
          (Ansi_color.strip_colors_for_stderr job.command_line)
          (Ansi_color.strip_colors_for_stderr output);
      end else begin
        Format.eprintf "@{<error>%12s@} %a @{<error>(got signal %s)@}@."
          progname pp_purpose job.job.purpose (Utils.signal_name n);
        Format.eprintf "@{<details>%s@}@."
          (Ansi_color.strip job.command_line);
        Format.eprintf "%s%!" output;
      end;
      if not exiting then die ""
    | WSTOPPED _ -> assert false

  let gen_id =
    let next = ref (-1) in
    fun () -> incr next; !next

  let at_exit_handlers = Queue.create ()
  let at_exit_after_waiting_for_commands f = Queue.push f at_exit_handlers
  let exec_at_exit_handlers () =
    while not (Queue.is_empty at_exit_handlers) do
      Queue.pop at_exit_handlers ()
    done

  let wait_for_unfinished_jobs () =
    let rec loop n =
      if Running_jobs.count () > 0 && n > 0 then
        match Running_jobs.wait_nonblocking () with
        | None ->
          ignore (Unix.select [] [] [] 0.05 : _ * _ * _);
          loop (n - 1)
        | Some (job, status) ->
          process_done job status ~exiting:true;
          loop n
    in
    loop 10;
    if Running_jobs.count () > 0 then begin
      if !Clflags.verbose then begin
        let pp_job ppf job =
          let _, name, _ = split_prog job.job.prog in
          Format.fprintf ppf "%s [@{<id>%d@}]" name job.id
        in
        Format.eprintf "\nWaiting for the following jobs to finish: %a@."
          (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ") pp_job)
          (Running_jobs.all ());
      end else begin
        let n = Running_jobs.count () in
        Format.eprintf "\nWaiting for %d %s to finish.@."
          n
          (if n = 1 then "job" else "jobs")
      end;
      while Running_jobs.count () > 0 do
        let job, status = Running_jobs.wait () in
        process_done job status ~exiting:true
      done
    end

  let () =
    at_exit (fun () ->
      wait_for_unfinished_jobs ();
      exec_at_exit_handlers ())

  let get_std_output ~default = function
    | Terminal -> (default, None)
    | File fn ->
      let fd = Unix.openfile fn [O_WRONLY; O_CREAT; O_TRUNC; O_SHARE_DELETE] 0o666 in
      (fd, Some (Fd fd))
    | Opened_file { desc; tail; _ } ->
      let fd =
        match desc with
        | Fd      fd -> fd
        | Channel oc -> flush oc; Unix.descr_of_out_channel oc
      in
      (fd, Option.some_if tail desc)

  let close_std_output = function
    | None -> ()
    | Some (Fd      fd) -> Unix.close fd
    | Some (Channel oc) -> close_out  oc

  let rec go_rec cwd log t =
    match (repr t).state with
    | Return v -> v
    | _ ->
      while Running_jobs.count () < !Clflags.concurrency &&
            not (Queue.is_empty to_run) do
        let job = Queue.pop to_run in
        let id = gen_id () in
        let command_line = command_line job in
        if !Clflags.verbose then
          Format.eprintf "@{<kwd>Running@}[@{<id>%d@}]: %s@." id
            (Ansi_color.strip_colors_for_stderr command_line);
        let argv = Array.of_list (job.prog :: job.args) in
        let output_filename, stdout_fd, stderr_fd, to_close =
          match job.stdout_to, job.stderr_to with
          | (Terminal, _ | _, Terminal) when !Clflags.capture_outputs ->
            let fn = Temp.create "jbuilder" ".output" in
            let fd = Unix.openfile fn [O_WRONLY; O_SHARE_DELETE] 0 in
            (Some fn, fd, fd, Some fd)
          | _ ->
            (None, Unix.stdout, Unix.stderr, None)
        in
        let stdout, close_stdout = get_std_output job.stdout_to ~default:stdout_fd in
        let stderr, close_stderr = get_std_output job.stderr_to ~default:stderr_fd in
        Option.iter job.dir ~f:(fun dir -> Sys.chdir dir);
        let pid =
          match job.env with
          | None ->
            Unix.create_process job.prog argv
              Unix.stdin stdout stderr
          | Some env ->
            Unix.create_process_env job.prog argv env
              Unix.stdin stdout stderr
        in
        Option.iter job.dir ~f:(fun _ -> Sys.chdir cwd);
        Option.iter to_close ~f:Unix.close;
        close_std_output close_stdout;
        close_std_output close_stderr;
        Running_jobs.add
          { id
          ; job
          ; pid
          ; output_filename
          ; command_line
          ; log
          }
      done;
      let job, status = Running_jobs.wait () in
      process_done job status;
      while not (Queue.is_empty to_fill) do
        let (To_fill (ivar, x)) = Queue.pop to_fill in
        Ivar.fill ivar x
      done;
      go_rec cwd log t

  let go ?(log=Log.no_log) t =
    Lazy.force Ansi_color.setup_env_for_colors;
    Log.info log ("Workspace root: " ^ !Clflags.workspace_root);
    let cwd = Sys.getcwd () in
    if cwd <> initial_cwd then
      Printf.eprintf "Entering directory '%s'\n%!" cwd;
    go_rec cwd log t
end
