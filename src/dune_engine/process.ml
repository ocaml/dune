open Import
open Fiber.O
module Json = Chrome_trace.Json
module Event = Chrome_trace.Event
module Timestamp = Event.Timestamp
module Action_output_on_success = Execution_parameters.Action_output_on_success
module Action_output_limit = Execution_parameters.Action_output_limit

let with_directory_annot =
  User_message.Annots.Key.create ~name:"with-directory" Path.to_dyn
;;

let limit_output = Dune_output_truncation.limit_output ~message:"TRUNCATED BY DUNE"

module Failure_mode = struct
  type ('a, 'b) t =
    | Strict : ('a, 'a) t
    | Accept : int Predicate.t -> ('a, ('a, int) result) t
    | Return : ('a, 'a * int) t

  let accepted_codes : type a b. (a, b) t -> int -> bool = function
    | Strict -> Int.equal 0
    | Accept exit_codes -> fun i -> Predicate.test exit_codes i
    | Return -> fun _ -> true
  ;;

  let map_result : type a b. (a, b) t -> int -> f:(unit -> a) -> b =
    fun mode t ~f ->
    match mode with
    | Strict -> f ()
    | Accept _ ->
      (match t with
       | 0 -> Ok (f ())
       | n -> Error n)
    | Return -> f (), t
  ;;
end

module Io = struct
  type input = Input
  type output = Output

  type 'a mode =
    | In : input mode
    | Out : output mode

  type kind =
    | File of Path.t
    | Null
      (* This argument make no sense for inputs, but it seems annoying to
         change, especially as this code is meant to change again in #4435. *)
    | Terminal of
        { output_on_success : Action_output_on_success.t
        ; output_limit : Action_output_limit.t
        }

  type status =
    | Keep_open
    | Close_after_exec
    | Closed

  type 'a channel =
    | In_chan : in_channel -> input channel
    | Out_chan : out_channel -> output channel

  let descr_of_channel : type a. a channel -> _ = function
    | In_chan ic -> Unix.descr_of_in_channel ic
    | Out_chan oc -> Unix.descr_of_out_channel oc
  ;;

  let channel_of_descr : type a. _ -> a mode -> a channel =
    fun fd mode ->
    match mode with
    | In -> In_chan (Unix.in_channel_of_descr fd)
    | Out -> Out_chan (Unix.out_channel_of_descr fd)
  ;;

  let close_channel : type a. a channel -> unit = function
    | Out_chan ch -> close_out ch
    | In_chan ch -> close_in ch
  ;;

  type 'a t =
    { kind : kind
    ; fd : Unix.file_descr Lazy.t
    ; channel : 'a channel Lazy.t
    ; mutable status : status
    }

  let terminal ch ~output_on_success ~output_limit =
    let fd = descr_of_channel ch in
    { kind = Terminal { output_on_success; output_limit }
    ; fd = lazy fd
    ; channel = lazy ch
    ; status = Keep_open
    }
  ;;

  let make_stdout = terminal (Out_chan stdout)

  let stdout =
    make_stdout ~output_on_success:Print ~output_limit:Action_output_limit.default
  ;;

  let make_stderr = terminal (Out_chan stderr)

  let stderr =
    make_stderr ~output_on_success:Print ~output_limit:Action_output_limit.default
  ;;

  let stdin =
    terminal
      (In_chan stdin)
      ~output_on_success:Print
      ~output_limit:Action_output_limit.default
  ;;

  let null (type a) (mode : a mode) : a t =
    let fd =
      match mode with
      | In -> Dev_null.in_
      | Out -> Dev_null.out
    in
    let channel = lazy (channel_of_descr (Lazy.force fd) mode) in
    { kind = Null; fd; channel; status = Keep_open }
  ;;

  let file : type a. _ -> ?perm:int -> a mode -> a t =
    fun fn ?(perm = 0o666) mode ->
    let flags =
      match mode with
      | Out -> [ Unix.O_WRONLY; O_CREAT; O_TRUNC; O_SHARE_DELETE ]
      | In -> [ O_RDONLY; O_SHARE_DELETE ]
    in
    let fd = lazy (Unix.openfile (Path.to_string fn) flags perm) in
    let channel = lazy (channel_of_descr (Lazy.force fd) mode) in
    { kind = File fn; fd; channel; status = Close_after_exec }
  ;;

  let flush : type a. a t -> unit =
    fun t ->
    if Lazy.is_val t.channel
    then (
      match Lazy.force t.channel with
      | Out_chan oc -> flush oc
      | In_chan _ -> ())
  ;;

  let fd t =
    flush t;
    Lazy.force t.fd
  ;;

  let out_channel = function
    | { channel = (lazy (Out_chan oc)); _ } -> oc
  ;;

  let release t =
    match t.status with
    | Closed -> ()
    | Keep_open -> flush t
    | Close_after_exec ->
      t.status <- Closed;
      if Lazy.is_val t.channel
      then close_channel (Lazy.force t.channel)
      else Unix.close (Lazy.force t.fd)
  ;;

  let multi_use t = { t with status = Keep_open }

  let output_on_success (out : output t) =
    match out.kind with
    | Terminal { output_on_success; _ } -> output_on_success
    | _ -> Print
  ;;

  let output_limit (out : output t) =
    match out.kind with
    | Terminal { output_limit; _ } -> output_limit
    | _ -> Action_output_limit.default
  ;;
end

type purpose =
  | Internal_job
  | Build_job of Targets.Validated.t option

type metadata =
  { loc : Loc.t option
  ; annots : User_message.Annots.t
  ; name : string option
  ; categories : string list
  ; purpose : purpose
  }

let default_metadata =
  { loc = None
  ; annots = User_message.Annots.empty
  ; purpose = Internal_job
  ; categories = []
  ; name = None
  }
;;

let create_metadata
  ?loc
  ?(annots = default_metadata.annots)
  ?name
  ?(categories = default_metadata.categories)
  ?(purpose = Internal_job)
  ()
  =
  { loc; annots; name; categories; purpose }
;;

let io_to_redirection_path (kind : Io.kind) =
  match kind with
  | Terminal _ -> None
  | Null -> Some (Path.to_string Dev_null.path)
  | File fn -> Some (Path.to_string fn)
;;

let command_line_enclosers
  ~dir
  ~(stdout_to : Io.output Io.t)
  ~(stderr_to : Io.output Io.t)
  ~(stdin_from : Io.input Io.t)
  =
  let quote fn = String.quote_for_shell (Path.to_string fn) in
  let prefix, suffix =
    match dir with
    | None -> "", ""
    | Some dir -> sprintf "(cd %s && " (quote dir), ")"
  in
  let suffix =
    match stdin_from.kind with
    | Null | Terminal _ -> suffix
    | File fn -> suffix ^ " < " ^ quote fn
  in
  let suffix =
    match
      io_to_redirection_path stdout_to.kind, io_to_redirection_path stderr_to.kind
    with
    | Some fn1, Some fn2 when String.equal fn1 fn2 ->
      suffix ^ " &> " ^ String.quote_for_shell fn1
    | path_out, path_err ->
      let add_to_suffix suffix path redirect =
        match path with
        | None -> suffix
        | Some path -> suffix ^ redirect ^ String.quote_for_shell path
      in
      let suffix = add_to_suffix suffix path_out " > " in
      add_to_suffix suffix path_err " 2> "
  in
  prefix, suffix
;;

let command_line ~prog ~args ~dir ~stdout_to ~stderr_to ~stdin_from =
  let prefix, suffix = command_line_enclosers ~dir ~stdout_to ~stderr_to ~stdin_from in
  prefix ^ String.quote_list_for_shell (prog :: args) ^ suffix
;;

module Exit_status = struct
  type error =
    | Failed of int
    | Signaled of Signal.t

  type t = (int, error) result
end

module Fancy = struct
  let split_prog s =
    let len = String.length s in
    if len = 0
    then "", "", ""
    else (
      let rec find_prog_start i =
        if i < 0
        then 0
        else (
          match s.[i] with
          | '\\' | '/' -> i + 1
          | _ -> find_prog_start (i - 1))
      in
      let prog_end =
        match s.[len - 1] with
        | '"' -> len - 1
        | _ -> len
      in
      let prog_start = find_prog_start (prog_end - 1) in
      let prog_end =
        match String.index_from s prog_start '.' with
        | None -> prog_end
        | Some i -> i
      in
      let before = String.take s prog_start in
      let after = String.drop s prog_end in
      let prog = String.sub s ~pos:prog_start ~len:(prog_end - prog_start) in
      before, prog, after)
  ;;

  let short_prog_name_of_prog s =
    let _, s, _ = split_prog s in
    s
  ;;

  let color_combos =
    [| [ `Fg_blue; `Bg_bright_green ]
     ; [ `Fg_red; `Bg_bright_yellow ]
     ; [ `Fg_yellow; `Bg_blue ]
     ; [ `Fg_magenta; `Bg_bright_cyan ]
     ; [ `Fg_bright_green; `Bg_blue ]
     ; [ `Fg_bright_yellow; `Bg_red ]
     ; [ `Fg_blue; `Bg_yellow ]
     ; [ `Fg_bright_cyan; `Bg_magenta ]
    |]
  ;;

  let colorize_prog s =
    let len = String.length s in
    if len = 0
    then Pp.verbatim s
    else (
      let before, prog, after = split_prog s in
      let styles =
        let hash = Poly.hash prog in
        let styles = color_combos.(hash mod Array.length color_combos) in
        User_message.Style.Ansi_styles styles
      in
      Pp.seq
        (Pp.verbatim before)
        (Pp.seq (Pp.tag styles (Pp.verbatim prog)) (Pp.verbatim after)))
  ;;

  let rec colorize_args = function
    | [] -> []
    | "-o" :: fn :: rest ->
      Pp.verbatim "-o"
      :: Pp.tag
           (User_message.Style.Ansi_styles [ `Bold; `Fg_green ])
           (Pp.verbatim (String.quote_for_shell fn))
      :: colorize_args rest
    | x :: rest -> Pp.verbatim (String.quote_for_shell x) :: colorize_args rest
  ;;

  let command_line ~prog ~args ~dir ~stdout_to ~stderr_to ~stdin_from =
    let open Pp.O in
    let prog = colorize_prog (String.quote_for_shell prog) in
    let pp = Pp.concat ~sep:(Pp.char ' ') (prog :: colorize_args args) in
    let prefix, suffix = command_line_enclosers ~dir ~stdout_to ~stderr_to ~stdin_from in
    Pp.verbatim prefix ++ pp ++ Pp.verbatim suffix
  ;;
end

(* Implemt the rendering for [--display short] *)
module Short_display : sig
  val pp_ok : prog:string -> purpose:purpose -> User_message.Style.t Pp.t

  val pp_error
    :  prog:string
    -> purpose:purpose
    -> has_unexpected_stdout:bool
    -> has_unexpected_stderr:bool
    -> error:Exit_status.error
    -> User_message.Style.t Pp.t
end = struct
  let pp_purpose = function
    | Internal_job -> Pp.verbatim "(internal)"
    | Build_job targets ->
      let rec split_paths targets_acc ctxs_acc = function
        | [] -> List.rev targets_acc, Context_name.Set.to_list ctxs_acc
        | path :: rest ->
          let add_ctx ctx acc =
            if Context_name.is_default ctx then acc else Context_name.Set.add acc ctx
          in
          (match Dpath.analyse_target path with
           | Other path ->
             split_paths (Path.Build.to_string path :: targets_acc) ctxs_acc rest
           | Regular (ctx, filename) ->
             split_paths
               (Path.Source.to_string filename :: targets_acc)
               (add_ctx ctx ctxs_acc)
               rest
           | Alias (ctx, name) ->
             split_paths
               (("alias " ^ Path.Source.to_string name) :: targets_acc)
               (add_ctx ctx ctxs_acc)
               rest
           | Anonymous_action ctx ->
             split_paths ("(internal)" :: targets_acc) (add_ctx ctx ctxs_acc) rest)
      in
      let target_names, contexts =
        let targets =
          match targets with
          | None -> []
          | Some targets ->
            Targets.Validated.fold
              targets
              ~init:[]
              ~file:(fun path acc -> path :: acc)
              ~dir:(fun dir acc -> dir :: acc)
            |> List.rev
        in
        split_paths [] Context_name.Set.empty targets
      in
      let targets =
        List.map target_names ~f:Filename.split_extension_after_dot
        |> String.Map.of_list_multi
        |> String.Map.to_list_map ~f:(fun prefix suffixes ->
          match suffixes with
          | [] -> assert false
          | [ suffix ] -> prefix ^ suffix
          | _ -> sprintf "%s{%s}" prefix (String.concat ~sep:"," suffixes))
        |> String.concat ~sep:","
      in
      let pp = Pp.verbatim targets in
      (match contexts with
       | [] -> pp
       | l ->
         let open Pp.O in
         pp
         ++ Pp.char ' '
         ++ Pp.tag
              User_message.Style.Details
              (Pp.char '['
               ++ Pp.concat_map l ~sep:(Pp.char ',') ~f:(fun ctx ->
                 Pp.verbatim (Context_name.to_string ctx))
               ++ Pp.char ']'))
  ;;

  let progname_and_purpose ~tag ~prog ~purpose =
    let open Pp.O in
    let progname = sprintf "%12s" (Fancy.short_prog_name_of_prog prog) in
    Pp.tag tag (Pp.verbatim progname) ++ Pp.char ' ' ++ pp_purpose purpose
  ;;

  let pp_ok = progname_and_purpose ~tag:Ok

  let pp_error
    ~prog
    ~purpose
    ~has_unexpected_stdout
    ~has_unexpected_stderr
    ~(error : Exit_status.error)
    =
    let open Pp.O in
    let msg =
      match error with
      | Signaled signame -> sprintf "(got signal %s)" (Signal.name signame)
      | Failed n ->
        let unexpected_outputs =
          List.filter_map
            [ has_unexpected_stdout, "stdout"; has_unexpected_stderr, "stderr" ]
            ~f:(fun (b, name) -> Option.some_if b name)
        in
        (match n, unexpected_outputs with
         | 0, _ :: _ ->
           sprintf
             "(had unexpected output on %s)"
             (String.enumerate_and unexpected_outputs)
         | _ -> sprintf "(exit %d)" n)
    in
    progname_and_purpose ~prog ~tag:Error ~purpose
    ++ Pp.char ' '
    ++ Pp.tag User_message.Style.Error (Pp.verbatim msg)
  ;;
end

let cmdline_approximate_length prog args =
  List.fold_left args ~init:(String.length prog) ~f:(fun acc arg ->
    acc + String.length arg)
;;

let pp_id id =
  let open Pp.O in
  Pp.char '['
  ++ Pp.tag User_message.Style.Id (Pp.textf "%d" (Running_jobs.Id.to_int id))
  ++ Pp.char ']'
;;

module Handle_exit_status : sig
  open Exit_status

  val verbose
    :  ('a, error) result
    -> id:Running_jobs.Id.t
    -> metadata:metadata
    -> output:string
    -> command_line:User_message.Style.t Pp.t
    -> dir:Path.t option
    -> 'a

  val non_verbose
    :  ('a, error) result
    -> verbosity:Display.t
    -> metadata:metadata
    -> output:string
    -> prog:string
    -> command_line:string
    -> dir:Path.t option
    -> has_unexpected_stdout:bool
    -> has_unexpected_stderr:bool
    -> 'a
end = struct
  open Exit_status

  type output =
    | No_output
    | Has_output of
        { with_color : User_message.Style.t Pp.t
        ; without_color : string
        ; has_embedded_location : bool
        }

  let pp_output = function
    | No_output -> []
    | Has_output t -> [ t.with_color ]
  ;;

  let parse_output = function
    | "" -> No_output
    | s ->
      let with_color =
        Pp.map_tags (Ansi_color.parse s) ~f:(fun styles ->
          User_message.Style.Ansi_styles styles)
      in
      let without_color = Ansi_color.strip s in
      let has_embedded_location = String.is_prefix ~prefix:"File " without_color in
      Has_output { with_color; without_color; has_embedded_location }
  ;;

  let get_loc_and_annots ~dir ~metadata ~output =
    let { loc; annots; _ } = metadata in
    let dir = Option.value dir ~default:Path.root in
    let annots = User_message.Annots.set annots with_directory_annot dir in
    let annots =
      match output with
      | No_output -> annots
      | Has_output output ->
        if output.has_embedded_location
        then (
          let annots =
            User_message.Annots.set annots User_message.Annots.has_embedded_location ()
          in
          match Compound_user_error.parse_output ~dir output.without_color with
          | [] -> annots
          | errors -> User_message.Annots.set annots Compound_user_error.annot errors)
        else annots
    in
    loc, annots
  ;;

  let fail ~loc ~annots paragraphs =
    (* We don't use [User_error.make] as it would add the "Error: " prefix. We
       don't need this prefix as it is already included in the output of the
       command. *)
    raise (User_error.E (User_message.make ?loc ~annots paragraphs))
  ;;

  let verbose t ~id ~metadata ~output ~command_line ~dir =
    let open Pp.O in
    let output = parse_output output in
    match t with
    | Ok n ->
      (match output with
       | No_output -> ()
       | Has_output output ->
         Console.print_user_message
           (User_message.make
              [ Pp.tag User_message.Style.Kwd (Pp.verbatim "Output")
                ++ pp_id id
                ++ Pp.char ':'
              ; output.with_color
              ]));
      n
    | Error err ->
      let msg =
        match err with
        | Failed n -> sprintf "exited with code %d" n
        | Signaled signame -> sprintf "got signal %s" (Signal.name signame)
      in
      let loc, annots = get_loc_and_annots ~dir ~metadata ~output in
      fail
        ~loc
        ~annots
        ((Pp.tag User_message.Style.Kwd (Pp.verbatim "Command")
          ++ Pp.space
          ++ pp_id id
          ++ Pp.space
          ++ Pp.text msg
          ++ Pp.char ':')
         :: (Pp.tag User_message.Style.Prompt (Pp.char '$') ++ Pp.char ' ' ++ command_line)
         :: pp_output output)
  ;;

  let non_verbose
    t
    ~(verbosity : Display.t)
    ~metadata
    ~output
    ~prog
    ~command_line
    ~dir
    ~has_unexpected_stdout
    ~has_unexpected_stderr
    =
    let output = parse_output output in
    let show_command =
      !Clflags.always_show_command_line
      || (* We want to show command lines in the CI, but not when running inside
            dune. Otherwise tests would yield different result whether they are
            executed locally or in the CI. *)
      (Execution_env.inside_ci && not Execution_env.inside_dune)
    in
    let add_command_line paragraphs =
      if show_command
      then Pp.tag User_message.Style.Details (Pp.verbatim command_line) :: paragraphs
      else paragraphs
    in
    let purpose = metadata.purpose in
    match t with
    | Ok n ->
      let paragraphs =
        match output with
        | No_output -> []
        | Has_output output -> add_command_line [ output.with_color ]
      in
      let paragraphs =
        match verbosity, purpose, output with
        | Short, Build_job _, _ | Short, Internal_job, Has_output _ ->
          Short_display.pp_ok ~prog ~purpose :: paragraphs
        | _ -> paragraphs
      in
      if not (List.is_empty paragraphs)
      then Console.print_user_message (User_message.make paragraphs);
      n
    | Error error ->
      let loc, annots = get_loc_and_annots ~dir ~metadata ~output in
      let paragraphs =
        match verbosity with
        | Short ->
          Short_display.pp_error
            ~prog
            ~purpose
            ~error
            ~has_unexpected_stdout
            ~has_unexpected_stderr
          :: add_command_line (pp_output output)
        | _ ->
          add_command_line
            (match output with
             | Has_output output -> [ output.with_color ]
             | No_output ->
               (* If the command has no output, we need to say something.
                  Otherwise it's not clear what's going on. *)
               (match error with
                | Failed n -> [ Pp.textf "Command exited with code %d." n ]
                | Signaled signame ->
                  [ Pp.textf "Command got signal %s." (Signal.name signame) ]))
      in
      fail ~loc ~annots paragraphs
  ;;
end

type t =
  { started_at : float
  ; pid : Pid.t
  ; response_file : Path.t option
  ; stdout : Path.t option
  ; stderr : Path.t option
  ; stdout_on_success : Action_output_on_success.t
  ; stderr_on_success : Action_output_on_success.t
  ; stdout_limit : Action_output_limit.t
  ; stderr_limit : Action_output_limit.t
  }

module Result = struct
  type nonrec process = t

  module Out = struct
    type state =
      | No_capture
      | File of Path.t
      | Read of string
      | Closed

    type t =
      { on_success : Action_output_on_success.t
      ; limit : Action_output_limit.t
      ; mutable unexpected_output : bool
      ; mutable state : state
      }

    let get t =
      match t.state with
      | Closed ->
        Code_error.raise "it is an error to access the contents after it's closed" []
      | No_capture -> ""
      | Read s -> s
      | File p ->
        let contents = Stdune.Io.read_file p |> limit_output ~n:t.limit in
        Temp.destroy File p;
        t.state <- Read contents;
        contents
    ;;

    let cleanup_file t =
      match t.state with
      | File p -> Temp.destroy File p
      | _ -> ()
    ;;

    let close t =
      cleanup_file t;
      t.state <- Closed
    ;;

    let make f ~on_success ~limit =
      let state =
        match f with
        | None -> No_capture
        | Some p -> File p
      in
      { state; on_success; limit; unexpected_output = false }
    ;;

    let check_unexpected_output_and_swallow_on_success t =
      (match t.state with
       | Closed -> Code_error.raise "already closed" []
       | _ -> ());
      match t.on_success with
      | Must_be_empty -> t.unexpected_output <- get t <> ""
      | Swallow ->
        cleanup_file t;
        t.state <- No_capture
      | Print -> ()
    ;;
  end

  type t =
    { stdout : Out.t
    ; stderr : Out.t
    ; exit_status : Exit_status.t
    }

  let close t =
    Out.close t.stdout;
    Out.close t.stderr
  ;;

  let make
    ({ stdout_on_success
     ; stderr_on_success
     ; stdout_limit
     ; stderr_limit
     ; stdout
     ; stderr
     ; _
     } :
      process)
    (process_info : Proc.Process_info.t)
    fail_mode
    =
    let stdout = Out.make stdout ~on_success:stdout_on_success ~limit:stdout_limit in
    let stderr = Out.make stderr ~on_success:stderr_on_success ~limit:stderr_limit in
    let exit_status : Exit_status.t =
      match process_info.status with
      | WEXITED n when Failure_mode.accepted_codes fail_mode n ->
        Out.check_unexpected_output_and_swallow_on_success stdout;
        Out.check_unexpected_output_and_swallow_on_success stderr;
        if stdout.unexpected_output || stderr.unexpected_output
        then Error (Failed n)
        else Ok n
      | WEXITED n -> Error (Failed n)
      | WSIGNALED n -> Error (Signaled (Signal.of_int n))
      | WSTOPPED _ -> assert false
    in
    { stdout; stderr; exit_status }
  ;;
end

let report_process_finished
  stats
  ~metadata
  ~dir
  ~prog
  ~pid
  ~args
  ~started_at
  ~exit_status
  ~stdout
  ~stderr
  (times : Proc.Times.t)
  =
  let common =
    let name =
      match metadata.name with
      | Some n -> n
      | None -> Filename.basename prog
    in
    let ts = Timestamp.of_float_seconds started_at in
    Event.common_fields ~cat:("process" :: metadata.categories) ~name ~ts ()
  in
  let always =
    [ "process_args", `List (List.map args ~f:(fun arg -> `String arg))
    ; "pid", `Int (Pid.to_int pid)
    ]
  in
  let extended =
    if not (Dune_stats.extended_build_job_info stats)
    then []
    else (
      let targets =
        match metadata.purpose with
        | Internal_job -> []
        | Build_job None -> []
        | Build_job (Some targets) -> Targets.Validated.to_trace_args targets
      in
      let exit =
        match exit_status with
        | Ok n -> [ "exit", `Int n ]
        | Error (Exit_status.Failed n) ->
          [ "exit", `Int n; "error", `String (sprintf "exited with code %d" n) ]
        | Error (Signaled s) ->
          [ "exit", `Int (Signal.to_int s)
          ; "error", `String (sprintf "got signal %s" (Signal.name s))
          ]
      in
      let output name s =
        match Result.Out.get s with
        | "" -> []
        | s -> [ name, `String s ]
      in
      List.concat
        [ [ "prog", `String prog
          ; "dir", `String (Option.map ~f:Path.to_string dir |> Option.value ~default:".")
          ]
        ; targets
        ; exit
        ; output "stdout" stdout
        ; output "stderr" stderr
        ])
  in
  let args = always @ extended in
  let dur = Event.Timestamp.of_float_seconds times.elapsed_time in
  let event = Event.complete ~args ~dur common in
  Dune_stats.emit stats event
;;

let set_temp_dir_when_running_actions = ref true

let await { response_file; pid; _ } =
  let+ process_info, termination_reason =
    Scheduler.wait_for_build_process pid ~is_process_group_leader:true
  in
  Option.iter response_file ~f:Path.unlink_exn;
  process_info, termination_reason
;;

let spawn
  ?dir
  ?(env = Env.initial)
  ~(stdout : _ Io.t)
  ~(stderr : _ Io.t)
  ~(stdin : _ Io.t)
  ~prog
  ~args
  ()
  =
  let stdout_on_success = Io.output_on_success stdout
  and stderr_on_success = Io.output_on_success stderr in
  let stdout_limit = Io.output_limit stdout
  and stderr_limit = Io.output_limit stderr in
  let (stdout_capture, stdout), (stderr_capture, stderr) =
    match stdout.kind, stderr.kind with
    | (Terminal _, _ | _, Terminal _) when !Clflags.capture_outputs ->
      let capture ~suffix =
        let fn = Temp.create File ~prefix:"dune" ~suffix in
        Some fn, Io.file fn Io.Out
      in
      let stdout_capture, stdout =
        match stdout.kind with
        | Terminal _ ->
          Io.flush stdout;
          capture ~suffix:"stdout"
        | _ -> None, stdout
      in
      let stderr =
        match stdout.kind, stderr.kind with
        | ( Terminal { output_on_success = Print; _ }
          , Terminal { output_on_success = Print; _ } )
        | ( Terminal { output_on_success = Swallow; _ }
          , Terminal { output_on_success = Swallow; _ } ) ->
          (* We don't merge when both are [Must_be_empty]. If we did and an
             action had unexpected output on both stdout and stderr the
             error message would be "has unexpected output on stdout". With
             the current code, it is "has unexpected output on stdout and
             stderr", which is more precise. *)
          Io.flush stderr;
          None, stdout
        | _, Terminal _ ->
          Io.flush stderr;
          capture ~suffix:"stderr"
        | _ -> None, stderr
      in
      (stdout_capture, stdout), stderr
    | _ -> (None, stdout), (None, stderr)
  in
  let prog_str = Path.reach_for_running ?from:dir prog in
  let args, response_file =
    if Sys.win32 && cmdline_approximate_length prog_str args >= 1024
    then (
      match Response_file.get ~prog with
      | Not_supported -> args, None
      | Zero_terminated_strings arg ->
        let fn = Temp.create File ~prefix:"responsefile" ~suffix:"data" in
        Stdune.Io.with_file_out fn ~f:(fun oc ->
          List.iter args ~f:(fun arg ->
            output_string oc arg;
            output_char oc '\000'));
        [ arg; Path.to_string fn ], Some fn)
    else args, None
  in
  let started_at =
    (* jeremiedimino: I think we should do this just before the [execve]
       in the stub for [Spawn.spawn] to be as precise as possible *)
    Unix.gettimeofday ()
  in
  let pid =
    let env =
      let env =
        match !set_temp_dir_when_running_actions with
        | true -> Dtemp.add_to_env env
        | false -> env
      in
      Env.to_unix env |> Spawn.Env.of_list
    in
    let stdout = Io.fd stdout in
    let stderr = Io.fd stderr in
    let stdin = Io.fd stdin in
    let argv = prog_str :: args in
    Spawn.spawn
      ()
      ~prog:prog_str
      ~argv
      ~env
      ~stdout
      ~stderr
      ~stdin
      ~setpgid:Spawn.Pgid.new_process_group
      ~cwd:
        (match dir with
         | None -> Inherit
         | Some dir -> Path (Path.to_string dir))
    |> Pid.of_int
  in
  Io.release stdout;
  Io.release stderr;
  { started_at
  ; pid
  ; response_file
  ; stdout = stdout_capture
  ; stderr = stderr_capture
  ; stdout_on_success
  ; stderr_on_success
  ; stdout_limit
  ; stderr_limit
  }
;;

let run_internal
  ?dir
  ~(display : Display.t)
  ?(stdout_to = Io.stdout)
  ?(stderr_to = Io.stderr)
  ?(stdin_from = Io.null In)
  ?env
  ?(metadata = default_metadata)
  fail_mode
  prog
  args
  =
  Scheduler.with_job_slot (fun _cancel (config : Scheduler.Config.t) ->
    let dir =
      match dir with
      | None -> dir
      | Some p -> if Path.is_root p then None else Some p
    in
    let id = Running_jobs.Id.gen () in
    let prog_str = Path.reach_for_running ?from:dir prog in
    let command_line =
      command_line ~prog:prog_str ~args ~dir ~stdout_to ~stderr_to ~stdin_from
    in
    let fancy_command_line =
      match display with
      | Verbose ->
        let open Pp.O in
        let cmdline =
          Fancy.command_line ~prog:prog_str ~args ~dir ~stdout_to ~stderr_to ~stdin_from
        in
        Console.print_user_message
          (User_message.make
             [ Pp.tag User_message.Style.Kwd (Pp.verbatim "Running")
               ++ pp_id id
               ++ Pp.verbatim ": "
               ++ cmdline
             ]);
        cmdline
      | _ -> Pp.nop
    in
    let t =
      spawn ?dir ?env ~stdout:stdout_to ~stderr:stderr_to ~stdin:stdin_from ~prog ~args ()
    in
    let* () =
      let description =
        (* CR-soon amokhov: What happens with actions attached to aliases? Do they go into
           [Build_job None] category? Can produce more informative description for them? *)
        match metadata.purpose with
        | Internal_job -> Pp.text "(internal)"
        | Build_job None -> Pp.text "(no targets)"
        | Build_job (Some target) ->
          Targets.Validated.head target
          |> Path.Build.to_string_maybe_quoted
          |> Pp.verbatim
      in
      Running_jobs.start id t.pid ~description ~started_at:t.started_at
    in
    let* process_info, termination_reason = await t in
    let+ () = Running_jobs.stop id in
    let result = Result.make t process_info fail_mode in
    let times =
      { Proc.Times.elapsed_time = process_info.end_time -. t.started_at
      ; resource_usage = process_info.resource_usage
      }
    in
    Option.iter config.stats ~f:(fun stats ->
      report_process_finished
        stats
        ~metadata
        ~dir
        ~prog:prog_str
        ~pid:t.pid
        ~args
        ~started_at:t.started_at
        ~exit_status:result.exit_status
        ~stdout:result.stdout
        ~stderr:result.stderr
        times);
    match termination_reason with
    | Cancel ->
      (* if the cancellation token was fired, then we:

         1) aren't interested in printing the output from the cancelled job

         2) allowing callers to continue work with the already stale value
         we're about to return. *)
      Result.close result;
      raise (Memo.Non_reproducible Scheduler.Run.Build_cancelled)
    | Normal ->
      let output = Result.Out.get result.stdout ^ Result.Out.get result.stderr in
      Log.command ~command_line ~output ~exit_status:process_info.status;
      let res =
        match display, result.exit_status, output with
        | Quiet, Ok n, "" -> n (* Optimisation for the common case *)
        | Verbose, _, _ ->
          Handle_exit_status.verbose
            result.exit_status
            ~id
            ~metadata
            ~dir
            ~command_line:fancy_command_line
            ~output
        | _ ->
          Handle_exit_status.non_verbose
            result.exit_status
            ~prog:prog_str
            ~dir
            ~command_line
            ~output
            ~metadata
            ~verbosity:display
            ~has_unexpected_stdout:result.stdout.unexpected_output
            ~has_unexpected_stderr:result.stderr.unexpected_output
      in
      Result.close result;
      res, times)
;;

let run ?dir ~display ?stdout_to ?stderr_to ?stdin_from ?env ?metadata fail_mode prog args
  =
  let+ run =
    run_internal
      ?dir
      ~display
      ?stdout_to
      ?stderr_to
      ?stdin_from
      ?env
      ?metadata
      fail_mode
      prog
      args
    >>| fst
  in
  Failure_mode.map_result fail_mode run ~f:ignore
;;

let run_with_times
  ?dir
  ~display
  ?stdout_to
  ?stderr_to
  ?stdin_from
  ?env
  ?metadata
  fail_mode
  prog
  args
  =
  let+ code, times =
    run_internal
      ?dir
      ~display
      ?stdout_to
      ?stderr_to
      ?stdin_from
      ?env
      ?metadata
      fail_mode
      prog
      args
  in
  Failure_mode.map_result fail_mode code ~f:(fun () -> times)
;;

let run_capture_gen
  ?dir
  ~display
  ?stderr_to
  ?stdin_from
  ?env
  ?metadata
  fail_mode
  prog
  args
  ~f
  =
  let fn = Temp.create File ~prefix:"dune" ~suffix:"output" in
  let+ run =
    run_internal
      ?dir
      ~display
      ~stdout_to:(Io.file fn Io.Out)
      ?stderr_to
      ?stdin_from
      ?env
      ?metadata
      fail_mode
      prog
      args
    >>| fst
  in
  Failure_mode.map_result fail_mode run ~f:(fun () ->
    let x = f fn in
    Temp.destroy File fn;
    x)
;;

let run_capture = run_capture_gen ~f:Stdune.Io.read_file
let run_capture_lines = run_capture_gen ~f:Stdune.Io.lines_of_file
let run_capture_zero_separated = run_capture_gen ~f:Stdune.Io.zero_strings_of_file

let run_capture_line
  ?dir
  ~display
  ?stderr_to
  ?stdin_from
  ?env
  ?metadata
  fail_mode
  prog
  args
  =
  run_capture_gen
    ?dir
    ~display
    ?stderr_to
    ?stdin_from
    ?env
    ?metadata
    fail_mode
    prog
    args
    ~f:(fun fn ->
      match Stdune.Io.lines_of_file fn with
      | [ x ] -> x
      | l ->
        let cmdline =
          let prog = Path.reach_for_running ?from:dir prog in
          let prog_display = String.concat (prog :: args) ~sep:" " in
          match dir with
          | None -> prog_display
          | Some dir -> sprintf "cd %s && %s" (Path.to_string dir) prog_display
        in
        let { loc; annots; _ } = Option.value metadata ~default:default_metadata in
        (match l with
         | [] ->
           User_error.raise
             ?loc
             ~annots
             [ Pp.textf "Command returned nothing: %s" cmdline ]
         | _ ->
           User_error.raise
             ?loc
             ~annots
             [ Pp.textf "command returned too many lines: %s" cmdline
             ; Pp.vbox
                 (Pp.concat_map l ~sep:Pp.cut ~f:(fun line ->
                    Pp.seq (Pp.verbatim "> ") (Pp.verbatim line)))
             ]))
;;
