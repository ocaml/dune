open Import

let iter_sexps file ~f =
  Io.String_path.with_file_in ~binary:true file ~f:(fun chan ->
    let rec loop () =
      match Csexp.input_opt chan with
      | Error _ | Ok None -> ()
      | Ok (Some sexp) ->
        f sexp;
        loop ()
    in
    loop ())
;;

let rec json_of_sexp : Sexp.t -> Json.t = function
  | Atom "true" -> Json.bool true
  | Atom "false" -> Json.bool false
  | Atom s ->
    (match int_of_string s with
     | s -> Json.int s
     | exception _ ->
       (match float_of_string s with
        | s -> Json.float s
        | exception _ -> Json.string s))
  | List [] -> Json.list []
  | List xs ->
    if
      List.for_all xs ~f:(function
        | Sexp.List [ Atom _; _ ] -> true
        | _ -> false)
    then
      List.map xs ~f:(function
        | Sexp.List [ Atom k; v ] -> k, json_of_sexp v
        | _ -> assert false)
      |> Json.assoc
    else Json.list (List.map xs ~f:json_of_sexp)
;;

let invalid_sexp sexp = User_error.raise [ Pp.text "invalid sexp"; Sexp.pp sexp ]

let base_of_sexp (sexp : Sexp.t) =
  match sexp with
  | List (Atom cat :: Atom name :: ts :: rest) -> cat, name, ts, rest
  | _ -> invalid_sexp sexp
;;

type process_info =
  { prog : string
  ; args : string list
  ; dir : string option
  ; exit_code : int
  ; error : string option
  ; stderr : string
  }

let parse_process_event (sexp : Sexp.t) : process_info option =
  match base_of_sexp sexp with
  | "process", "finish", _ts, rest ->
    let rec extract_fields prog args dir exit error stderr = function
      | [] -> prog, args, dir, exit, error, stderr
      | Sexp.List [ Atom "process_args"; List arg_sexps ] :: rest ->
        let args =
          List.filter_map arg_sexps ~f:(function
            | Sexp.Atom s -> Some s
            | _ -> None)
        in
        extract_fields prog (Some args) dir exit error stderr rest
      | List [ Atom "prog"; Atom p ] :: rest ->
        extract_fields (Some p) args dir exit error stderr rest
      | List [ Atom "dir"; Atom d ] :: rest ->
        extract_fields prog args (Some d) exit error stderr rest
      | List [ Atom "exit"; Atom e ] :: rest ->
        let exit_code =
          try int_of_string e with
          | Failure _ -> 0
        in
        extract_fields prog args dir (Some exit_code) error stderr rest
      | List [ Atom "error"; Atom err ] :: rest ->
        extract_fields prog args dir exit (Some err) stderr rest
      | List [ Atom "stderr"; Atom s ] :: rest ->
        extract_fields prog args dir exit error (Some s) rest
      | _ :: rest -> extract_fields prog args dir exit error stderr rest
    in
    let prog, args, dir, exit, error, stderr =
      extract_fields None None None None None None rest
    in
    Option.map prog ~f:(fun prog ->
      { prog
      ; args = Option.value args ~default:[]
      ; dir
      ; exit_code = Option.value exit ~default:0
      ; error
      ; stderr = Option.value stderr ~default:""
      })
  | _ -> None
;;

let format_shell_command (info : process_info) : string =
  let module Escape = Escape0 in
  let cmd =
    let quoted_prog = Escape.quote_if_needed info.prog in
    let quoted_args = List.map info.args ~f:Escape.quote_if_needed in
    String.concat ~sep:" " (quoted_prog :: quoted_args)
  in
  match info.dir with
  | None -> sprintf "(%s)" cmd
  | Some dir ->
    let dir = Escape.quote_if_needed dir in
    Printf.sprintf "(cd %s && %s)" dir cmd
;;

let format_output (info : process_info) : string =
  let cmd_line = format_shell_command info in
  if info.exit_code = 0
  then cmd_line
  else (
    let error_line =
      match info.error with
      | Some err -> Printf.sprintf "# %s" err
      | None -> Printf.sprintf "# Exit code: %d" info.exit_code
    in
    let lines = [ cmd_line; error_line ] in
    let lines =
      if info.stderr <> "" then lines @ [ "# Stderr:"; info.stderr ] else lines
    in
    String.concat ~sep:"\n" lines)
;;

let iter_sexps_follow file ~f =
  Io.String_path.with_file_in ~binary:true file ~f:(fun chan ->
    let rec loop () =
      match Csexp.input_opt chan with
      | Ok (Some sexp) ->
        (* Check if exit event before processing *)
        let is_exit =
          match base_of_sexp sexp with
          | "config", "exit", _, _ -> true
          | _ -> false
          | exception _ -> true
        in
        f sexp;
        if not is_exit then loop ()
      | Ok None | Error _ ->
        (* EOF or parse error - poll and retry *)
        Unix.sleepf 0.1;
        loop ()
    in
    loop ())
;;

let times_of_sexp (sexp : Sexp.t) =
  match sexp with
  | Atom s -> float_of_string s, None
  | List [ Atom ts; Atom dur ] ->
    float_of_string ts, Some ("dur", Json.float (float_of_string dur))
  | _ -> invalid_sexp sexp
;;

let pid = lazy (Unix.getpid ())

(* Chrome trace event with parsed fields for post-processing *)
type chrome_event =
  { cat : string
  ; name : string
  ; ts : float (* seconds *)
  ; dur : float option (* seconds *)
  ; args : (string * Json.t) list
  }

(* Extract a meaningful target name from process args for chrome trace *)
let extract_target_name ~prog ~process_args =
  let prog_short =
    match String.rindex_opt prog '/' with
    | Some i -> String.sub prog ~pos:(i + 1) ~len:(String.length prog - i - 1)
    | None -> prog
  in
  let is_source_file s =
    List.exists
      ~f:(fun ext -> String.is_suffix s ~suffix:ext)
      [ ".ml"; ".mli"; ".ml-gen"; ".pp.ml"; ".pp.mli"
      ; ".mll"; ".mly"; ".atd"; ".re"; ".rei"
      ; ".c"; ".cpp"; ".h"
      ; ".exe"; ".a"; ".cmo"; ".cmx"; ".cmi"
      ]
  in
  let shorten_path s =
    let parts = String.split s ~on:'/' in
    let len = List.length parts in
    if len > 3
    then
      let rec drop n lst = if n <= 0 then lst else drop (n - 1) (List.tl lst) in
      String.concat ~sep:"/" (drop (len - 3) parts)
    else s
  in
  (* Search args in reverse for source files (target usually at end) *)
  let target =
    List.find_map (List.rev process_args) ~f:(fun arg ->
      if String.is_prefix arg ~prefix:"-"
      then None
      else if is_source_file arg
      then Some (shorten_path arg)
      else None)
  in
  match target with
  | Some t -> Printf.sprintf "%s: %s" prog_short t
  | None ->
    (* Fallback: first non-flag arg *)
    (match List.find process_args ~f:(fun s -> not (String.is_prefix s ~prefix:"-")) with
     | Some arg -> Printf.sprintf "%s: %s" prog_short (shorten_path arg)
     | None -> prog_short)
;;

(* Assign thread IDs based on overlapping execution times for chrome trace.
   Works on events with ts/dur already in microseconds to avoid float precision issues. *)
let assign_tids (events : chrome_event list) : (chrome_event * int) list =
  let sorted = List.sort events ~compare:(fun a b -> Float.compare a.ts b.ts) in
  let thread_ends = ref [] in
  List.map sorted ~f:(fun event ->
    let end_time = event.ts +. Option.value event.dur ~default:0.0 in
    let rec find_free i = function
      | [] ->
        thread_ends := !thread_ends @ [ end_time ];
        i
      | end_t :: rest ->
        if end_t <= event.ts then (
          thread_ends := List.mapi !thread_ends ~f:(fun j t -> if j = i then end_time else t);
          i)
        else find_free (i + 1) rest
    in
    let tid = find_free 0 !thread_ends in
    event, tid)
;;

(* Parse sexp into chrome_event, extracting target name for process events *)
let parse_chrome_event (sexp : Sexp.t) : chrome_event =
  let cat, name, ts_sexp, rest = base_of_sexp sexp in
  let ts, dur =
    match ts_sexp with
    | Sexp.Atom s -> float_of_string s, None
    | Sexp.List [ Atom ts_str; Atom dur_str ] ->
      float_of_string ts_str, Some (float_of_string dur_str)
    | _ -> invalid_sexp sexp
  in
  let args =
    List.map rest ~f:(function
      | Sexp.List [ Atom ("process_args" as k); List v ] ->
        ( k
        , Json.list
            (List.map v ~f:(function
               | Sexp.Atom s -> Json.string s
               | _ -> invalid_sexp sexp)) )
      | Sexp.List [ Atom k; v ] -> k, json_of_sexp v
      | _ -> invalid_sexp sexp)
  in
  (* For process finish events, extract a better name *)
  let name =
    if String.equal cat "process" && String.equal name "finish"
    then (
      let prog =
        List.find_map args ~f:(fun (k, v) ->
          if String.equal k "prog"
          then (match v with `String s -> Some s | _ -> None)
          else None)
        |> Option.value ~default:"unknown"
      in
      let process_args =
        List.find_map args ~f:(fun (k, v) ->
          if String.equal k "process_args"
          then (match v with
            | `List l ->
              Some (List.filter_map l ~f:(function `String s -> Some s | _ -> None))
            | _ -> None)
          else None)
        |> Option.value ~default:[]
      in
      extract_target_name ~prog ~process_args)
    else name
  in
  { cat; name; ts; dur; args }
;;

(* Convert chrome_event (already in microseconds) to JSON *)
let chrome_event_to_json_us ~tid (event : chrome_event) : Json.t =
  (* Keep only exit code in args *)
  let args =
    List.filter event.args ~f:(fun (k, _) -> String.equal k "exit")
  in
  let base =
    [ "name", Json.string event.name
    ; "cat", Json.string event.cat
    ; "ts", Json.float event.ts
    ; "pid", Json.int 1
    ; "tid", Json.int tid
    ; "args", Json.assoc args
    ]
  in
  let with_dur =
    match event.dur with
    | Some d -> ("dur", Json.float d) :: base
    | None -> base
  in
  let ph = match event.dur with Some _ -> "X" | None -> "i" in
  Json.assoc (("ph", Json.string ph) :: with_dur)
;;

let json_of_event ~chrome (sexp : Sexp.t) =
  let cat, name, ts, rest = base_of_sexp sexp in
  let ts, dur = times_of_sexp ts in
  let rest =
    List.map rest ~f:(function
      | Sexp.List [ Atom ("process_args" as k); List v ] ->
        ( k
        , Json.list
            (List.map v ~f:(function
               | Sexp.Atom s -> Json.string s
               | _ -> invalid_sexp sexp)) )
      | Sexp.List [ Atom k; v ] -> k, json_of_sexp v
      | _ -> invalid_sexp sexp)
  in
  let base =
    [ "cat", Json.string cat
    ; "name", Json.string name
    ; "ts", Json.float ts
    ; "args", Json.assoc rest
    ]
    @
    match dur with
    | None -> []
    | Some (x, k) -> [ x, k ]
  in
  match chrome with
  | false -> Json.assoc base
  | true ->
    let kind =
      match dur with
      | None -> "i"
      | Some _ -> "X"
    in
    Json.assoc (base @ [ "ph", Json.string kind; "pid", Json.int (Lazy.force pid) ])
;;

let cat =
  let info = Cmd.info "cat" in
  let term =
    let+ sexp =
      Arg.(
        value
        & flag
        & info [ "sexp" ] ~doc:(Some "print the trace file in pretty-printed sexp"))
    and+ chrome_trace =
      Arg.(
        value
        & flag
        & info
            [ "chrome-trace" ]
            ~doc:(Some "print the trace file in the chrome event format"))
    and+ trace_file =
      Arg.(
        value
        & opt (some string) None
        & info [ "trace-file" ] ~docv:"FILE" ~doc:(Some "Read this trace-file"))
    and+ follow =
      Arg.(
        value
        & flag
        & info [ "follow"; "f" ] ~doc:(Some "follow the trace file until the exit event"))
    in
    let mode =
      match chrome_trace, sexp with
      | true, true ->
        User_error.raise [ Pp.text "--chrome-trace and --sexp are mutually exclusive" ]
      | false, true -> `Sexp
      | true, false -> `Chrome
      | false, false -> `Json
    in
    let trace_file =
      match trace_file with
      | Some s -> s
      | None -> Path.Local.to_string Common.default_trace_file
    in
    match mode with
    | `Chrome ->
      (* Buffer events, assign tids based on overlap, output with proper names *)
      if follow
      then User_error.raise [ Pp.text "--follow is not supported with --chrome-trace" ];
      let events = ref [] in
      iter_sexps trace_file ~f:(fun sexp -> events := parse_chrome_event sexp :: !events);
      let events = List.rev !events in
      (* Filter out noisy events *)
      let events = List.filter events ~f:(fun e ->
        not (String.equal e.name "signal_received")
        && not (String.equal e.name "create-sandbox")) in
      (* Only process events with duration need tid assignment for parallelism *)
      let process_events, other_events =
        List.partition_map events ~f:(fun e ->
          match e.dur with
          | Some _ when String.equal e.cat "process" -> Left e
          | _ -> Right e)
      in
      (* Convert to microseconds BEFORE tid assignment to avoid float precision issues *)
      let to_microseconds e =
        { e with ts = e.ts *. 1_000_000.0
        ; dur = Option.map e.dur ~f:(fun d -> d *. 1_000_000.0) }
      in
      let process_events_us = List.map process_events ~f:to_microseconds in
      let other_events_us = List.map other_events ~f:to_microseconds in
      let with_tids = assign_tids process_events_us in
      (* Output as JSON array *)
      print_char '[';
      let first = ref true in
      let output json =
        if not !first then print_char ',';
        first := false;
        print_endline (Json.to_string json)
      in
      List.iter with_tids ~f:(fun (event, tid) -> output (chrome_event_to_json_us ~tid event));
      List.iter other_events_us ~f:(fun event -> output (chrome_event_to_json_us ~tid:0 event));
      print_endline "]"
    | `Sexp | `Json ->
      let print =
        match mode with
        | `Sexp -> fun sexp -> print_endline (Sexp.to_string sexp)
        | `Json ->
          fun sexp -> print_endline (Json.to_string (json_of_event ~chrome:false sexp))
        | `Chrome -> assert false
      in
      let print_with_flush sexp =
        print sexp;
        if follow then flush stdout
      in
      if follow
      then iter_sexps_follow trace_file ~f:print_with_flush
      else iter_sexps trace_file ~f:print
  in
  Cmd.v info term
;;

let commands =
  let info =
    let doc = "Display executed processes in shell format" in
    Cmd.info "commands" ~doc
  in
  let term =
    let+ trace_file =
      Arg.(
        value
        & opt (some string) None
        & info
            [ "trace-file" ]
            ~docv:"FILE"
            ~doc:(Some "Read this trace file (default: _build/trace.json)"))
    in
    let trace_file =
      match trace_file with
      | Some s -> s
      | None -> Path.Local.to_string Common.default_trace_file
    in
    iter_sexps trace_file ~f:(fun sexp ->
      match parse_process_event sexp with
      | Some info ->
        let output = format_output info in
        print_endline output
      | None -> ())
  in
  Cmd.v info term
;;

let group =
  let info =
    let doc = "Commands to view dune's event trace" in
    Cmd.info "trace" ~doc
  in
  Cmd.group info [ cat; commands ]
;;
