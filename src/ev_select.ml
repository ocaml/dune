
(* Signal handling via the self-pipe trick *)

let nsig = 128

type signal_handler_info =
  | Signal_event_disabled
  | Signal_event_enabled of
    (* pipe_rd : *) Unix.file_descr *
    (* pipe_wr : *) Unix.file_descr

let signal_handlers = Array.make nsig Signal_event_disabled

let get_sig_info s = signal_handlers.(-s-1)
let set_sig_info s i = signal_handlers.(-s-1) <- i

let make_sig_info () =
  let pipe_rd, pipe_wr = Unix.pipe () in
  Unix.set_close_on_exec pipe_rd;
  Unix.set_nonblock pipe_rd;
  Unix.set_close_on_exec pipe_wr;
  Unix.set_nonblock pipe_wr;
  Signal_event_enabled (pipe_rd, pipe_wr)

let byte = Bytes.make 1 '!'
let handle_signal s =
  match get_sig_info s with
  | Signal_event_disabled ->
     failwith "internal error: signal received when handler disabled"
  | Signal_event_enabled (_, pipe_wr) ->
     match Unix.single_write pipe_wr byte 0 1 with
     | 1 -> ()
     | _ -> failwith "please return your operating system for a refund"
     | exception (Unix.Unix_error ((EAGAIN|EWOULDBLOCK), _, _)) ->
        (* this is OK *)
        ()

let check_valid_signal s =
  if s = Sys.sigchld then
    raise (Invalid_argument ("Signal events cannot be used for SIGCHLD"));
  if s >= 0 || s < -nsig then
    raise (Invalid_argument ("Unknown signal " ^ string_of_int s))

let enable_signal_event s =
  check_valid_signal s;
  match get_sig_info s with
  | Signal_event_enabled _ ->
     raise (Invalid_argument ("Signal events already enabled for signal " ^ string_of_int s))
  | Signal_event_disabled ->
     set_sig_info s (make_sig_info ());
     Sys.set_signal s (Sys.Signal_handle handle_signal)

let disable_signal_event s =
  check_valid_signal s;
  match get_sig_info s with
  | Signal_event_disabled ->
     raise (Invalid_argument ("Signal events already disabled for signal " ^ string_of_int s))
  | Signal_event_enabled (pipe_rd, pipe_wr) ->
     Sys.set_signal s Sys.Signal_default;
     Unix.close pipe_rd;
     Unix.close pipe_wr;
     set_sig_info s Signal_event_disabled

(* Drains a pipe by reading until EWOULDBLOCK.
   Returns true if the pipe was nonempty. *)
let drain_pipe pipe_rd =
  let buf = Bytes.create 1 in
  let rec go status =
    match Unix.read pipe_rd buf 0 1 with
    | 1 -> go true
    | _ -> failwith "uh oh"
    | exception (Unix.Unix_error (EINTR, _, _)) -> go status
    | exception (Unix.Unix_error ((EWOULDBLOCK|EAGAIN),_,_)) -> status in
  go false

let acknowledge_signal s =
  match get_sig_info s with
  | Signal_event_disabled ->
     raise (Invalid_argument ("Signal " ^ string_of_int s ^ " does not have events enabled"))
  | Signal_event_enabled (pipe_rd, _) ->
     if not (drain_pipe pipe_rd) then
       raise (Invalid_argument ("Signal " ^ string_of_int s ^ " was not pending, so should not have been acknowledged"))



(* Tracking of already-terminated processes *)

let terminated_processes : (int, Unix.process_status) Hashtbl.t = Hashtbl.create 20

let child_terminated pid =
  if Hashtbl.mem terminated_processes pid then
    true
  else
    match Unix.waitpid [Unix.WNOHANG] pid with
    | 0, _ -> false
    | _, status ->
       Hashtbl.add terminated_processes pid status;
       true

let acknowledge_termination pid =
  match Hashtbl.find terminated_processes pid with
  | exception Not_found ->
     raise (Invalid_argument ("Process " ^ string_of_int pid ^ " has not terminated, or has already been acknowledged"))
  | status ->
     Hashtbl.remove terminated_processes pid;
     status


(* select implementation *)

type event =
  | Ev_signal of int
  | Ev_child_process of int
  | Ev_read_fd of Unix.file_descr
  | Ev_write_fd of Unix.file_descr
  | Ev_urgent_fd of Unix.file_descr

let rec map_app f xs tail =
  match xs with
  | [] -> tail
  | x :: xs -> f x :: map_app f xs tail

let rec retry_on_eintr f =
  try f ()
  with Unix.Unix_error (Unix.EINTR, _, _) -> retry_on_eintr f

let select ?(timeout = (-1.0)) events =
  let rec go timeout read write urgent sigs pids pids_terminated = function
    | [] ->
       let events =
         let read, write, urgent =
           retry_on_eintr (fun () -> Unix.select read write urgent timeout) in
         let read_sig_fd, read_other =
           List.partition (fun fd -> List.mem_assoc fd sigs) read in
         (* This is O(n^2), but n is the number of active signal handlers
            (which is never large) *)
         let read_sigs =
           List.map (fun fd -> List.assoc fd sigs) read_sig_fd in
         let read_other, newly_terminated =
           match pids with
           | [] -> read_other, []
           | _ ->
              let sigchld_fd =
                match get_sig_info Sys.sigchld with
                | Signal_event_disabled -> assert false
                | Signal_event_enabled (pipe_rd, _) -> pipe_rd in
              if not (List.mem sigchld_fd read_other) then
                read_other, []
              else
                let read_other = List.filter (fun x -> x <> sigchld_fd) read_other in
                let b = drain_pipe sigchld_fd in
                assert b;
                let terminated = List.filter child_terminated pids in
                read_other, terminated in
         map_app (fun x -> Ev_signal x) read_sigs
         @@ map_app (fun x -> Ev_read_fd x) read_other
         @@ map_app (fun x -> Ev_write_fd x) write
         @@ map_app (fun x -> Ev_urgent_fd x) urgent
         @@ map_app (fun x -> Ev_child_process x) newly_terminated
         @@ map_app (fun x -> Ev_child_process x) pids_terminated [] in
       begin match events with
       | [] when timeout < 0. ->
          (* This is possible if we got a spurious SIGCHLD:
             i.e. a process terminated, but it wasn't one we're interested in *)
          go timeout read write urgent sigs pids pids_terminated [] (* retry *)
       | ev ->
          (* BUG: spurious SIGCHLDs can make us return before the timer's expired *)
          ev
       end
    | Ev_read_fd fd :: rest ->
       go timeout (fd :: read) write urgent sigs pids pids_terminated rest
    | Ev_write_fd fd :: rest ->
       go timeout read (fd :: write) urgent sigs pids pids_terminated rest
    | Ev_urgent_fd fd :: rest ->
       go timeout read write (fd :: urgent) sigs pids pids_terminated rest
    | Ev_signal s :: rest ->
       check_valid_signal s;
       begin match get_sig_info s with
       | Signal_event_disabled ->
          raise (Invalid_argument ("Signal " ^ string_of_int s ^ " must have events enabled before using select"))
       | Signal_event_enabled (pipe_rd, _) ->
          go timeout (pipe_rd :: read) write urgent ((pipe_rd, s) :: sigs) pids pids_terminated rest
       end
    | Ev_child_process pid :: rest ->
       if child_terminated pid then
         (* This child has already terminated, so select should not block.
            We still want to do the select, though, so that we report *all* of the
            events that have occurred, not just this termination. So, we still do
            a select, but the timeout is zero *)
         go 0. read write urgent sigs pids (pid :: pids_terminated) rest
       else
         (* This child had not already terminated when we tested it, so
            if it terminates after that point we'll see a SIGCHLD *)
         let sigchld_rd =
           match get_sig_info Sys.sigchld with
           | Signal_event_disabled ->
              (match make_sig_info () with
              | Signal_event_disabled -> assert false
              | Signal_event_enabled (pipe_rd, _) as si ->
                 set_sig_info Sys.sigchld si;
                 Sys.set_signal Sys.sigchld (Sys.Signal_handle handle_signal);
                pipe_rd)
           | Signal_event_enabled (pipe_rd, _ ) -> pipe_rd in
         go timeout (sigchld_rd :: read) write urgent sigs (pid :: pids) pids_terminated rest in
  match events with
  | [] when timeout < 0. ->
     raise (Invalid_argument "Ev_select.select: nothing to wait for")
  | events ->
     go timeout [] [] [] [] [] [] events
