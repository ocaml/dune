open Stdune

(** We don't make calls to [Inotify] functions ([add_watch], [rm_watch]) in a
    separate thread because:

    - we don't think they can block for a while
    - Inotify doesn't release the OCaml lock anyway
    - it avoids racing with the select loop below, by preventing adding a watch
      and seeing an event about it before having filled the hashtable (not that
      we have observed this particular race). *)

module External_deps = struct
  module Unix = Unix
  module Table = Table

  let ( ^/ ) = Filename.concat
  let sprintf = Printf.sprintf
end

open External_deps
module Inotify = Ocaml_inotify.Inotify

module Event = struct
  type move =
    | Away of string
    | Into of string
    | Move of string * string

  type t =
    | Created of string
    | Unlinked of string
    | Modified of string
    | Moved of move
    | Queue_overflow

  let move_to_string m =
    match m with
    | Away s -> sprintf "%s -> Unknown" s
    | Into s -> sprintf "Unknown -> %s" s
    | Move (f, t) -> sprintf "%s -> %s" f t
  ;;

  let to_string t =
    match t with
    | Created s -> sprintf "created %s" s
    | Unlinked s -> sprintf "unlinked %s" s
    | Moved mv -> sprintf "moved %s" (move_to_string mv)
    | Modified s -> sprintf "modified %s" s
    | Queue_overflow -> "queue overflow"
  ;;
end

open Event

module Inotify_watch = struct
  type t = Inotify.watch

  let hash t = Int.hash (Inotify.int_of_watch t)
  let equal a b = Int.equal (Inotify.int_of_watch a) (Inotify.int_of_watch b)
  let to_dyn t = Int.to_dyn (Inotify.int_of_watch t)
end

type t =
  { fd : Unix.file_descr
  ; log_error : string -> unit
  ; watch_table : (Inotify_watch.t, string) Table.t
  ; path_table : (string, Inotify.watch) Table.t
  ; send_emit_events_job_to_scheduler : (unit -> Event.t list) -> unit
  ; select_events : Inotify.selector list
  }

type file_info = string * Unix.stats

type modify_event_selector =
  [ `Any_change
  | `Closed_writable_fd
  ]

let add t path =
  let watch = Inotify.add_watch t.fd path t.select_events in
  (* XXX why are we just overwriting existing watches? *)
  Table.set t.watch_table watch path;
  Table.set t.path_table path watch
;;

let process_raw_events t events =
  let watch_table = t.watch_table in
  let ev_kinds =
    List.concat_map events ~f:(fun (watch, ev_kinds, trans_id, fn) ->
      if Inotify.int_of_watch watch = -1
         (* queue overflow event is always reported on watch -1 *)
      then
        List.filter_map ev_kinds ~f:(fun ev ->
          match ev with
          | Inotify.Q_overflow -> Some (ev, trans_id, "<overflow>")
          | _ -> None)
      else (
        match Table.find watch_table watch with
        | None ->
          t.log_error
            (sprintf
               "Events for an unknown watch (%d) [%s]\n"
               (Inotify.int_of_watch watch)
               (String.concat
                  ~sep:", "
                  (List.map ev_kinds ~f:Inotify.string_of_event_kind)));
          []
        | Some path ->
          let fn =
            match fn with
            | None -> path
            | Some fn -> path ^/ fn
          in
          List.map ev_kinds ~f:(fun ev -> ev, trans_id, fn)))
  in
  let pending_mv, actions =
    List.fold_left
      ev_kinds
      ~init:(None, [])
      ~f:(fun (pending_mv, actions) ((kind : Inotify.event_kind), trans_id, fn) ->
        let add_pending lst =
          match pending_mv with
          | None -> lst
          | Some (_, fn) -> Moved (Away fn) :: lst
        in
        match kind with
        | Moved_from -> Some (trans_id, fn), add_pending actions
        | Moved_to ->
          (match pending_mv with
           | None -> None, Moved (Into fn) :: actions
           | Some (m_trans_id, m_fn) ->
             if m_trans_id = trans_id
             then None, Moved (Move (m_fn, fn)) :: actions
             else None, Moved (Away m_fn) :: Moved (Into fn) :: actions)
        | Move_self -> Some (trans_id, fn), add_pending actions
        | Create -> None, Created fn :: add_pending actions
        | Delete -> None, Unlinked fn :: add_pending actions
        | Modify | Close_write -> None, Modified fn :: add_pending actions
        | Q_overflow -> None, Queue_overflow :: add_pending actions
        | Delete_self -> None, add_pending actions
        | Access | Attrib | Open | Ignored | Isdir | Unmount | Close_nowrite ->
          None, add_pending actions)
  in
  List.rev
    (match pending_mv with
     | None -> actions
     | Some (_, fn) -> Moved (Away fn) :: actions)
;;

let pump_events t ~spawn_thread =
  let fd = t.fd in
  spawn_thread (fun () ->
    while true do
      match UnixLabels.select ~read:[ fd ] ~write:[] ~except:[] ~timeout:(-1.) with
      | _, _, _ ->
        let events = Inotify.read fd in
        t.send_emit_events_job_to_scheduler (fun () -> process_raw_events t events)
      | exception Unix.Unix_error (EINTR, _, _) -> ()
    done)
;;

let create
  ~spawn_thread
  ~modify_event_selector
  ~log_error
  ~send_emit_events_job_to_scheduler
  =
  let fd = Inotify.create () in
  let watch_table = Table.create (module Inotify_watch) 10 in
  let modify_selector : Inotify.selector =
    match modify_event_selector with
    | `Any_change -> S_Modify
    | `Closed_writable_fd -> S_Close_write
  in
  let t =
    { fd
    ; watch_table
    ; path_table = Table.create (module String) 10
    ; select_events =
        [ S_Create; S_Delete; modify_selector; S_Move_self; S_Moved_from; S_Moved_to ]
    ; log_error
    ; send_emit_events_job_to_scheduler
    }
  in
  pump_events t ~spawn_thread;
  t
;;
