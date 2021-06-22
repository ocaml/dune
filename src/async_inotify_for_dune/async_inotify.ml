open Stdune
open Poly

(* We don't make calls to [Inotify] functions ([add_watch], [rm_watch]) in a
   separate thread because:

   - we don't think they can block for a while - Inotify doesn't release the
   OCaml lock anyway - it avoids racing with the select loop below, by
   preventing adding a watch and seeing an event about it before having filled
   the hashtable (not that we have observed this particular race). *)

module External_deps = struct
  module Unix = Unix
  module String_table = String.Table
  module Filename = Filename
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
    | Away s -> Printf.sprintf "%s -> Unknown" s
    | Into s -> Printf.sprintf "Unknown -> %s" s
    | Move (f, t) -> Printf.sprintf "%s -> %s" f t

  let to_string t =
    match t with
    | Created s -> Printf.sprintf "created %s" s
    | Unlinked s -> Printf.sprintf "unlinked %s" s
    | Moved mv -> Printf.sprintf "moved %s" (move_to_string mv)
    | Modified s -> Printf.sprintf "modified %s" s
    | Queue_overflow -> "queue overflow"
end

open Event

module Inotify_watch = struct
  type t = Inotify.watch

  let hash t = Int.hash (Inotify.int_of_watch t)

  let equal a b = Int.equal (Inotify.int_of_watch a) (Inotify.int_of_watch b)

  let to_dyn t = Int.to_dyn (Inotify.int_of_watch t)
end

module Watch_table = Hashtbl.Make (Inotify_watch)

type t =
  { fd : Unix.file_descr
  ; log_error : string -> unit
  ; watch_table : string Watch_table.t
  ; path_table : Inotify.watch String_table.t
  ; send_emit_events_job_to_scheduler : (unit -> Event.t list) -> unit
  ; select_events : Inotify.selector list
  }

type file_info = string * Unix.stats

type modify_event_selector =
  [ `Any_change
  | `Closed_writable_fd
  ]

let ( ^/ ) = Filename.concat

let add t path =
  let watch = Inotify.add_watch t.fd path t.select_events in
  Watch_table.set t.watch_table watch path;
  String_table.set t.path_table path watch

let process_raw_events t events =
  let watch_table = t.watch_table in
  let ev_kinds =
    List.filter_map events ~f:(fun (watch, ev_kinds, trans_id, fn) ->
        if
          Inotify.int_of_watch watch = -1
          (* queue overflow event is always reported on watch -1 *)
        then
          let maybe_overflow =
            List.filter_map ev_kinds ~f:(fun ev ->
                match ev with
                | Inotify.Q_overflow -> Some (ev, trans_id, "<overflow>")
                | _ -> None)
          in
          match maybe_overflow with
          | [] -> None
          | _ :: _ -> Some maybe_overflow
        else
          match Watch_table.find watch_table watch with
          | None ->
            t.log_error
              (Printf.sprintf "Events for an unknown watch (%d) [%s]\n"
                 (Inotify.int_of_watch watch)
                 (String.concat ~sep:", "
                    (List.map ev_kinds ~f:Inotify.string_of_event_kind)));
            None
          | Some path ->
            let fn =
              match fn with
              | None -> path
              | Some fn -> path ^/ fn
            in
            Some (List.map ev_kinds ~f:(fun ev -> (ev, trans_id, fn))))
    |> List.concat
  in
  let pending_mv, actions =
    List.fold_left ev_kinds ~init:(None, [])
      ~f:(fun (pending_mv, actions) (kind, trans_id, fn) ->
        let add_pending lst =
          match pending_mv with
          | None -> lst
          | Some (_, fn) -> Moved (Away fn) :: lst
        in
        match kind with
        | Inotify.Moved_from -> (Some (trans_id, fn), add_pending actions)
        | Inotify.Moved_to -> (
          match pending_mv with
          | None -> (None, Moved (Into fn) :: actions)
          | Some (m_trans_id, m_fn) ->
            if m_trans_id = trans_id then
              (None, Moved (Move (m_fn, fn)) :: actions)
            else
              (None, Moved (Away m_fn) :: Moved (Into fn) :: actions))
        | Inotify.Move_self -> (Some (trans_id, fn), add_pending actions)
        | Inotify.Create -> (None, Created fn :: add_pending actions)
        | Inotify.Delete -> (None, Unlinked fn :: add_pending actions)
        | Inotify.Modify
        | Inotify.Close_write ->
          (None, Modified fn :: add_pending actions)
        | Inotify.Q_overflow -> (None, Queue_overflow :: add_pending actions)
        | Inotify.Delete_self -> (None, add_pending actions)
        | Inotify.Access
        | Inotify.Attrib
        | Inotify.Open
        | Inotify.Ignored
        | Inotify.Isdir
        | Inotify.Unmount
        | Inotify.Close_nowrite ->
          (None, add_pending actions))
  in
  List.rev
    (match pending_mv with
    | None -> actions
    | Some (_, fn) -> Moved (Away fn) :: actions)

let pump_events t ~spawn_thread =
  let fd = t.fd in
  let () =
    spawn_thread (fun () ->
        while true do
          let _, _, _ =
            UnixLabels.select ~read:[ fd ] ~write:[] ~except:[] ~timeout:(-1.)
          in
          let events = Inotify.read fd in
          t.send_emit_events_job_to_scheduler (fun () ->
              process_raw_events t events)
        done)
  in
  ()

let create ~spawn_thread ~modify_event_selector ~log_error
    ~send_emit_events_job_to_scheduler =
  let fd = Inotify.create () in
  let watch_table = Watch_table.create 10 in
  let modify_selector : Inotify.selector =
    match modify_event_selector with
    | `Any_change -> S_Modify
    | `Closed_writable_fd -> S_Close_write
  in
  let t =
    { fd
    ; watch_table
    ; path_table = String_table.create 10
    ; select_events =
        [ S_Create
        ; S_Delete
        ; modify_selector
        ; S_Move_self
        ; S_Moved_from
        ; S_Moved_to
        ]
    ; log_error
    ; send_emit_events_job_to_scheduler
    }
  in
  pump_events t ~spawn_thread;
  t
