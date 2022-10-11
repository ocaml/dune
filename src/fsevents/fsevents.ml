open Stdune

external available : unit -> bool = "dune_fsevents_available"

module State : sig
  type 'a t

  val create : 'a -> 'a t

  type 'a ref

  val get : 'a ref -> 'a

  val set : 'a ref -> 'a -> unit

  val critical_section : 'a t -> ('a ref -> 'b) -> 'b
end = struct
  type 'a t =
    { mutex : Mutex.t
    ; mutable data : 'a
    }

  type 'a ref = 'a t

  let set t a = t.data <- a

  let get t = t.data

  let create data = { mutex = Mutex.create (); data }

  let critical_section (type a) (t : a t) f =
    Mutex.lock t.mutex;
    Fun.protect (fun () -> f t) ~finally:(fun () -> Mutex.unlock t.mutex)
end

module RunLoop = struct
  module Raw = struct
    type t

    external in_current_thread : unit -> t = "dune_fsevents_runloop_current"

    (* After this function terminates, the reference to [t] is no longer
       valid *)
    external run_current_thread : t -> unit = "dune_fsevents_runloop_run"
  end

  type state =
    | Idle of Raw.t
    | Running of Raw.t
    | Stopped

  type t = state State.t

  let in_current_thread () = State.create (Idle (Raw.in_current_thread ()))

  let stop (t : t) =
    State.critical_section t (fun t ->
        match State.get t with
        | Running _ -> State.set t Stopped
        | Stopped -> ()
        | Idle _ -> Code_error.raise "RunLoop.stop: not started" [])

  let run_current_thread t =
    let w =
      State.critical_section t (fun t ->
          match State.get t with
          | Stopped -> Code_error.raise "RunLoop.run_current_thread: stopped" []
          | Running _ ->
            Code_error.raise "RunLoop.run_current_thread: running" []
          | Idle w ->
            State.set t (Running w);
            w)
    in
    let res = try Ok (Raw.run_current_thread w) with exn -> Error exn in
    stop t;
    res
end

module Event = struct
  module Id = struct
    type t
  end

  type t =
    { path : string
    ; id : Id.t
    ; flags : Int32.t
    }

  module Raw = struct
    type t =
      { must_scan_subdirs : bool
      ; user_dropped : bool
      ; kernel_dropped : bool
      ; event_ids_wrapped : bool
      ; history_done : bool
      ; root_changed : bool
      ; mount : bool
      ; unmount : bool
      ; item_created : bool
      ; item_removed : bool
      ; item_inode_meta_mod : bool
      ; item_renamed : bool
      ; item_modified : bool
      ; item_finder_info_mod : bool
      ; item_change_owner : bool
      ; item_xattr_mod : bool
      ; item_is_file : bool
      ; item_is_dir : bool
      ; item_is_symlink : bool
      ; own_event : bool
      ; item_is_hardlink : bool
      ; item_is_last_hardlink : bool
      ; item_cloned : bool
      }

    let to_dyn
        { must_scan_subdirs
        ; user_dropped
        ; kernel_dropped
        ; event_ids_wrapped
        ; history_done
        ; root_changed
        ; mount
        ; unmount
        ; own_event
        ; item_created
        ; item_removed
        ; item_inode_meta_mod
        ; item_renamed
        ; item_modified
        ; item_finder_info_mod
        ; item_change_owner
        ; item_xattr_mod
        ; item_is_file
        ; item_is_dir
        ; item_is_symlink
        ; item_is_hardlink
        ; item_is_last_hardlink
        ; item_cloned
        } =
      let open Dyn in
      record
        [ ("must_scan_subdirs", bool must_scan_subdirs)
        ; ("user_dropped", bool user_dropped)
        ; ("kernel_dropped", bool kernel_dropped)
        ; ("event_ids_wrapped", bool event_ids_wrapped)
        ; ("history_done", bool history_done)
        ; ("root_changed", bool root_changed)
        ; ("mount", bool mount)
        ; ("unmount", bool unmount)
        ; ("own_event", bool own_event)
        ; ("item_created", bool item_created)
        ; ("item_removed", bool item_removed)
        ; ("item_inode_meta_mod", bool item_inode_meta_mod)
        ; ("item_renamed", bool item_renamed)
        ; ("item_modified", bool item_modified)
        ; ("item_finder_info_mod", bool item_finder_info_mod)
        ; ("item_change_owner", bool item_change_owner)
        ; ("item_xattr_mod", bool item_xattr_mod)
        ; ("item_is_file", bool item_is_file)
        ; ("item_is_dir", bool item_is_dir)
        ; ("item_is_symlink", bool item_is_symlink)
        ; ("item_is_hardlink", bool item_is_hardlink)
        ; ("item_is_last_hardlink", bool item_is_last_hardlink)
        ; ("item_cloned", bool item_cloned)
        ]
  end

  external raw : Int32.t -> Raw.t = "dune_fsevents_raw"

  let to_dyn_raw t =
    let open Dyn in
    record [ ("flags", Raw.to_dyn (raw t.flags)); ("path", string t.path) ]

  let id t = t.id

  let path t = t.path

  type kind =
    | Dir
    | File
    | Dir_and_descendants

  let dyn_of_kind kind =
    Dyn.string
      (match kind with
      | Dir -> "Dir"
      | File -> "File"
      | Dir_and_descendants -> "Dir_and_descendants")

  external kind : Int32.t -> kind = "dune_fsevents_kind"

  let kind t = kind t.flags

  type action =
    | Create
    | Remove
    | Modify
    | Rename
    | Unknown

  external action : Int32.t -> action = "dune_fsevents_action"

  let action t = action t.flags

  let dyn_of_action a =
    Dyn.string
      (match a with
      | Create -> "Create"
      | Remove -> "Remove"
      | Modify -> "Modify"
      | Unknown -> "Unknown"
      | Rename -> "Rename")

  let to_dyn t =
    let open Dyn in
    record
      [ ("action", dyn_of_action (action t))
      ; ("kind", dyn_of_kind (kind t))
      ; ("path", string t.path)
      ]
end

module Raw = struct
  type t

  external stop : t -> unit = "dune_fsevents_stop"

  external start : t -> RunLoop.Raw.t -> unit = "dune_fsevents_start"

  external create : string list -> float -> (Event.t list -> unit) -> t
    = "dune_fsevents_create"

  external set_exclusion_paths : t -> string list -> unit
    = "dune_fsevents_set_exclusion_paths"

  external flush_sync : t -> unit = "dune_fsevents_flush_sync"

  (* external flush_async : t -> Event.Id.t = "dune_fsevents_flush_async" *)
end

type state =
  | Idle of Raw.t
  | Start of Raw.t * RunLoop.t
  | Stop of RunLoop.t

type t = state State.t

let stop t =
  State.critical_section t (fun t ->
      match State.get t with
      | Idle _ -> Code_error.raise "Fsevents.stop: idle" []
      | Stop _ -> ()
      | Start (raw, rl) ->
        State.set t (Stop rl);
        Raw.stop raw)

let start t (rl : RunLoop.t) =
  State.critical_section t (fun t ->
      match State.get t with
      | Stop _ -> Code_error.raise "Fsevents.start: stop" []
      | Start _ -> Code_error.raise "Fsevents.start: start" []
      | Idle r ->
        State.critical_section rl (fun rl' ->
            match State.get rl' with
            | Stopped -> Code_error.raise "Fsevents.start: runloop stopped" []
            | Idle rl' | Running rl' ->
              State.set t (Start (r, rl));
              Raw.start r rl'))

let runloop t =
  State.critical_section t (fun t ->
      match State.get t with
      | Idle _ -> None
      | Start (_, rl) | Stop rl -> Some rl)

let flush_sync t =
  let t =
    State.critical_section t (fun t ->
        match State.get t with
        | Idle _ -> Code_error.raise "Fsevents.flush_sync: idle" []
        | Stop _ -> Code_error.raise "Fsevents.flush_sync: stop" []
        | Start (r, _) -> r)
  in
  Raw.flush_sync t

let create ~paths ~latency ~f =
  (match paths with
  | [] -> Code_error.raise "Fsevents.create: paths empty" []
  | _ -> ());
  State.create (Idle (Raw.create paths latency f))

let set_exclusion_paths t ~paths =
  if List.length paths > 8 then
    Code_error.raise
      "Fsevents.set_exclusion_paths: 8 directories should be enough for anybody"
      [ ("paths", Dyn.(list string) paths) ];
  State.critical_section t (fun t ->
      match State.get t with
      | Stop _ -> Code_error.raise "Fsevents.set_exclusion_paths: stop" []
      | Idle r | Start (r, _) -> (
        try Raw.set_exclusion_paths r paths
        with Failure msg ->
          Code_error.raise msg [ ("paths", Dyn.(list string) paths) ]))

(* let flush_async t = *)
(*   let res = flush_async t in *)
(*   if UInt64.equal res UInt64.zero then *)
(*     `No_events_queued *)
(*   else *)
(*     `Last res *)

let flush_async _ = failwith "temporarily disabled"
