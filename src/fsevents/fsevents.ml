open Stdune

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
      let open Dyn.Encoder in
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
    let open Dyn.Encoder in
    record [ ("flags", Raw.to_dyn (raw t.flags)); ("path", string t.path) ]

  let id t = t.id

  let path t = t.path

  type kind =
    | Dir
    | File
    | Dir_and_descendants

  let dyn_of_kind kind =
    Dyn.Encoder.string
      (match kind with
      | Dir -> "Dir"
      | File -> "File"
      | Dir_and_descendants -> "Dir_and_descendants")

  external kind : Int32.t -> kind = "dune_fsevents_kind"

  let kind t = kind t.flags

  type action =
    | Unknown
    | Create
    | Remove
    | Modify

  external action : Int32.t -> action = "dune_fsevents_action"

  let action t = action t.flags

  let dyn_of_action a =
    Dyn.Encoder.string
      (match a with
      | Create -> "Create"
      | Remove -> "Remove"
      | Modify -> "Modify"
      | Unknown -> "Unknown")

  let to_dyn t =
    let open Dyn.Encoder in
    record
      [ ("action", dyn_of_action (action t))
      ; ("kind", dyn_of_kind (kind t))
      ; ("path", string t.path)
      ]
end

type t

external available : unit -> bool = "dune_fsevents_available"

external stop : t -> unit = "dune_fsevents_stop"

external start : t -> unit = "dune_fsevents_start"

external loop : t -> unit = "dune_fsevents_loop"

let loop t =
  match loop t with
  | exception exn -> Error exn
  | () -> Ok ()

external break : t -> unit = "dune_fsevents_break"

external flush_sync : t -> unit = "dune_fsevents_flush_sync"

external destroy : t -> unit = "dune_fsevents_destroy"

external dune_fsevents_create :
  string list -> float -> (t -> Event.t list -> unit) -> t
  = "dune_fsevents_create"

let create ~paths ~latency ~f =
  (match paths with
  | [] -> Code_error.raise "Fsevents.create: paths empty" []
  | _ -> ());
  dune_fsevents_create paths latency f

(* external flush_async : t -> Event.Id.t = "dune_fsevents_flush_async" *)

external set_exclusion_paths : t -> string list -> unit
  = "dune_fsevents_set_exclusion_paths"

let set_exclusion_paths t ~paths =
  if List.length paths > 8 then
    Code_error.raise
      "Fsevents.set_exclusion_paths: 8 directories should be enough for anybody"
      [ ("paths", Dyn.Encoder.(list string) paths) ];
  set_exclusion_paths t paths

(* let flush_async t = *)
(*   let res = flush_async t in *)
(*   if UInt64.equal res UInt64.zero then *)
(*     `No_events_queued *)
(*   else *)
(*     `Last res *)

let flush_async _ = failwith "temporarily disabled"
