open Import

let needs_dumping = ref false
let mark_dirty () = needs_dumping := true

module Dir_contents = struct
  type t =
    { files : Filename.Array.Set.t
    ; dirs : Filename.Array.Set.t
    ; rest : File_kind.t Filename.Array.Map.t
    }

  let repr =
    let set = Repr.view (Repr.list Filename.repr) ~to_:Filename.Array.Set.to_list in
    let rest =
      Repr.view
        (Repr.list (Repr.pair Filename.repr File_kind.repr))
        ~to_:Filename.Array.Map.to_list
    in
    Repr.record
      "fs-memo-dir-contents"
      [ Repr.field "files" set ~get:(fun t -> t.files)
      ; Repr.field "dirs" set ~get:(fun t -> t.dirs)
      ; Repr.field "rest" rest ~get:(fun t -> t.rest)
      ]
  ;;
end

let digest_repr = Repr.view Repr.string ~to_:Digest.to_string

module Fs_memo = struct
  module Stats = struct
    type t =
      { mtime : Time.t
      ; ctime : Time.t
      ; size : int
      ; perm : Unix.file_perm
      ; dev : int
      ; ino : int
      }

    let create ~mtime ~ctime ~size ~perm ~dev ~ino =
      { mtime; ctime; size; perm; dev; ino }
    ;;

    let mtime t = t.mtime
    let ctime t = t.ctime
    let size t = t.size
    let perm t = t.perm
    let dev t = t.dev
    let ino t = t.ino

    let repr =
      Repr.record
        "fs-memo-cached-digest-reduced-stats"
        [ Repr.field "mtime" Time.repr ~get:mtime
        ; Repr.field "ctime" Time.repr ~get:ctime
        ; Repr.field "size" Repr.int ~get:size
        ; Repr.field "perm" Repr.int ~get:perm
        ; Repr.field "dev" Repr.int ~get:dev
        ; Repr.field "ino" Repr.int ~get:ino
        ]
    ;;
  end

  type 'a file =
    { mutable contents : 'a
    ; mutable stats : Stats.t
    ; mutable stats_checked : int
    }

  type t =
    { mutable checked_key : int
    ; mutable max_timestamp : Time.t
    ; table : Digest.t file Path.Table.t
    ; dir_contents : Dir_contents.t file Path.Table.t
    }

  let file_repr contents_repr =
    Repr.record
      "fs-memo-cached-digest-file"
      [ Repr.field "contents" contents_repr ~get:(fun t -> t.contents)
      ; Repr.field "stats" Stats.repr ~get:(fun t -> t.stats)
      ; Repr.field "stats_checked" Repr.int ~get:(fun t -> t.stats_checked)
      ]
  ;;

  let repr =
    let table contents_repr =
      Repr.abstract (Path.Table.to_dyn (Repr.to_dyn (file_repr contents_repr)))
    in
    Repr.record
      "fs-memo-cached-digest"
      [ Repr.field "checked_key" Repr.int ~get:(fun t -> t.checked_key)
      ; Repr.field "max_timestamp" Time.repr ~get:(fun t -> t.max_timestamp)
      ; Repr.field "table" (table digest_repr) ~get:(fun t -> t.table)
      ; Repr.field "dir_contents" (table Dir_contents.repr) ~get:(fun t -> t.dir_contents)
      ]
  ;;

  let create () =
    { checked_key = 0
    ; table = Path.Table.create ()
    ; max_timestamp = Time.of_ns 0
    ; dir_contents = Path.Table.create ()
    }
  ;;
end

module Rule_cache = struct
  module Entry = struct
    type t =
      { rule_digest : Digest.t
      ; dynamic_deps_stages : (Dep.Set.t * Digest.t) list
      ; targets_digest : Digest.t
      }

    let repr =
      Repr.record
        "rule-cache-workspace-local-entry"
        [ Repr.field "rule_digest" digest_repr ~get:(fun t -> t.rule_digest)
        ; Repr.field
            "dynamic_deps_stages"
            (Repr.list (Repr.pair (Repr.abstract Dep.Set.to_dyn) digest_repr))
            ~get:(fun t -> t.dynamic_deps_stages)
        ; Repr.field "targets_digest" digest_repr ~get:(fun t -> t.targets_digest)
        ]
    ;;
  end

  type digest =
    { digest : Digest.t
    ; siblings : Digest.t Targets.Produced.t
    ; generation : int
    }

  let digest_repr =
    Repr.record
      "rule-cache-workspace-local-digest"
      [ Repr.field "digest" digest_repr ~get:(fun t -> t.digest)
      ; Repr.field "siblings" (Repr.abstract Targets.Produced.to_dyn) ~get:(fun t ->
          t.siblings)
      ; Repr.field "generation" Repr.int ~get:(fun t -> t.generation)
      ]
  ;;

  type t =
    { rules : Entry.t Path.Table.t
    ; digests : digest Path.Build.Table.t
    ; invalidated_subtrees : int Path.Build.Table.t
    ; mutable generation : int
    }

  let repr =
    Repr.record
      "rule-cache-workspace-local-database"
      [ Repr.field
          "rules"
          (Repr.abstract (Path.Table.to_dyn (Repr.to_dyn Entry.repr)))
          ~get:(fun t -> t.rules)
      ; Repr.field
          "digests"
          (Repr.abstract (Path.Build.Table.to_dyn (Repr.to_dyn digest_repr)))
          ~get:(fun t -> t.digests)
      ; Repr.field
          "invalidated_subtrees"
          (Repr.abstract (Path.Build.Table.to_dyn Dyn.int))
          ~get:(fun t -> t.invalidated_subtrees)
      ; Repr.field "generation" Repr.int ~get:(fun t -> t.generation)
      ]
  ;;

  let create () =
    { rules = Path.Table.create ()
    ; digests = Path.Build.Table.create 128
    ; invalidated_subtrees = Path.Build.Table.create 16
    ; generation = 0
    }
  ;;
end

type t =
  { fs_memo : Fs_memo.t
  ; rule_cache : Rule_cache.t
  }

let file = Path.relative Path.build_dir ".db"
let old_digest_file = Path.relative Path.build_dir ".digest-db"

let repr =
  Repr.record
    "workspace-cache"
    [ Repr.field "fs_memo" Fs_memo.repr ~get:(fun t -> t.fs_memo)
    ; Repr.field "rule_cache" Rule_cache.repr ~get:(fun t -> t.rule_cache)
    ]
;;

module P = Persistent.Make (struct
    type nonrec t = t

    let name = "WORKSPACE-CACHE"
    let version = 2
    let sharing = true
    let repr = repr
  end)

let cache =
  lazy
    (match P.load file with
     | None -> { fs_memo = Fs_memo.create (); rule_cache = Rule_cache.create () }, false
     | Some t -> t, true)
;;

let get () = fst (Lazy.force cache)
let fs_memo () = (get ()).fs_memo
let rule_cache () = (get ()).rule_cache
let loaded_from_disk () = snd (Lazy.force cache)

let dump () =
  if !needs_dumping && Path.build_dir_exists ()
  then (
    needs_dumping := false;
    Console.Status_line.with_overlay
      (Live (fun () -> Pp.hbox (Pp.text "Saving workspace cache...")))
      ~f:(fun () ->
        P.dump file (get ());
        Fpath.unlink_no_err (Path.to_string old_digest_file)))
;;

let at_exit = At_exit.at_exit Dune_trace.at_exit dump
let load_fs_memo () = Option.map (P.load file) ~f:(fun t -> t.fs_memo)
