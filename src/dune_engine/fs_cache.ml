open Import

(* CR-someday amokhov: Persistently store the caches of (some?) operations. *)

(* CR-someday amokhov: Implement garbage collection. *)

type 'a t =
  { name : string (* For debugging *)
  ; sample : Path.Outside_build_dir.t -> 'a
  ; cache : 'a Path.Outside_build_dir.Table.t
  ; equal : 'a -> 'a -> bool (* Used to implement cutoff *)
  }

let create name ~sample ~equal : 'a t =
  { name; sample; equal; cache = Path.Outside_build_dir.Table.create 128 }

let read { sample; cache; _ } path =
  match Path.Outside_build_dir.Table.find cache path with
  | Some cached_result -> cached_result
  | None ->
    let result = sample path in
    Path.Outside_build_dir.Table.add_exn cache path result;
    result

let evict { cache; _ } path = Path.Outside_build_dir.Table.remove cache path

let update { sample; cache; equal; name } path =
  let result =
    match Path.Outside_build_dir.Table.find cache path with
    | None -> Fs_cache_update_result.Skipped
    | Some old_result -> (
      let new_result = sample path in
      match equal old_result new_result with
      | true -> Updated { changed = false }
      | false ->
        Path.Outside_build_dir.Table.set cache path new_result;
        Updated { changed = true })
  in
  Fs_cache_update_result.log_update result ~name path;
  result

module Reduced_stats = struct
  type t =
    { st_dev : int
    ; st_ino : int
    ; st_kind : Unix.file_kind
    }

  let of_unix_stats { Unix.st_dev; st_ino; st_kind; _ } =
    { st_dev; st_ino; st_kind }

  let equal x y =
    Int.equal x.st_dev y.st_dev
    && Int.equal x.st_ino y.st_ino
    && File_kind.equal x.st_kind y.st_kind
end

module Dir_contents : sig
  type t

  val of_list : (Filename.t * File_kind.t) list -> t

  val to_list : t -> (Filename.t * File_kind.t) list

  val iter : t -> f:(Filename.t * File_kind.t -> unit) -> unit

  val equal : t -> t -> bool
end = struct
  (* CR-someday amokhov: Using a [Filename.Map] instead of a list would be better
     since we'll not need to worry about the invariant that the list is sorted
     and doesn't contain any duplicate file names. Using maps will likely be
     more costly, so we need to do some benchmarking before switching. *)
  type t = (Filename.t * File_kind.t) list

  let to_list t = t

  let iter t = List.iter t

  (* The names must be unique, so we don't care about comparing file kinds. *)
  let of_list = List.sort ~compare:(fun (x, _) (y, _) -> Filename.compare x y)

  let equal = List.equal (Tuple.T2.equal Filename.equal File_kind.equal)
end

module Untracked = struct
  let path_stat =
    let sample path =
      Path.outside_build_dir path
      |> Path.Untracked.stat
      |> Result.map ~f:Reduced_stats.of_unix_stats
    in
    create "path_stat" ~sample
      ~equal:(Result.equal Reduced_stats.equal Unix_error.Detailed.equal)

  let dir_contents =
    create "dir_contents"
      ~sample:(fun path ->
        Path.Untracked.readdir_unsorted_with_kinds (Path.outside_build_dir path)
        |> Result.map ~f:Dir_contents.of_list)
      ~equal:(Result.equal Dir_contents.equal Unix_error.Detailed.equal)
end

module Debug = struct
  let name t = t.name
end
