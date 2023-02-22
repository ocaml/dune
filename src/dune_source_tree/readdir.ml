open Stdune
open Memo.O

type t =
  { path : Path.Source.t
  ; files : Filename.Set.t
  ; dirs : (Filename.t * Path.Source.t * File.t) list
  }

module type S = sig
  type reduced_stats

  val stat :
       Path.Outside_build_dir.t
    -> (reduced_stats, Unix_error.Detailed.t) result Memo.t

  val readdir : Path.Source.t -> (t, Unix_error.Detailed.t) result Memo.t
end

let equal =
  let dirs_equal (s1, p1, f1) (s2, p2, f2) =
    Filename.equal s1 s2 && Path.Source.equal p1 p2 && File.compare f1 f2 = Eq
  in
  fun x y ->
    Path.Source.equal x.path y.path
    && Filename.Set.equal x.files y.files
    && List.equal dirs_equal x.dirs y.dirs

let empty path = { path; files = Filename.Set.empty; dirs = [] }

let _to_dyn { path; files; dirs } =
  let open Dyn in
  record
    [ ("path", Path.Source.to_dyn path)
    ; ("files", Filename.Set.to_dyn files)
    ; ("dirs", list (triple string Path.Source.to_dyn File.to_dyn) dirs)
    ]

(* Returns [true] for special files such as character devices of sockets; see
   #3124 for more on issues caused by special devices *)
let is_special (st_kind : Unix.file_kind) =
  match st_kind with
  | S_CHR | S_BLK | S_FIFO | S_SOCK -> true
  | _ -> false

module Make
    (Reduced_stats : Reduced_stats_intf.S) (Dir_contents : sig
      type t

      (** The sorted list of file names with kinds. *)
      val to_list : t -> (string * File_kind.t) list
    end) (Fs_memo : sig
      val path_stat :
           Path.Outside_build_dir.t
        -> (Reduced_stats.t, Unix_error.Detailed.t) result Memo.t

      val dir_contents :
           Path.Outside_build_dir.t
        -> (Dir_contents.t, Unix_error.Detailed.t) result Memo.t
    end) =
struct
  module Reduced_stats_of_file = File.Make (Reduced_stats)

  let stat dir =
    let+ res = Fs_memo.path_stat dir in
    Result.map res ~f:Reduced_stats_of_file.of_stats

  let of_source_path_impl path =
    Fs_memo.dir_contents (In_source_dir path) >>= function
    | Error unix_error -> Memo.return @@ Error unix_error
    | Ok dir_contents ->
      let dir_contents = Dir_contents.to_list dir_contents in
      let+ files, dirs =
        Memo.parallel_map dir_contents ~f:(fun (fn, kind) ->
            let path = Path.Source.relative path fn in
            if Path.Source.is_in_build_dir path then Memo.return List.Skip
            else
              let+ is_directory, file =
                match kind with
                | S_DIR -> (
                  stat (In_source_dir path) >>| function
                  | Ok file -> (true, file)
                  | Error _ -> (true, File.dummy))
                | S_LNK -> (
                  Fs_memo.path_stat (In_source_dir path) >>| function
                  | Ok ({ st_kind = S_DIR; _ } as st) ->
                    (true, Reduced_stats_of_file.of_stats st)
                  | Ok _ | Error _ -> (false, File.dummy))
                | _ -> Memo.return (false, File.dummy)
              in
              if is_directory then List.Right (fn, path, file)
              else if is_special kind then Skip
              else Left fn)
        >>| List.filter_partition_map ~f:Fun.id
      in
      { path; files = Filename.Set.of_list files; dirs } |> Result.ok

  (* Having a cutoff here speeds up incremental rebuilds quite a bit when a
     directory contents is invalidated but the result stays the same. *)
  let of_source_path_memo =
    Memo.create "readdir-of-source-path"
      ~input:(module Path.Source)
      ~cutoff:(Result.equal equal Unix_error.Detailed.equal)
      of_source_path_impl

  let readdir = Memo.exec of_source_path_memo

  let stat = Fs_memo.path_stat
end
