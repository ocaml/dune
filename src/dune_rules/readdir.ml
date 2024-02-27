open Import
open Memo.O

module File = struct
  module T = struct
    type t =
      { ino : int
      ; dev : int
      }

    let to_dyn { ino; dev } =
      let open Dyn in
      record [ "ino", Int.to_dyn ino; "dev", Int.to_dyn dev ]
    ;;

    let compare { ino; dev } t =
      match Int.compare ino t.ino with
      | Ordering.Eq -> Int.compare dev t.dev
      | _ as e -> e
    ;;
  end

  include T

  let dummy = { ino = 0; dev = 0 }
  let of_stats (st : Fs_cache.Reduced_stats.t) = { ino = st.st_ino; dev = st.st_dev }

  module Map = Map.Make (T)

  let of_source_path p = Fs_memo.path_stat p >>| Result.map ~f:of_stats
end

type t =
  { files : Filename.Set.t
  ; dirs : (Filename.t * File.t) list
  }

let files t = t.files
let dirs t = t.dirs

let equal =
  let dirs_equal (s1, f1) (s2, f2) = Filename.equal s1 s2 && File.compare f1 f2 = Eq in
  fun x y -> Filename.Set.equal x.files y.files && List.equal dirs_equal x.dirs y.dirs
;;

let empty = { files = Filename.Set.empty; dirs = [] }

let to_dyn { files; dirs } =
  let open Dyn in
  record
    [ "files", Filename.Set.to_dyn files; "dirs", list (pair string File.to_dyn) dirs ]
;;

(* Returns [true] for special files such as character devices of sockets; see
   #3124 for more on issues caused by special devices *)
let is_special (st_kind : Unix.file_kind) =
  match st_kind with
  | S_CHR | S_BLK | S_FIFO | S_SOCK -> true
  | _ -> false
;;

let is_temp_file fn =
  String.is_prefix fn ~prefix:".#"
  || String.is_suffix fn ~suffix:".swp"
  || String.is_suffix fn ~suffix:"~"
;;

let of_source_path_impl path =
  Fs_memo.dir_contents (In_source_dir path)
  >>= function
  | Error unix_error ->
    User_warning.emit
      [ Pp.textf
          "Unable to read directory %s. Ignoring."
          (Path.Source.to_string_maybe_quoted path)
      ; Pp.text "Remove this message by ignoring by adding:"
      ; Pp.textf "(dirs \\ %s)" (Path.Source.basename path)
      ; Pp.textf
          "to the dune file: %s"
          (Path.Source.to_string_maybe_quoted
             (Path.Source.relative (Path.Source.parent_exn path) "dune"))
      ; Unix_error.Detailed.pp ~prefix:"Reason: " unix_error
      ];
    Memo.return (Error unix_error)
  | Ok dir_contents ->
    let+ files, dirs =
      Fs_cache.Dir_contents.to_list dir_contents
      |> Memo.parallel_map ~f:(fun (fn, (kind : File_kind.t)) ->
        let path = Path.Source.relative path fn in
        if is_special kind || Path.Source.is_in_build_dir path || is_temp_file fn
        then Memo.return List.Skip
        else
          let+ is_directory, file =
            match kind with
            | S_DIR ->
              let+ file =
                File.of_source_path (In_source_dir path)
                >>| function
                | Ok file -> file
                | Error _ -> File.dummy
              in
              true, file
            | S_LNK ->
              Fs_memo.path_stat (In_source_dir path)
              >>| (function
               | Ok ({ st_kind = S_DIR; _ } as st) -> true, File.of_stats st
               | Ok _ | Error _ -> false, File.dummy)
            | _ -> Memo.return (false, File.dummy)
          in
          if is_directory then List.Right (fn, file) else Left fn)
      >>| List.filter_partition_map ~f:Fun.id
    in
    { files = Filename.Set.of_list files; dirs } |> Result.ok
;;

(* Having a cutoff here speeds up incremental rebuilds quite a bit when a
   directory contents is invalidated but the result stays the same. *)
let of_source_path_memo =
  Memo.create
    "readdir-of-source-path"
    ~input:(module Path.Source)
    ~cutoff:(Result.equal equal Unix_error.Detailed.equal)
    of_source_path_impl
;;

let of_source_path = Memo.exec of_source_path_memo
