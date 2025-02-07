open Stdune
open Memo.O

open struct
  open Dune_engine
  module Fs_memo = Fs_memo
  module Fs_cache = Fs_cache
  module Build_system = Build_system
end

let dir_contents (dir : Path.t) =
  let* () = Memo.return () in
  match Path.destruct_build_dir dir with
  | `Outside dir ->
    Fs_memo.dir_contents dir
    >>| Result.map ~f:(fun contents ->
      Fs_cache.Dir_contents.to_list contents |> List.map ~f:fst)
  | `Inside _ ->
    let* () = Build_system.build_dir dir in
    Memo.return (Path.readdir_unsorted dir)
;;

let exists path kind =
  Build_system.file_exists path
  >>= function
  | false -> Memo.return false
  | true ->
    let+ () = Build_system.build_file path in
    (match Path.stat path with
     | Ok { st_kind; _ } -> kind = st_kind
     | _ -> false)
;;

let file_exists file =
  let* () = Memo.return () in
  match Path.destruct_build_dir file with
  | `Outside file -> Fs_memo.file_exists file
  | `Inside _ -> exists file Unix.S_REG
;;

let dir_exists dir =
  let* () = Memo.return () in
  match Path.destruct_build_dir dir with
  | `Outside dir -> Fs_memo.dir_exists dir
  | `Inside _ -> exists dir Unix.S_DIR
;;

let with_lexbuf_from_file file ~f =
  let* () = Memo.return () in
  match Path.destruct_build_dir file with
  | `Outside file -> Fs_memo.with_lexbuf_from_file ~f file
  | `Inside _ ->
    let* () = Build_system.build_file file in
    Memo.return @@ Io.with_lexbuf_from_file file ~f
;;
