open Import
open Fiber.O

type backend =
  | Path of Path.t
  | Git of Rev_store.At_rev.t

type t = backend

let backend t = t

let of_opam_url loc url =
  let* () = Fiber.return () in
  match OpamUrl.local_or_git_only url loc with
  | `Path dir -> Fiber.return (Path dir)
  | `Git ->
    let+ rev =
      let* rev_store = Rev_store.get in
      OpamUrl.resolve url ~loc rev_store
      >>= (function
             | Error _ as e -> Fiber.return e
             | Ok s -> OpamUrl.fetch_revision url ~loc s rev_store)
      >>| User_error.ok_exn
    in
    Git rev
;;

let read t file =
  match t with
  | Git rev -> Rev_store.At_rev.content rev file
  | Path dir ->
    let+ () = Fiber.return () in
    let file = Path.append_local dir file in
    (match Io.read_file ~binary:true file with
     | s -> Some s
     | exception Unix.Unix_error (ENOENT, _, _) -> None)
;;

let stat t path =
  let+ () = Fiber.return () in
  match t with
  | Path dir ->
    let path = Path.append_local dir path in
    (match (Path.stat_exn path).st_kind with
     | S_REG -> `File
     | S_DIR -> `Dir
     | _ -> `Absent_or_unrecognized
     | exception Unix.Unix_error (ENOENT, _, _) -> `Absent_or_unrecognized)
  | Git rev ->
    (match
       Rev_store.File.Set.is_empty
         (Rev_store.At_rev.directory_entries rev ~recursive:false path)
     with
     | false -> `Dir
     | true ->
       (match Path.Local.parent path with
        | None -> `Absent_or_unrecognized
        | Some parent ->
          let files = Rev_store.At_rev.directory_entries ~recursive:false rev parent in
          let basename = Path.Local.basename path in
          if Rev_store.File.Set.exists files ~f:(fun file ->
               let path = Rev_store.File.path file in
               String.equal basename (Path.Local.basename path))
          then `File
          else `Absent_or_unrecognized))
;;

let readdir t dir =
  let+ () = Fiber.return () in
  match t with
  | Git rev ->
    Rev_store.At_rev.directory_entries ~recursive:false rev dir
    |> Rev_store.File.Set.to_list_map ~f:(fun file ->
      Rev_store.File.path file |> Path.Local.basename)
    |> Filename.Map.of_list_map_exn ~f:(fun fname -> fname, `File)
  | Path p ->
    let dir = Path.append_local p dir in
    (match Path.readdir_unsorted_with_kinds dir with
     | Error e -> Unix_error.Detailed.raise e
     | Ok listing ->
       List.filter_map listing ~f:(fun (name, kind) ->
         match
           match kind with
           | S_REG -> Some `File
           | S_DIR -> Some `Dir
           | S_LNK ->
             (match (Path.stat_exn (Path.relative dir name)).st_kind with
              | S_REG -> Some `File
              | S_DIR -> Some `Dir
              | _ -> None)
           | _ -> None
         with
         | None -> None
         | Some kind -> Some (name, kind))
       |> Filename.Map.of_list_exn)
;;
