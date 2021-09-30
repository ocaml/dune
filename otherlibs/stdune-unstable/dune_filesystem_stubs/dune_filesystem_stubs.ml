module File_kind = struct
  type t = Unix.file_kind =
    | S_REG
    | S_DIR
    | S_CHR
    | S_BLK
    | S_LNK
    | S_FIFO
    | S_SOCK

  module Option = struct
    [@@@warning "-37"]

    (* The values are constructed on the C-side *)
    type t =
      | S_REG
      | S_DIR
      | S_CHR
      | S_BLK
      | S_LNK
      | S_FIFO
      | S_SOCK
      | UNKNOWN

    let elim ~none ~some t =
      match t with
      | S_REG -> some (S_REG : Unix.file_kind)
      | S_DIR -> some S_DIR
      | S_CHR -> some S_CHR
      | S_BLK -> some S_BLK
      | S_LNK -> some S_LNK
      | S_FIFO -> some S_FIFO
      | S_SOCK -> some S_SOCK
      | UNKNOWN -> none ()
  end
end

let catch_unix_error f x =
  match f x with
  | exception Unix.Unix_error (error, _, _) -> Error error
  | res -> Ok res

external scandir_with_kind_if_available_unix :
  string -> (string * File_kind.Option.t) list = "dune_scandir"

let scandir_with_kind_if_available_win32 dir =
  (* Windows also gives us the information about file kind and it's discarded by
     [readdir]. We could do better here, but the Windows code is more
     complicated. (there's an additional OCaml abstraction layer) *)
  match catch_unix_error Unix.opendir dir with
  | Error e -> Error e
  | Ok dir ->
    Fun.protect
      ~finally:(fun () -> Unix.closedir dir)
      (fun () ->
        let rec loop acc =
          match Unix.readdir dir with
          | "."
          | ".." ->
            loop acc
          | exception End_of_file -> List.rev acc
          | entry -> loop ((entry, File_kind.Option.UNKNOWN) :: acc)
        in
        Ok (loop []))

let scandir_with_kind_if_available :
    string -> ((string * File_kind.Option.t) list, Unix.error) result =
  if Stdlib.Sys.win32 then
    scandir_with_kind_if_available_win32
  else
    fun dir ->
  catch_unix_error scandir_with_kind_if_available_unix dir

let read_directory_with_kinds dir_path =
  match scandir_with_kind_if_available dir_path with
  | Error e -> Error e
  | Ok entries ->
    Ok
      (entries
      |> List.filter_map (fun (base, kind) ->
             File_kind.Option.elim kind
               ~none:(fun () ->
                 match Unix.lstat (Filename.concat dir_path base) with
                 | exception _ ->
                   (* File disappeared between readdir & lstat system calls.
                      Handle as if readdir never told us about it *)
                   None
                 | stat -> Some (base, stat.st_kind))
               ~some:(fun kind -> Some (base, kind))))

(* XXX this function is subtly different from [read_directory_with_kinds]. it
   does not skip [UNKNOWN] entries for which lstat failed. Is this
   intentional? *)
let read_directory dir_path =
  match scandir_with_kind_if_available dir_path with
  | Error e -> Error e
  | Ok s -> Ok (List.map fst s)
