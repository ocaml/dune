open Stdune

module T = struct
  type t = Blake3_mini.Digest.t

  let to_string = Blake3_mini.Digest.to_hex
  let to_dyn s = Dyn.variant "digest" [ String (to_string s) ]
  let compare x y = Ordering.of_int (Blake3_mini.Digest.compare x y)
end

include T
module C = Comparable.Make (T)
module Set = C.Set
module Map = C.Map
module Metrics = Dune_metrics

let file file =
  (* On Windows, if this function is invoked in a background thread,
       if can happen that the file is not properly closed.
       [O_SHARE_DELETE] ensures that the main thread can delete it even if it
       is still open. See #8243. *)
  let fd =
    match Unix.openfile file [ Unix.O_RDONLY; O_SHARE_DELETE; O_CLOEXEC ] 0 with
    | fd -> fd
    | exception Unix.Unix_error (Unix.EACCES, _, _) ->
      raise (Sys_error (sprintf "%s: Permission denied" file))
    | exception exn -> reraise exn
  in
  Exn.protectx fd ~f:Blake3_mini.fd ~finally:Unix.close
;;

let equal = Blake3_mini.Digest.equal
let hash = Poly.hash
let file p = file (Path.to_string p)
let from_hex s = Blake3_mini.Digest.of_hex s

module Hasher = struct
  type t = Blake3_mini.t

  let with_singleton =
    let singleton = lazy (Blake3_mini.create ()) in
    let in_use = ref false in
    fun f ->
      if !in_use
      then
        Code_error.raise
          "[Hasher.with_singleton] called within argument function to \
           [Hasher.with_singleton], which is not allowed."
          []
      else (
        in_use := true;
        let hasher = Lazy.force singleton in
        f hasher;
        let digest = Blake3_mini.digest hasher in
        Blake3_mini.reset hasher;
        in_use := false;
        digest)
  ;;
end

module Feed = struct
  type hasher = Hasher.t
  type 'a t = hasher -> 'a -> unit

  let contramap a ~f hasher b = a hasher (f b)
  let string hasher s = Blake3_mini.feed_string hasher s ~pos:0 ~len:(String.length s)
  let bool = contramap string ~f:Bool.to_string
  let int = contramap string ~f:Int.to_string

  (* We use [No_sharing] to avoid generating different digests for inputs that
       differ only in how they share internal values. Without [No_sharing], if a
       command line contains duplicate flags, such as multiple occurrences of the
       flag [-I], then [Marshal.to_string] will produce different digests depending
       on whether the corresponding strings ["-I"] point to the same memory location
       or to different memory locations. *)
  let generic hasher x =
    contramap string ~f:(fun x -> Marshal.to_string x [ No_sharing ]) hasher x
  ;;

  let list feed_x hasher xs = List.iter xs ~f:(feed_x hasher)
  let option feed_x hasher option_x = Option.iter option_x ~f:(feed_x hasher)

  let tuple2 feed_a feed_b hasher (a, b) =
    feed_a hasher a;
    feed_b hasher b
  ;;

  let tuple3 feed_a feed_b feed_c hasher (a, b, c) =
    feed_a hasher a;
    feed_b hasher b;
    feed_c hasher c
  ;;

  let digest hasher digest = contramap string ~f:to_string hasher digest
  let compute_digest t x = Hasher.with_singleton (fun hasher -> t hasher x)
end

let string s = Feed.compute_digest Feed.string s
let to_string_raw s = Blake3_mini.Digest.to_binary s

let generic a =
  Metrics.Timer.record "generic_digest" ~f:(fun () -> Feed.compute_digest Feed.generic a)
;;

let path_with_executable_bit =
  (* We follow the digest scheme used by Jenga. *)
  let string_and_bool ~digest_hex ~bool =
    string (Blake3_mini.Digest.to_hex digest_hex ^ if bool then "\001" else "\000")
  in
  fun ~executable ~content_digest ->
    string_and_bool ~digest_hex:content_digest ~bool:executable
;;

let file_with_executable_bit ~executable path =
  let content_digest = file path in
  path_with_executable_bit ~content_digest ~executable
;;

module Stats_for_digest = struct
  type t =
    { st_kind : Unix.file_kind
    ; executable : bool
    }

  let of_unix_stats (stats : Unix.stats) =
    (* Check if any of the +x bits are set, ignore read and write *)
    let executable = 0o111 land stats.st_perm <> 0 in
    { st_kind = stats.st_kind; executable }
  ;;
end

module Path_digest_error = struct
  type nonrec t =
    | Unexpected_kind
    | Unix_error of Unix_error.Detailed.t
end

exception E of Path_digest_error.t

let directory_digest_version = 3

let path_with_stats ~allow_dirs path (stats : Stats_for_digest.t) =
  let rec loop path (stats : Stats_for_digest.t) =
    match stats.st_kind with
    | S_LNK ->
      Unix_error.Detailed.catch
        (fun path ->
           let contents = Path.to_string path |> Unix.readlink |> string in
           path_with_executable_bit ~executable:stats.executable ~content_digest:contents)
        path
      |> Result.map_error ~f:(fun x -> Path_digest_error.Unix_error x)
    | S_REG ->
      Unix_error.Detailed.catch
        (file_with_executable_bit ~executable:stats.executable)
        path
      |> Result.map_error ~f:(fun x -> Path_digest_error.Unix_error x)
    | S_DIR when allow_dirs ->
      (* CR-someday amokhov: The current digesting scheme has collisions for files
         and directories. It's unclear if this is actually a problem. If it turns
         out to be a problem, we should include [st_kind] into both digests. *)
      (match Path.readdir_unsorted path with
       | Error e -> Error (Path_digest_error.Unix_error e)
       | Ok listing ->
         (match
            List.rev_map listing ~f:(fun name ->
              let path = Path.relative path name in
              let stats =
                match Path.lstat path with
                | Error e -> raise_notrace (E (Unix_error e))
                | Ok stat -> Stats_for_digest.of_unix_stats stat
              in
              let digest =
                match loop path stats with
                | Ok s -> s
                | Error e -> raise_notrace (E e)
              in
              name, digest)
            |> List.sort ~compare:(fun (x, _) (y, _) -> String.compare x y)
          with
          | exception E e -> Error e
          | contents ->
            Ok (generic (directory_digest_version, contents, stats.executable))))
    | S_DIR | S_BLK | S_CHR | S_FIFO | S_SOCK -> Error Unexpected_kind
  in
  match stats.st_kind with
  | S_DIR when not allow_dirs -> Error Path_digest_error.Unexpected_kind
  | S_BLK | S_CHR | S_LNK | S_FIFO | S_SOCK -> Error Unexpected_kind
  | _ -> loop path stats
;;
