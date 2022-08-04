open Stdune

type t = string

module D = Stdlib.Digest
module Set = String.Set
module Map = String.Map
module Metrics = Dune_metrics

module type Digest_impl = sig
  val file : string -> t

  val string : string -> t
end

module Direct_impl : Digest_impl = struct
  let file = D.file

  let string = D.string
end

module Mutable_impl = struct
  let file_ref = ref D.file

  let string_ref = ref D.string

  let file f = !file_ref f

  let string s = !string_ref s
end

let override_impl ~file ~string =
  Mutable_impl.file_ref := file;
  Mutable_impl.string_ref := string

module Impl : Digest_impl = Mutable_impl

let hash = Poly.hash

let equal = String.equal

let file p = Impl.file (Path.to_string p)

let compare x y = Ordering.of_int (D.compare x y)

let to_string = D.to_hex

let to_dyn s = Dyn.variant "digest" [ String (to_string s) ]

let from_hex s =
  match D.from_hex s with
  | s -> Some s
  | exception Invalid_argument _ -> None

let string = Impl.string

let to_string_raw s = s

(* We use [No_sharing] to avoid generating different digests for inputs that
   differ only in how they share internal values. Without [No_sharing], if a
   command line contains duplicate flags, such as multiple occurrences of the
   flag [-I], then [Marshal.to_string] will produce different digests depending
   on whether the corresponding strings ["-I"] point to the same memory location
   or to different memory locations. *)
let generic a =
  Metrics.Timer.record "generic_digest" ~f:(fun () ->
      string (Marshal.to_string a [ No_sharing ]))

let file_with_executable_bit ~executable path =
  (* We follow the digest scheme used by Jenga. *)
  let string_and_bool ~digest_hex ~bool =
    Impl.string (digest_hex ^ if bool then "\001" else "\000")
  in
  let content_digest = file path in
  string_and_bool ~digest_hex:content_digest ~bool:executable

module Stats_for_digest = struct
  type t =
    { st_kind : Unix.file_kind
    ; st_perm : Unix.file_perm
    }

  let of_unix_stats (stats : Unix.stats) =
    { st_kind = stats.st_kind; st_perm = stats.st_perm }
end

module Path_digest_result = struct
  type nonrec t =
    | Ok of t
    | Unexpected_kind
    | Unix_error of Dune_filesystem_stubs.Unix_error.Detailed.t

  let of_result = function
    | Result.Ok t -> Ok t
    | Error unix_error -> Unix_error unix_error

  let equal x y =
    match (x, y) with
    | Ok x, Ok y -> D.equal x y
    | Ok _, _ | _, Ok _ -> false
    | Unexpected_kind, Unexpected_kind -> true
    | Unexpected_kind, _ | _, Unexpected_kind -> false
    | Unix_error x, Unix_error y ->
      Dune_filesystem_stubs.Unix_error.Detailed.equal x y
end

exception
  E of
    [ `Unix_error of Dune_filesystem_stubs.Unix_error.Detailed.t
    | `Unexpected_kind
    ]

let directory_digest_version = 2

let rec path_with_stats ~allow_dirs path (stats : Stats_for_digest.t) :
    Path_digest_result.t =
  match stats.st_kind with
  | S_REG ->
    let executable =
      Path.Permissions.test Path.Permissions.execute stats.st_perm
    in
    Dune_filesystem_stubs.Unix_error.Detailed.catch
      (file_with_executable_bit ~executable)
      path
    |> Path_digest_result.of_result
  | S_DIR when allow_dirs -> (
    (* CR-someday amokhov: The current digesting scheme has collisions for files
       and directories. It's unclear if this is actually a problem. If it turns
       out to be a problem, we should include [st_kind] into both digests. *)
    match Path.readdir_unsorted path with
    | Error e -> Path_digest_result.Unix_error e
    | Ok listing -> (
      match
        List.rev_map listing ~f:(fun name ->
            let path = Path.relative path name in
            let stats =
              match Path.lstat path with
              | Error e -> raise_notrace (E (`Unix_error e))
              | Ok stat -> Stats_for_digest.of_unix_stats stat
            in
            let digest =
              match path_with_stats ~allow_dirs path stats with
              | Ok s -> s
              | Unix_error e -> raise_notrace (E (`Unix_error e))
              | Unexpected_kind -> raise_notrace (E `Unexpected_kind)
            in
            (name, digest))
        |> List.sort ~compare:(fun (x, _) (y, _) -> String.compare x y)
      with
      | exception E (`Unix_error e) -> Path_digest_result.Unix_error e
      | exception E `Unexpected_kind -> Path_digest_result.Unexpected_kind
      | contents ->
        Ok (generic (directory_digest_version, contents, stats.st_perm))))
  | S_DIR | S_BLK | S_CHR | S_LNK | S_FIFO | S_SOCK -> Unexpected_kind
