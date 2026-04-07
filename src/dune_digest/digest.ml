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

module Hasher = struct
  type t = Blake3_mini.t

  let singleton = lazy (Blake3_mini.create ())

  let with_singleton =
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

  let with_pooled =
    let pool = ref [] in
    let mutex = Mutex.create () in
    let take () =
      Mutex.lock mutex;
      let hasher =
        match !pool with
        | hasher :: rest ->
          pool := rest;
          hasher
        | [] -> Blake3_mini.create ()
      in
      Mutex.unlock mutex;
      hasher
    in
    let release hasher =
      Mutex.lock mutex;
      pool := hasher :: !pool;
      Mutex.unlock mutex
    in
    fun f ->
      let hasher = take () in
      Exn.protectx
        hasher
        ~f:(fun hasher ->
          f hasher;
          Blake3_mini.digest hasher)
        ~finally:(fun hasher ->
          Blake3_mini.reset hasher;
          release hasher)
  ;;
end

let open_for_digest file =
  (* On Windows, if this function is invoked in a background thread,
     if can happen that the file is not properly closed.
     [O_SHARE_DELETE] ensures that the main thread can delete it even if it
     is still open. See #8243. *)
  Unix.openfile file [ Unix.O_RDONLY; O_SHARE_DELETE; O_CLOEXEC ] 0
;;

(* CR-someday rgrinberg: maybe this should exist in blake3_mini? *)
let zero = lazy (Hasher.with_singleton (fun _f -> ()))

let digest_and_close_fd fd =
  let start = Counter.Timer.start () in
  let res = Exn.protectx fd ~f:Blake3_mini.fd ~finally:Unix.close in
  Counter.Timer.stop Metrics.Digest.File.time start;
  res
;;

let file file =
  Counter.incr Metrics.Digest.File.count;
  let fd =
    match open_for_digest file with
    | fd -> fd
    | exception exn -> reraise exn
  in
  digest_and_close_fd fd
;;

let async_digest_minimum = 1_000

let file_async file =
  let open Fiber.O in
  let* () = Fiber.return () in
  let fd = open_for_digest file in
  Counter.incr Metrics.Digest.File.count;
  let size =
    match Unix.fstat fd with
    | exception exn ->
      Unix.close fd;
      raise exn
    | stat -> stat.st_size
  in
  Counter.add Metrics.Digest.File.bytes size;
  if size = 0
  then
    let+ () = Fiber.return @@ Unix.close fd in
    Lazy.force zero
  else if size < async_digest_minimum
  then Fiber.return (digest_and_close_fd fd)
  else Dune_scheduler.Scheduler.async_exn (fun () -> digest_and_close_fd fd)
;;

let equal = Blake3_mini.Digest.equal
let hash = Poly.hash
let file p = file (Path.to_string p)
let file_async p = file_async (Path.to_string p)
let from_hex s = Blake3_mini.Digest.of_hex s

let feed_string_raw hasher s =
  Counter.add Metrics.Digest.Value.bytes (String.length s);
  Blake3_mini.feed_string hasher s ~pos:0 ~len:(String.length s)
;;

let feed_bytes_raw hasher bytes ~len =
  Counter.add Metrics.Digest.Value.bytes len;
  Blake3_mini.feed_string hasher (Bytes.unsafe_to_string bytes) ~pos:0 ~len
;;

let feed_int64 hasher scratch i =
  for byte = 0 to 7 do
    let shift = 8 * byte in
    let value = Int64.(to_int (logand (shift_right_logical i shift) 0xffL)) in
    Bytes.set scratch byte (Char.chr value)
  done;
  feed_bytes_raw hasher scratch ~len:8
;;

let feed_bool hasher scratch b =
  Bytes.set scratch 0 (if b then '\001' else '\000');
  feed_bytes_raw hasher scratch ~len:1
;;

let feed_int hasher scratch i = feed_int64 hasher scratch (Int64.of_int i)

let feed_string hasher scratch s =
  feed_int hasher scratch (String.length s);
  feed_string_raw hasher s
;;

let feed_repr hasher =
  let scratch = Bytes.create 8 in
  let rec loop : type a. a Repr.t -> a -> unit =
    fun repr value ->
    match repr with
    | Unit ->
      feed_int hasher scratch 1;
      feed_bool hasher scratch false
    | Bool ->
      feed_int hasher scratch 2;
      feed_bool hasher scratch value
    | Int ->
      feed_int hasher scratch 3;
      feed_int hasher scratch value
    | String ->
      feed_int hasher scratch 4;
      feed_string hasher scratch value
    | Option repr ->
      feed_int hasher scratch 5;
      (match value with
       | None -> feed_bool hasher scratch false
       | Some x ->
         feed_bool hasher scratch true;
         loop repr x)
    | List repr ->
      feed_int hasher scratch 6;
      feed_int hasher scratch (List.length value);
      List.iter value ~f:(loop repr)
    | Array repr ->
      feed_int hasher scratch 7;
      feed_int hasher scratch (Array.length value);
      Array.iter value ~f:(loop repr)
    | Pair (left, right) ->
      feed_int hasher scratch 8;
      let left_value, right_value = value in
      loop left left_value;
      loop right right_value
    | Triple (first, second, third) ->
      feed_int hasher scratch 9;
      let first_value, second_value, third_value = value in
      loop first first_value;
      loop second second_value;
      loop third third_value
    | Fix repr -> loop (Lazy.force repr) value
    | Record (_, fields) ->
      feed_int hasher scratch 10;
      loop_fields fields value
    | Variant (_, cases) ->
      feed_int hasher scratch 11;
      loop_cases cases value
    | View { repr; to_ } -> loop repr (to_ value)
    | Abstract _ ->
      Code_error.raise
        "Digest.repr does not support Repr.abstract"
        [ "repr", Dyn.string "<abstract>" ]
  and loop_fields : type a. a Repr.field list -> a -> unit =
    fun fields value ->
    feed_int hasher scratch (List.length fields);
    List.iter fields ~f:(fun (Repr.Field { name; repr; get }) ->
      feed_string hasher scratch name;
      loop repr (get value))
  and loop_cases : type a. a Repr.case list -> a -> unit =
    fun cases value ->
    match cases with
    | [] ->
      Code_error.raise
        "Repr.variant: value did not match any case"
        [ "value", Dyn.string "<opaque>" ]
    | Repr.Case0 { tag; test } :: rest ->
      if test value
      then (
        feed_string hasher scratch tag;
        feed_bool hasher scratch false)
      else loop_cases rest value
    | Repr.Case1 { tag; repr; proj } :: rest ->
      (match proj value with
       | Some argument ->
         feed_string hasher scratch tag;
         feed_bool hasher scratch true;
         loop repr argument
       | None -> loop_cases rest value)
  in
  loop
;;

module Feed = struct
  type hasher = Hasher.t
  type 'a t = hasher -> 'a -> unit

  let contramap a ~f hasher b = a hasher (f b)

  let string hasher s =
    Counter.add Metrics.Digest.Value.bytes (String.length s);
    Blake3_mini.feed_string hasher s ~pos:0 ~len:(String.length s)
  ;;

  let bool = contramap string ~f:Bool.to_string
  let int = contramap string ~f:Int.to_string
  let repr repr hasher value = feed_repr hasher repr value

  (* We use [No_sharing] to avoid generating different digests for inputs that
       differ only in how they share internal values. Without [No_sharing], if a
       command line contains duplicate flags, such as multiple occurrences of the
       flag [-I], then [Marshal.to_string] will produce different digests depending
       on whether the corresponding strings ["-I"] point to the same memory location
       or to different memory locations. *)
  let generic hasher x = contramap string ~f:(Marshal.to_string ~sharing:false) hasher x

  let list feed_x hasher xs =
    int hasher (List.length xs);
    List.iter xs ~f:(feed_x hasher)
  ;;

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
  let compute_digest_with t x ~with_hasher = with_hasher (fun hasher -> t hasher x)
  let compute_digest t x = compute_digest_with t x ~with_hasher:Hasher.with_singleton
  let compute_digest_pooled t x = compute_digest_with t x ~with_hasher:Hasher.with_pooled
end

module Manual = struct
  type t = unit

  let scratch = Bytes.create 8
  let create () = ()

  let feed_string_raw s =
    Blake3_mini.feed_string ~pos:0 ~len:(String.length s) (Lazy.force Hasher.singleton) s
  ;;

  let feed_int64 i =
    for byte = 0 to 7 do
      let shift = 8 * byte in
      let value = Int64.(to_int (logand (shift_right_logical i shift) 0xffL)) in
      Bytes.set scratch byte (Char.chr value)
    done;
    feed_string_raw (Bytes.unsafe_to_string scratch)
  ;;

  let bool () b =
    Bytes.set scratch 0 (if b then '\001' else '\000');
    Blake3_mini.feed_string
      ~pos:0
      ~len:1
      (Lazy.force Hasher.singleton)
      (Bytes.unsafe_to_string scratch)
  ;;

  let int () i = feed_int64 (Int64.of_int i)

  let string () s =
    int () (String.length s);
    feed_string_raw s
  ;;

  let option t ~f = function
    | None -> bool t false
    | Some x ->
      bool t true;
      f t x
  ;;

  let list t ~f xs =
    int t (List.length xs);
    List.iter xs ~f:(f t)
  ;;

  let repr () repr value = Feed.repr repr (Lazy.force Hasher.singleton) value

  let digest () s =
    let s = Blake3_mini.Digest.to_binary s in
    feed_string_raw s
  ;;

  let get () =
    let hasher = Lazy.force Hasher.singleton in
    let res = Blake3_mini.digest hasher in
    Blake3_mini.reset hasher;
    res
  ;;
end

let string s = Feed.compute_digest Feed.string s
let string_pooled s = Feed.compute_digest_pooled Feed.string s
let to_string_raw s = Blake3_mini.Digest.to_binary s

let generic a =
  let start = Counter.Timer.start () in
  Counter.incr Metrics.Digest.Value.count;
  let res = Feed.compute_digest Feed.generic a in
  Counter.Timer.stop Metrics.Digest.Value.time start;
  res
;;

let repr repr a =
  let start = Counter.Timer.start () in
  Counter.incr Metrics.Digest.Value.count;
  let res = Feed.compute_digest (Feed.repr repr) a in
  Counter.Timer.stop Metrics.Digest.Value.time start;
  res
;;

let generic_pooled a =
  let start = Counter.Timer.start () in
  Counter.incr Metrics.Digest.Value.count;
  let res = Feed.compute_digest_pooled Feed.generic a in
  Counter.Timer.stop Metrics.Digest.Value.time start;
  res
;;

let path_with_executable_bit_with string_digest =
  (* We follow the digest scheme used by Jenga. *)
  let string_and_bool ~digest_hex ~bool =
    let suffix = if bool then "\001" else "\000" in
    string_digest (Blake3_mini.Digest.to_hex digest_hex ^ suffix)
  in
  fun ~executable ~content_digest ->
    string_and_bool ~digest_hex:content_digest ~bool:executable
;;

let path_with_executable_bit = path_with_executable_bit_with string
let path_with_executable_bit_pooled = path_with_executable_bit_with string_pooled

let file_with_executable_bit_sync ~executable path =
  let content_digest = file path in
  path_with_executable_bit ~content_digest ~executable
;;

let file_with_executable_bit_pooled ~executable path =
  let content_digest = file path in
  path_with_executable_bit_pooled ~content_digest ~executable
;;

module Stats_for_digest = struct
  type t =
    { st_kind : Unix.file_kind
    ; executable : bool
    }

  let of_kind_and_perm ~st_kind ~perm =
    (* Check if any of the +x bits are set, ignore read and write *)
    let executable = 0o111 land perm <> 0 in
    { st_kind; executable }
  ;;

  let of_unix_stats (stats : Unix.stats) =
    of_kind_and_perm ~st_kind:stats.st_kind ~perm:stats.st_perm
  ;;

  let of_time_stat (stats : Stat.t) =
    of_kind_and_perm ~st_kind:stats.kind ~perm:stats.perm
  ;;
end

module Path_digest_error = struct
  type nonrec t =
    | Unexpected_kind
    | Unix_error of Unix_error.Detailed.t
end

exception E of Path_digest_error.t

let directory_digest_version = 3

let path_with_stats_internal
      ~allow_dirs
      ~string_digest
      ~generic_digest
      ~file_with_executable_bit
      path
      (stats : Stats_for_digest.t)
  =
  let rec loop path (stats : Stats_for_digest.t) =
    match stats.st_kind with
    | S_LNK ->
      Unix_error.Detailed.catch
        (fun path ->
           let contents = Path.to_string path |> Unix.readlink |> string_digest in
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
            Ok (generic_digest (directory_digest_version, contents, stats.executable))))
    | S_DIR | S_BLK | S_CHR | S_FIFO | S_SOCK -> Error Unexpected_kind
  in
  match stats.st_kind with
  | S_DIR when not allow_dirs -> Error Path_digest_error.Unexpected_kind
  | S_BLK | S_CHR | S_LNK | S_FIFO | S_SOCK -> Error Unexpected_kind
  | _ -> loop path stats
;;

let path_with_stats ~allow_dirs path stats =
  path_with_stats_internal
    ~allow_dirs
    ~string_digest:string
    ~generic_digest:generic
    ~file_with_executable_bit:file_with_executable_bit_sync
    path
    stats
;;

let path_with_stats_async ~allow_dirs path (stats : Stats_for_digest.t) =
  let f () =
    path_with_stats_internal
      ~allow_dirs
      ~string_digest:string_pooled
      ~generic_digest:generic_pooled
      ~file_with_executable_bit:file_with_executable_bit_pooled
      path
      stats
  in
  match Config.(get background_digests) with
  | `Disabled -> Fiber.return (f ())
  | `Enabled -> Dune_scheduler.Scheduler.async_exn f
;;

let file_with_executable_bit ~executable path =
  let open Fiber.O in
  let+ content_digest = file_async path in
  path_with_executable_bit ~content_digest ~executable
;;
