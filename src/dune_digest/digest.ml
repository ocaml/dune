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

  module Scratch = struct
    let buf = Bytes.create 4096
    let len = Bytes.length buf
    let pos = ref 0

    let flush () =
      if !pos > 0
      then (
        Blake3_mini.feed_bytes ~pos:0 ~len:!pos (Lazy.force singleton) buf;
        pos := 0)
    ;;
  end

  let feed_manual_string s =
    let length = String.length s in
    if length > Scratch.len
    then (
      Scratch.flush ();
      Blake3_mini.feed_string ~pos:0 ~len:length (Lazy.force singleton) s)
    else (
      if !Scratch.pos + length > Scratch.len then Scratch.flush ();
      let pos = !Scratch.pos in
      Bytes.blit_string ~src:s ~src_pos:0 ~dst:Scratch.buf ~dst_pos:pos ~len:length;
      Scratch.pos := pos + length)
  ;;

  let feed_manual_int i =
    if !Scratch.pos + 8 > Scratch.len then Scratch.flush ();
    let pos = !Scratch.pos in
    Bytes.set_int64_le Scratch.buf pos (Int64.of_int i);
    Scratch.pos := pos + 8
  ;;

  let feed_manual_bool b =
    if !Scratch.pos = Scratch.len then Scratch.flush ();
    Bytes.set Scratch.buf !Scratch.pos (if b then '\001' else '\000');
    Scratch.pos := !Scratch.pos + 1
  ;;

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
        (* [Manual] shares this hasher, so preserve the ordering of pending input
           before using it for a nested digest computation. *)
        Scratch.flush ();
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
  |> Fd.unsafe_of_unix_file_descr
;;

let digest_and_close_fd fd =
  let start = Counter.Timer.start () in
  let res =
    Exn.protectx
      fd
      ~f:(fun fd -> Blake3_mini.fd (Fd.unsafe_to_unix_file_descr fd))
      ~finally:Fd.close
  in
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

let file_async =
  let digest_throttle = lazy (Fiber.Throttle.create 32) in
  fun file ->
    Fiber.Throttle.run (Lazy.force digest_throttle) ~f:(fun () ->
      let open Fiber.O in
      let start = Counter.Timer.start () in
      let+ digest, size =
        Dune_scheduler.Scheduler.async_exn (fun () -> Blake3_mini.file_with_size file)
      in
      Counter.incr Metrics.Digest.File.count;
      Counter.add Metrics.Digest.File.bytes size;
      Counter.Timer.stop Metrics.Digest.File.time start;
      digest)
;;

let equal = Blake3_mini.Digest.equal
let hash = Blake3_mini.Digest.hash
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
  Bytes.set_int64_le scratch 0 i;
  feed_bytes_raw hasher scratch ~len:8
;;

let feed_bool hasher scratch b =
  Bytes.set scratch 0 (if b then '\001' else '\000');
  feed_bytes_raw hasher scratch ~len:1
;;

let feed_int hasher scratch i =
  Bytes.set_int64_le scratch 0 (Int64.of_int i);
  feed_bytes_raw hasher scratch ~len:8
;;

let feed_string hasher scratch s =
  feed_int hasher scratch (String.length s);
  feed_string_raw hasher s
;;

let rec feed_repr : type a. Hasher.t -> Bytes.t -> a Repr.t -> a -> unit =
  fun hasher scratch repr value ->
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
  | Int32 ->
    feed_int hasher scratch 12;
    feed_int64 hasher scratch (Int64.of_int32 value)
  | Int64 ->
    feed_int hasher scratch 13;
    feed_int64 hasher scratch value
  | Nativeint ->
    feed_int hasher scratch 14;
    feed_int64 hasher scratch (Int64.of_nativeint value)
  | Bytes ->
    feed_int hasher scratch 15;
    feed_int hasher scratch (Bytes.length value);
    feed_bytes_raw hasher value ~len:(Bytes.length value)
  | Char ->
    feed_int hasher scratch 16;
    feed_int hasher scratch (Char.code value)
  | Float ->
    feed_int hasher scratch 17;
    feed_int64 hasher scratch (Int64.bits_of_float value)
  | Option repr ->
    feed_int hasher scratch 5;
    (match value with
     | None -> feed_bool hasher scratch false
     | Some x ->
       feed_bool hasher scratch true;
       feed_repr hasher scratch repr x)
  | List repr ->
    feed_int hasher scratch 6;
    feed_int hasher scratch (List.length value);
    List.iter value ~f:(feed_repr hasher scratch repr)
  | Array repr ->
    feed_int hasher scratch 7;
    feed_int hasher scratch (Array.length value);
    Array.iter value ~f:(feed_repr hasher scratch repr)
  | Pair (left, right) ->
    feed_int hasher scratch 8;
    let left_value, right_value = value in
    feed_repr hasher scratch left left_value;
    feed_repr hasher scratch right right_value
  | Triple (first, second, third) ->
    feed_int hasher scratch 9;
    let first_value, second_value, third_value = value in
    feed_repr hasher scratch first first_value;
    feed_repr hasher scratch second second_value;
    feed_repr hasher scratch third third_value
  | Quadruple (first, second, third, fourth) ->
    feed_int hasher scratch 18;
    let first_value, second_value, third_value, fourth_value = value in
    feed_repr hasher scratch first first_value;
    feed_repr hasher scratch second second_value;
    feed_repr hasher scratch third third_value;
    feed_repr hasher scratch fourth fourth_value
  | Fix repr -> feed_repr hasher scratch (Lazy.force repr) value
  | Record (_, fields) ->
    feed_int hasher scratch 10;
    feed_repr_fields hasher scratch fields value
  | Variant (_, cases) ->
    feed_int hasher scratch 11;
    feed_repr_cases hasher scratch cases value
  | View { repr; to_ } -> feed_repr hasher scratch repr (to_ value)
  | Abstract _ ->
    Code_error.raise
      "Digest.repr does not support Repr.abstract"
      [ "repr", Dyn.string "<abstract>" ]

and feed_repr_fields : type a. Hasher.t -> Bytes.t -> a Repr.field list -> a -> unit =
  fun hasher scratch fields value ->
  feed_int hasher scratch (List.length fields);
  List.iter fields ~f:(fun (Repr.Field { name; repr; get }) ->
    feed_string hasher scratch name;
    feed_repr hasher scratch repr (get value))

and feed_repr_cases : type a. Hasher.t -> Bytes.t -> a Repr.case list -> a -> unit =
  fun hasher scratch cases value ->
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
    else feed_repr_cases hasher scratch rest value
  | Repr.Case1 { tag; repr; proj } :: rest ->
    (match proj value with
     | Some argument ->
       feed_string hasher scratch tag;
       feed_bool hasher scratch true;
       feed_repr hasher scratch repr argument
     | None -> feed_repr_cases hasher scratch rest value)
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
  let repr repr hasher value = feed_repr hasher (Bytes.create 8) repr value

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

  let create () = ()
  let bool () = Hasher.feed_manual_bool
  let int () = Hasher.feed_manual_int

  let string () s =
    int () (String.length s);
    Hasher.feed_manual_string s
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

  let repr () repr value =
    Hasher.Scratch.flush ();
    feed_repr (Lazy.force Hasher.singleton) Hasher.Scratch.buf repr value
  ;;

  let digest () s =
    let s = Blake3_mini.Digest.to_binary s in
    Hasher.feed_manual_string s
  ;;

  let get () =
    Hasher.Scratch.flush ();
    let hasher = Lazy.force Hasher.singleton in
    let res = Blake3_mini.digest hasher in
    Blake3_mini.reset hasher;
    res
  ;;
end

let string s = Feed.compute_digest Feed.string s
let string_pooled s = Feed.compute_digest_pooled Feed.string s
let to_string_raw s = Blake3_mini.Digest.to_binary s
let digest_repr = Repr.view Repr.string ~to_:to_string

let repr_with compute_digest repr a =
  let start = Counter.Timer.start () in
  Counter.incr Metrics.Digest.Value.count;
  let res = compute_digest (Feed.repr repr) a in
  Counter.Timer.stop Metrics.Digest.Value.time start;
  res
;;

let repr repr a = repr_with Feed.compute_digest repr a
let repr_pooled repr a = repr_with Feed.compute_digest_pooled repr a

let path_with_executable_bit_with string_digest =
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

let directory_digest_with =
  let directory_digest_version = 4 in
  let directory_digest_repr = Repr.(triple int (list (pair string digest_repr)) bool) in
  fun repr_digest ~contents ~executable ->
    repr_digest directory_digest_repr (directory_digest_version, contents, executable)
;;

let path_with_stats_internal
      ~allow_dirs
      ~string_digest
      ~directory_digest
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
              let name = Filename.to_string name in
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
          | contents -> Ok (directory_digest ~contents ~executable:stats.executable)))
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
    ~directory_digest:(directory_digest_with repr)
    ~file_with_executable_bit:file_with_executable_bit_sync
    path
    stats
;;

let path_with_stats_async ~allow_dirs path (stats : Stats_for_digest.t) =
  let f () =
    path_with_stats_internal
      ~allow_dirs
      ~string_digest:string_pooled
      ~directory_digest:(directory_digest_with repr_pooled)
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
