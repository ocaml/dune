let close_in = close_in
let close_out = close_out

let close_both (ic, oc) =
  match close_out oc with
  | () -> close_in ic
  | exception exn ->
    close_in ic;
    Exn.reraise exn
;;

let input_lines =
  let rec loop ic acc =
    match input_line ic with
    | exception End_of_file -> List.rev acc
    | line -> loop ic (line :: acc)
  in
  fun ic -> loop ic []
;;

let input_zero_from_buffer from buf =
  match String.index_from_opt buf from '\x00' with
  | None -> None
  | Some eos -> Some (String.sub buf ~pos:from ~len:(eos - from), eos + 1)
;;

(* Note, the complexity of this function will be bad if the zero-separated
   elements are much larger than the current input buffer *)
let input_zero_separated =
  (* Take all the \0-terminated strings from [buf], return the scanned list and
     the remainder *)
  let rec scan_inputs_buf from buf acc =
    (* note that from is untouched if input_zero_from_buffer returns None *)
    match input_zero_from_buffer from buf with
    | Some (istr, from) -> scan_inputs_buf from buf (istr :: acc)
    | None ->
      let total_len = String.length buf in
      if total_len > from
      then (
        let rest = String.sub buf ~pos:from ~len:(total_len - from) in
        Some rest, acc)
      else None, acc
  in
  let ibuf_size = 65536 in
  let ibuf = Bytes.create ibuf_size in
  let rec input_loop ic rem acc =
    let res = input ic ibuf 0 ibuf_size in
    if res = 0
    then (
      (* end of file, check if there is a remainder, and return the results *)
      match rem with
      | Some rem -> List.rev (rem :: acc)
      | None -> List.rev acc)
    else (
      (* new input, append remainder and scan it *)
      let actual_input = Bytes.sub_string ibuf ~pos:0 ~len:res in
      let actual_input =
        match rem with
        | None -> actual_input
        | Some rem -> rem ^ actual_input
      in
      let rem, acc = scan_inputs_buf 0 actual_input acc in
      input_loop ic rem acc)
  in
  fun ic -> input_loop ic None []
;;

let copy_channels =
  let buf_len = 65536 in
  let buf = Bytes.create buf_len in
  let rec loop ic oc =
    match input ic buf 0 buf_len with
    | 0 -> ()
    | n ->
      output oc buf 0 n;
      loop ic oc
  in
  loop
;;

let setup_copy ?(chmod = Fun.id) ~src ~dst () =
  let ic = Stdlib.open_in_bin src in
  let oc =
    try
      let perm = (Unix.fstat (Unix.descr_of_in_channel ic)).st_perm |> chmod in
      Stdlib.open_out_gen [ Open_wronly; Open_creat; Open_trunc; Open_binary ] perm dst
    with
    | exn ->
      close_in ic;
      Exn.reraise exn
  in
  ic, oc
;;

module Copyfile = struct
  (* Bindings to mac's fast copy function. It's similar to a hardlink, except
     it does COW when edited. It will also default back to regular copying if
     it fails for w/e reason *)
  external copyfile : string -> string -> unit = "stdune_copyfile"

  external sendfile
    :  src:Unix.file_descr
    -> dst:Unix.file_descr
    -> int
    -> unit
    = "stdune_sendfile"

  let available =
    match Platform.OS.value with
    | Darwin -> `Copyfile
    | Linux -> `Sendfile
    | _ -> `Nothing
  ;;

  let sendfile_with_fallback =
    let setup_copy ?(chmod = Fun.id) ~src ~dst () =
      match Unix.openfile src [ O_RDONLY; O_CLOEXEC ] 0 with
      | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Error `Src_missing
      | fd_src ->
        (match Unix.fstat fd_src with
         | exception exn ->
           Unix.close fd_src;
           Error (`Exn (Exn_with_backtrace.capture exn))
         | src_stat ->
           (match src_stat.st_kind with
            | S_DIR -> Error `Src_is_a_dir
            | _ ->
              let open Result.O in
              let+ fd_dst, src_size =
                match
                  let dst_perm = chmod src_stat.st_perm in
                  Unix.openfile dst [ O_WRONLY; O_CREAT; O_TRUNC; O_CLOEXEC ] dst_perm
                with
                | fd_dst -> Ok (fd_dst, src_stat.st_size)
                | exception exn ->
                  Unix.close fd_src;
                  (match exn with
                   | Unix.Unix_error (Unix.EISDIR, _, _) -> Error `Dst_is_a_dir
                   | _ -> Error (`Exn (Exn_with_backtrace.capture exn)))
              in
              fd_src, fd_dst, src_size))
    in
    fun ?chmod ~src ~dst () ->
      (* All of this exception translation is done for now to maintain the
         same error messages as the other file copying functions.

         Eventually, we should stop using exceptions for signalling these
         errors. But that's a bit of a large change since there's a lot of
         exception catching to audit. *)
      match setup_copy ?chmod ~src ~dst () with
      | Error (`Exn exn) -> Exn_with_backtrace.reraise exn
      | Error `Src_is_a_dir -> raise (Sys_error "Is a directory")
      | Error `Dst_is_a_dir ->
        let message = Printf.sprintf "%s: Is a directory" dst in
        raise (Sys_error message)
      | Error `Src_missing ->
        let message = Printf.sprintf "%s: No such file or directory" src in
        raise (Sys_error message)
      | Ok (src, dst, src_size) ->
        let close_fds () =
          Unix.close src;
          Unix.close dst
        in
        (match sendfile ~src ~dst src_size with
         | exception Unix.Unix_error (EINVAL, "sendfile", _) ->
           Exn.protectx
             (Unix.in_channel_of_descr src, Unix.out_channel_of_descr dst)
             (* we make sure to close the fd's with the channel api to make
                sure everything has been flushed *)
             ~f:(fun (ic, oc) -> copy_channels ic oc)
             ~finally:close_both
         | () -> close_fds ()
         | exception exn ->
           close_fds ();
           Exn.reraise exn)
  ;;

  let copyfile ?chmod ~src ~dst () =
    let src_stats =
      match Unix.stat src with
      | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
        let message = Printf.sprintf "%s: No such file or directory" src in
        raise (Sys_error message)
      | { st_kind = S_DIR; _ } -> raise (Sys_error "Is a directory")
      | stats -> stats
    in
    (try copyfile src dst with
     | Unix.Unix_error (Unix.EPERM, "unlink", _) ->
       let message = Printf.sprintf "%s: Is a directory" dst in
       raise (Sys_error message)
     | Unix.Unix_error (Unix.ENOENT, "realpath", _) ->
       let message = Printf.sprintf "%s: No such file or directory" src in
       raise (Sys_error message));
    match chmod with
    | None -> ()
    | Some chmod -> src_stats.st_perm |> chmod |> Unix.chmod dst
  ;;

  let copy_file_portable ?chmod ~src ~dst () =
    Exn.protectx (setup_copy ?chmod ~src ~dst ()) ~finally:close_both ~f:(fun (ic, oc) ->
      copy_channels ic oc)
  ;;

  let copy_file_best =
    match available with
    | `Sendfile -> sendfile_with_fallback
    | `Copyfile -> copyfile
    | `Nothing -> copy_file_portable
  ;;

  let copy_file_impl = ref `Best

  let copy_file ?chmod ~src ~dst () =
    match !copy_file_impl with
    | `Portable -> copy_file_portable ?chmod ~src ~dst ()
    | `Best -> copy_file_best ?chmod ~src ~dst ()
  ;;
end

let set_copy_impl m = Copyfile.copy_file_impl := m

module Make (Path : sig
    type t

    val to_string : t -> string
  end) =
struct
  type path = Path.t

  let open_in ?(binary = true) p =
    let fn = Path.to_string p in
    if binary then Stdlib.open_in_bin fn else Stdlib.open_in fn
  ;;

  let open_out ?(binary = true) ?(perm = 0o666) p =
    let fn = Path.to_string p in
    let flags : Stdlib.open_flag list =
      [ Open_wronly; Open_creat; Open_trunc; (if binary then Open_binary else Open_text) ]
    in
    Stdlib.open_out_gen flags perm fn
  ;;

  let with_file_in ?binary fn ~f = Exn.protectx (open_in ?binary fn) ~finally:close_in ~f

  let with_file_out ?binary ?perm p ~f =
    Exn.protectx (open_out ?binary ?perm p) ~finally:close_out ~f
  ;;

  let with_lexbuf_from_file fn ~f =
    with_file_in fn ~f:(fun ic ->
      let lb = Lexing.from_channel ic in
      lb.lex_curr_p
      <- { pos_fname = Path.to_string fn; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
      f lb)
  ;;

  let rec eagerly_input_acc ic s ~pos ~len acc =
    if len <= 0
    then acc
    else (
      let r = input ic s pos len in
      if r = 0 then acc else eagerly_input_acc ic s ~pos:(pos + r) ~len:(len - r) (acc + r))
  ;;

  (* [eagerly_input_string ic len] tries to read [len] chars from the channel.
     Unlike [really_input_string], if the file ends before [len] characters are
     found, it returns the characters it was able to read instead of raising an
     exception.

     This can be detected by checking that the length of the resulting string is
     less than [len]. *)
  let eagerly_input_string ic len =
    let buf = Bytes.create len in
    let r = eagerly_input_acc ic buf ~pos:0 ~len 0 in
    if r = len then Bytes.unsafe_to_string buf else Bytes.sub_string buf ~pos:0 ~len:r
  ;;

  let read_all_unless_large =
    (* We use 65536 because that is the size of OCaml's IO buffers. *)
    let chunk_size = 65536 in
    (* Generic function for channels such that seeking is unsupported or
       broken *)
    let read_all_generic t buffer =
      let rec loop () =
        Buffer.add_channel buffer t chunk_size;
        loop ()
      in
      try loop () with
      | End_of_file -> Ok (Buffer.contents buffer)
    in
    fun t ->
      (* Optimisation for regular files: if the channel supports seeking, we
         compute the length of the file so that we read exactly what we need and
         avoid an extra memory copy. We expect that most files Dune reads are
         regular files so this optimizations seems worth it. *)
      match in_channel_length t with
      | exception Sys_error _ -> read_all_generic t (Buffer.create chunk_size)
      | n when n > Sys.max_string_length -> Error ()
      | n ->
        (* For some files [in_channel_length] returns an invalid value. For
           instance for files in /proc it returns [0] and on Windows the
           returned value is larger than expected (it counts linebreaks as 2
           chars, even in text mode).

           To be robust in both directions, we: - use [eagerly_input_string]
           instead of [really_input_string] in case we reach the end of the file
           early - read one more character to make sure we did indeed reach the
           end of the file *)
        let s = eagerly_input_string t n in
        (match input_char t with
         | exception End_of_file -> Ok s
         | c ->
           (* The [+ chunk_size] is to make sure there is at least [chunk_size]
              free space so that the first [Buffer.add_channel buffer t
             chunk_size] in [read_all_generic] does not grow the buffer. *)
           let buffer = Buffer.create (String.length s + 1 + chunk_size) in
           Buffer.add_string buffer s;
           Buffer.add_char buffer c;
           read_all_generic t buffer)
  ;;

  let path_to_dyn path = String.to_dyn (Path.to_string path)

  let read_file ?binary fn =
    match with_file_in fn ~f:read_all_unless_large ?binary with
    | Ok x -> x
    | Error () ->
      Code_error.raise
        "read_file: file is larger than Sys.max_string_length"
        [ "fn", path_to_dyn fn ]
  ;;

  let lines_of_file fn = with_file_in fn ~f:input_lines ~binary:false
  let zero_strings_of_file fn = with_file_in fn ~f:input_zero_separated ~binary:true

  let write_file ?binary ?perm fn data =
    with_file_out ?binary ?perm fn ~f:(fun oc -> output_string oc data)
  ;;

  let write_lines ?binary ?perm fn lines =
    with_file_out ?binary ?perm fn ~f:(fun oc ->
      List.iter
        ~f:(fun line ->
          output_string oc line;
          output_string oc "\n")
        lines)
  ;;

  let read_file_and_normalize_eols fn =
    if not Stdlib.Sys.win32
    then read_file fn
    else (
      let src = read_file fn in
      let len = String.length src in
      let dst = Bytes.create len in
      let rec find_next_crnl i =
        match String.index_from src i '\r' with
        | None -> None
        | Some j ->
          if j + 1 < len && src.[j + 1] = '\n' then Some j else find_next_crnl (j + 1)
      in
      let rec loop src_pos dst_pos =
        match find_next_crnl src_pos with
        | None ->
          let len =
            if len > src_pos && src.[len - 1] = '\r'
            then len - 1 - src_pos
            else len - src_pos
          in
          Bytes.blit_string ~src ~src_pos ~dst ~dst_pos ~len;
          Bytes.sub_string dst ~pos:0 ~len:(dst_pos + len)
        | Some i ->
          let len = i - src_pos in
          Bytes.blit_string ~src ~src_pos ~dst ~dst_pos ~len;
          let dst_pos = dst_pos + len in
          Bytes.set dst dst_pos '\n';
          loop (i + 2) (dst_pos + 1)
      in
      loop 0 0)
  ;;

  let compare_text_files fn1 fn2 =
    let s1 = read_file_and_normalize_eols fn1 in
    let s2 = read_file_and_normalize_eols fn2 in
    String.compare s1 s2
  ;;

  let compare_files fn1 fn2 =
    let s1 = read_file fn1 in
    let s2 = read_file fn2 in
    String.compare s1 s2
  ;;

  let setup_copy ?chmod ~src ~dst () =
    let src = Path.to_string src in
    let dst = Path.to_string dst in
    setup_copy ?chmod ~src ~dst ()
  ;;

  let copy_file ?chmod ~src ~dst () =
    let src = Path.to_string src in
    let dst = Path.to_string dst in
    Copyfile.copy_file ?chmod ~src ~dst ()
  ;;

  let file_line path n =
    with_file_in ~binary:false path ~f:(fun ic ->
      for _ = 1 to n - 1 do
        ignore (input_line ic)
      done;
      input_line ic)
  ;;

  let file_lines path ~start ~stop =
    with_file_in ~binary:true path ~f:(fun ic ->
      let rec aux acc lnum =
        if lnum > stop
        then List.rev acc
        else if lnum < start
        then (
          ignore (input_line ic);
          aux acc (lnum + 1))
        else (
          let line = input_line ic in
          aux ((string_of_int lnum, line) :: acc) (lnum + 1))
      in
      aux [] 1)
  ;;

  let cat ?binary ?dst fn =
    let dst =
      match dst with
      | Some dst -> dst
      | None -> stdout
    in
    with_file_in ?binary fn ~f:(fun ic -> copy_channels ic dst)
  ;;
end

include Make (Path)

module String_path = struct
  include Make (struct
      type t = string

      let to_string x = x
    end)

  let copy_file = Copyfile.copyfile
end

let portable_symlink ~src ~dst =
  if Stdlib.Sys.win32
  then copy_file ~src ~dst ()
  else (
    let src =
      match Path.parent dst with
      | None -> Path.to_string src
      | Some from -> Path.reach ~from src
    in
    let dst = Path.to_string dst in
    match Unix.readlink dst with
    | target ->
      if target <> src
      then (
        (* @@DRA Win32 remove read-only attribute needed when symlinking
           enabled *)
        Unix.unlink dst;
        Unix.symlink src dst)
    | exception Unix.Unix_error _ -> Unix.symlink src dst)
;;

let portable_hardlink ~src ~dst =
  let user_error msg =
    User_error.raise
      [ Pp.textf
          "Sandbox creation error: cannot resolve symbolic link %S."
          (Path.to_string src)
      ; Pp.textf "Reason: %s" msg
      ]
  in
  (* CR-someday amokhov: Instead of always falling back to copying, we could
     detect if hardlinking works on Windows and if yes, use it. We do this in
     the Dune cache implementation, so we can share some code. *)
  match Stdlib.Sys.win32 with
  | true -> copy_file ~src ~dst ()
  | false ->
    let src =
      match Path.follow_symlink src with
      | Ok path -> path
      | Error Not_a_symlink -> src
      | Error Max_depth_exceeded ->
        user_error "Too many indirections; is this a cyclic symbolic link?"
      | Error (Unix_error error) ->
        user_error (Dune_filesystem_stubs.Unix_error.Detailed.to_string_hum error)
    in
    (try Path.link src dst with
     | Unix.Unix_error (Unix.EEXIST, _, _) ->
       (* CR-someday amokhov: Investigate why we need to occasionally clear the
          destination (we also do this in the symlink case above). Perhaps, the
          list of dependencies may have duplicates? If yes, it may be better to
          filter out the duplicates first. *)
       Path.unlink_exn dst;
       Path.link src dst
     | Unix.Unix_error (Unix.EMLINK, _, _) ->
       (* If we can't make a new hard link because we reached the limit on the
          number of hard links per file, we fall back to copying. We expect
          that this happens very rarely (probably only for empty files). *)
       copy_file ~src ~dst ())
;;
