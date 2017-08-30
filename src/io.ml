open Import

module P = Pervasives

module Win32_io = struct

  (* Pervasives.open* support neither O_CLOEXEC nor O_SHARE_DELETE *)

  let sys_exn e fn =
    let msg = fn ^ ":" ^ Unix.error_message e in
    raise (Sys_error msg)

  let eclose ecode fd fn =
    (try Unix.close fd with Unix.Unix_error _ -> ());
    sys_exn ecode fn

  let open_gen mode perm fn =
    let init = [Unix.O_SHARE_DELETE ; Unix.O_CLOEXEC] in
    let trans acc = function
    |	Open_rdonly -> Unix.O_RDONLY::acc
    |	Open_wronly -> Unix.O_WRONLY::acc
    |	Open_creat -> Unix.O_CREAT::acc
    |	Open_trunc -> Unix.O_TRUNC::acc
    |	Open_excl -> Unix.O_EXCL::acc
    |	Open_nonblock -> Unix.O_NONBLOCK::acc
    |	Open_append (* lseek must be used, if the HANDLE might be passed
                     to a child process. The not implemented support of
                     Unix.O_APPEND (FILE_APPEND_DATA) would probably not work
                     in this case anyway *)
    |	Open_binary (* Open_binary/Open_text are handled in open_(in|out)_gen *)
    |	Open_text -> acc in
    let m = List.fold_left ~f:trans ~init mode in
    match Unix.openfile fn m perm with
    | exception (Unix.Unix_error(x,_,_)) -> sys_exn x fn
    | fd ->
      if List.mem Open_append ~set:mode then (
        try
          ignore (Unix.lseek fd 0 Unix.SEEK_END : int)
        with
        | Unix.Unix_error(e,_,_) -> eclose e fd fn
      );
      fd

  let open_out_gen mode perm fn =
    let fd = open_gen mode perm fn in
    match Unix.out_channel_of_descr fd with
    | exception (Unix.Unix_error(e,_,_)) -> eclose e fd fn
    | oc ->
      if List.mem Open_text ~set:mode then (
        try
          set_binary_mode_out oc false
        with
        | x -> close_out oc ; raise x
      );
      oc

  let open_out name =
    open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_text] 0o666 name

  let open_out_bin name =
    open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o666 name

  let open_in_gen mode perm fn =
    let fd = open_gen mode perm fn in
    match Unix.in_channel_of_descr fd with
    | exception (Unix.Unix_error(e,_,_)) -> eclose e fd fn
    | ic ->
      if List.mem Open_text ~set:mode then (
        try
          set_binary_mode_in ic false
        with
        | x -> close_in ic ; raise x
      );
      ic

  let open_in name =
    open_in_gen [Open_rdonly; Open_text] 0 name

  let open_in_bin name =
    open_in_gen [Open_rdonly; Open_binary] 0 name
end

let open_in,open_in_bin,open_out,open_out_bin =
  if Sys.win32 then
    Win32_io.(open_in,open_in_bin,open_out,open_out_bin)
  else
    P.(open_in,open_in_bin,open_out,open_out_bin)

let open_in ?(binary=true) fn =
  if binary then open_in_bin fn else open_in fn

let open_out ?(binary=true) fn =
  if binary then open_out_bin fn else open_out fn

let close_in  = close_in
let close_out = close_out

let with_file_in ?binary fn ~f =
  protectx (open_in ?binary fn) ~finally:close_in ~f

let with_file_out ?binary fn ~f =
  protectx (open_out ?binary fn) ~finally:close_out ~f

let with_lexbuf_from_file fn ~f =
  with_file_in fn ~f:(fun ic ->
    let lb = Lexing.from_channel ic in
    lb.lex_curr_p <-
      { pos_fname = fn
      ; pos_lnum  = 1
      ; pos_bol   = 0
      ; pos_cnum  = 0
      };
    f lb)

let input_lines =
  let rec loop ic acc =
    match input_line ic with
    | exception End_of_file -> List.rev acc
    | line ->
       loop ic (line :: acc)
  in
  fun ic -> loop ic []

let read_file fn =
  with_file_in fn ~f:(fun ic ->
    let len = in_channel_length ic in
    really_input_string ic len)

let lines_of_file fn = with_file_in fn ~f:input_lines ~binary:false

let write_file fn data = with_file_out fn ~f:(fun oc -> output_string oc data)

let copy_channels =
  let buf_len = 65536 in
  let buf = Bytes.create buf_len in
  let rec loop ic oc =
    match input ic buf 0 buf_len with
    | 0 -> ()
    | n -> output oc buf 0 n; loop ic oc
  in
  loop

let copy_file ~src ~dst =
  with_file_in src ~f:(fun ic ->
    let perm = (Unix.fstat (Unix.descr_of_in_channel ic)).st_perm in
    protectx (P.open_out_gen
                [Open_wronly; Open_creat; Open_trunc; Open_binary]
                perm
                dst)
      ~finally:close_out
      ~f:(fun oc ->
        copy_channels ic oc))
