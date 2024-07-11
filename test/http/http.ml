open Stdune

module Server = struct
  type t =
    { sock : Unix.file_descr
    ; addr : Unix.sockaddr
    }

  let auto_shutdown_seconds = 30.

  type session = out_channel

  let make addr =
    let sock = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
    { sock; addr }
  ;;

  let port t =
    match Unix.getsockname t.sock with
    | Unix.ADDR_INET (_, port) -> port
    | ADDR_UNIX _ -> failwith "no port defined"
  ;;

  let start t =
    Unix.setsockopt t.sock Unix.SO_REUSEADDR true;
    Unix.bind t.sock t.addr;
    Unix.listen t.sock 1
  ;;

  let accept t ~f =
    let read_fds, _write_fds, _excpt_fds =
      Unix.select [ t.sock ] [] [] auto_shutdown_seconds
    in
    match read_fds with
    | [] ->
      Printf.eprintf
        "Exiting automatically due to reaching %.0fs timeout without any connection\n"
        auto_shutdown_seconds
    | sock :: _always_empty ->
      let descr, _sockaddr = Unix.accept ~cloexec:true sock in
      let out = Unix.out_channel_of_descr descr in
      f out;
      Out_channel.flush out;
      Unix.shutdown descr Unix.SHUTDOWN_SEND;
      close_out out
  ;;

  let stop t = Unix.close t.sock

  let header status mime content_length =
    let status_string =
      match status with
      | `Ok -> "200 Ok"
      | `Not_found -> "404 Not Found"
    in
    let mime_string =
      match mime with
      | `Text -> "text/plain"
      | `Binary -> "application/octet-stream"
    in
    sprintf
      "HTTP/1.1 %s\nContent-Type: %s\nContent-Length: %d\n\n"
      status_string
      mime_string
      content_length
  ;;

  let respond_not_found out = Out_channel.output_string out (header `Not_found `Text 0)

  (* Send a given number of bytes from a buffer to a file descriptor,
     retrying the send until the requested number of bytes have been
     sent. *)
  let send_bytes fd buf num_bytes_to_send =
    let total_bytes_sent = ref 0 in
    while !total_bytes_sent < num_bytes_to_send do
      let remaining_bytes_to_send = num_bytes_to_send - !total_bytes_sent in
      let bytes_sent = Unix.send fd buf !total_bytes_sent remaining_bytes_to_send [] in
      total_bytes_sent := !total_bytes_sent + bytes_sent
    done
  ;;

  let respond_file out ~file =
    In_channel.with_open_bin file (fun in_channel ->
      let length = In_channel.length in_channel |> Int64.to_int in
      let out_fd = Unix.descr_of_out_channel out in
      let header_bytes = Bytes.of_string (header `Ok `Binary length) in
      send_bytes out_fd header_bytes (Bytes.length header_bytes);
      let buf_size = 4096 in
      let buf = Bytes.create buf_size in
      let rec write () =
        match In_channel.input in_channel buf 0 buf_size with
        | 0 -> ()
        | bytes_read ->
          send_bytes out_fd buf bytes_read;
          write ()
      in
      write ())
  ;;
end
