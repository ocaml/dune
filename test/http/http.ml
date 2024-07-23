open Stdune

module Server = struct
  type t =
    { sock : Unix.file_descr
    ; addr : Unix.sockaddr
    }

  type session = in_channel * out_channel

  let close_session (_, out) = Out_channel.close out

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

  let accept_request (in_, _) =
    let rec loop () =
      match In_channel.input_line in_ with
      | Some "\r" | None -> ()
      | Some _ -> loop ()
    in
    loop ()
  ;;

  let auto_shutdown_seconds =
    match Sys.getenv_opt "DUNE_WEBSERVER_TIMEOUT" with
    | None -> 5.
    | Some s -> Float.of_string s |> Option.value_exn
  ;;

  let accept t ~f =
    let descr, _sockaddr =
      let read_fds, _write_fds, _excpt_fds =
        Unix.select [ t.sock ] [] [] auto_shutdown_seconds
      in
      match read_fds with
      | _ :: _ -> Unix.accept ~cloexec:true t.sock
      | [] ->
        Format.eprintf "Exiting after timeout@.";
        failwith "timeout"
    in
    let out = Unix.out_channel_of_descr descr in
    let in_ = Unix.in_channel_of_descr descr in
    let session = in_, out in
    Exn.protect ~f:(fun () -> f session) ~finally:(fun () -> close_session session)
  ;;

  let stop t = Unix.close t.sock

  let respond (_, out) ~status ~content_length =
    let status =
      match status with
      | `Ok -> "200 OK"
      | `Not_found -> "404 Not Found"
    in
    Printf.fprintf
      out
      "HTTP/1.1 %s\r\nConnection: close\r\nContent-Length: %Ld\r\n\r\n%!"
      status
      content_length
  ;;

  let respond_file session ~file =
    In_channel.with_open_bin file (fun chan ->
      let content_length = In_channel.length chan in
      respond session ~status:`Ok ~content_length;
      let bytes = Bytes.create 65536 in
      let to_write = ref (Int64.to_int content_length) in
      let out = snd session in
      let rec loop () =
        let size = In_channel.input chan bytes 0 (Bytes.length bytes) in
        if size > 0
        then (
          to_write := !to_write - size;
          Out_channel.output out bytes 0 size;
          loop ())
      in
      loop ();
      assert (!to_write = 0);
      Out_channel.flush out)
  ;;

  let respond session ~status ~content =
    let content_length = Int64.of_int (String.length content) in
    respond session ~status ~content_length;
    let out = snd session in
    Out_channel.output_string out content;
    Out_channel.flush out
  ;;
end
