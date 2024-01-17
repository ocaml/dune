open Stdune

module Server = struct
  type t =
    { sock : Unix.file_descr
    ; addr : Unix.sockaddr
    }

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
    let descr, _sockaddr = Unix.accept ~cloexec:true t.sock in
    let out = Unix.out_channel_of_descr descr in
    f out;
    close_out out
  ;;

  let stop t = Unix.close t.sock

  let respond out ~status ~content =
    let status =
      match status with
      | `Ok -> "200 Ok"
      | `Not_found -> "404 Not Found"
    in
    let content_length = String.length content in
    Printf.fprintf out "HTTP/1.1 %s\nContent-Length: %d\n\n" status content_length;
    Out_channel.output_string out content
  ;;

  let respond_file out ~file =
    let content = Io.String_path.read_file ~binary:true file in
    respond out ~status:`Ok ~content
  ;;
end
