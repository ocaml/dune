(* An http server which serves the contents of a given file a single time and
   then terminates *)

open Stdune

module Args = struct
  type t =
    { content_files : string list
    ; port_file : string
    ; simulate_not_found : bool
    }

  let parse () =
    let content_files = ref [] in
    let port_file = ref "" in
    let simulate_not_found = ref false in
    let specs =
      [ ( "--content-file"
        , Arg.String (fun file -> content_files := file :: !content_files)
        , "File to serve" )
      ; ( "--port-file"
        , Arg.Set_string port_file
        , "The server will write its port number to this file" )
      ; "--simulate-not-found", Arg.Set simulate_not_found, "Return a 404 page"
      ]
    in
    Arg.parse
      specs
      (fun _anon_arg -> failwith "unexpected anonymous argument")
      "Run a webserver on a random port which serves the contents of a  single file a \
       single time, then terminates.";
    { content_files = List.rev !content_files
    ; port_file = !port_file
    ; simulate_not_found = !simulate_not_found
    }
  ;;
end

let main content_files port_file ~simulate_not_found =
  let host = Unix.inet_addr_loopback in
  let addr = Unix.ADDR_INET (host, 0) in
  let server = Http.Server.make addr in
  Http.Server.start server;
  let port = Http.Server.port server in
  (* Create the port file immediately before starting the server. This way
     clients can use the existence of the port file to know roughly when the
     server is ready to accept connections. Note that there is technically a
     small delay between creating the port file and the server being ready
     which we can remove if it ends up causing us problems. *)
  Out_channel.with_open_text port_file (fun out_channel ->
    Out_channel.output_string out_channel (Printf.sprintf "%d\n" port));
  List.iter content_files ~f:(fun content_file ->
    Http.Server.accept server ~f:(fun out_channel ->
      if simulate_not_found
      then Http.Server.respond out_channel ~status:`Not_found ~content:""
      else Http.Server.respond_file out_channel ~file:content_file))
;;

let () =
  let { Args.content_files; port_file; simulate_not_found } = Args.parse () in
  main content_files port_file ~simulate_not_found
;;
