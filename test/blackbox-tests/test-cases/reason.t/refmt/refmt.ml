let sprintf = Printf.sprintf

type ('impl, 'intf) intf_or_impl =
  | Impl of 'impl
  | Intf of 'intf

module File = struct
  let of_filename s =
    if Filename.check_suffix s ".re" then
      Impl s
    else if Filename.check_suffix s ".rei" then
      Intf s
    else
      failwith (sprintf "unknown filename %S" s)

  let output_fn = function
    | Impl fn -> fn ^ ".ml"
    | Intf fn -> fn ^ ".mli"
end

let () =
  let set_binary = function
    | "binary" -> ()
    | _ -> failwith "Only the value 'binary' is allowed for --print"
  in
  let args =
    [ "--print", Arg.String set_binary, ""
    ]
  in
  let source = ref None in
  let anon s =
    match !source with
    | None -> source := Some s
    | Some _ -> failwith "source may be set only once"
  in
  Arg.parse args anon "";
  let source =
    match !source with
    | None -> failwith "source file isn't set"
    | Some s -> s
  in
  let ic = open_in source in
  let source_file = File.of_filename source in
  let out_fn = File.output_fn source_file in
  let out = open_out_bin out_fn in
  let rec loop () =
    match input_char ic with
    | exception End_of_file -> ()
    | s -> output_char out s; loop ()
  in
  loop ();
  close_out_noerr out
