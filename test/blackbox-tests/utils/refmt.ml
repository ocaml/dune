type kind =
  | Impl
  | Intf

type mode =
  | Format
  | Print_binary
  | Parse_binary

let kind_of_filename s = if String.ends_with ~suffix:".rei" s then Intf else Impl

let read_all file =
  let ic = open_in_bin file in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))
;;

let write_stdout file = output_string stdout (read_all file)

let write_binary_ast source kind =
  let source =
    if Filename.is_relative source then Filename.concat (Sys.getcwd ()) source else source
  in
  let tmp = Filename.temp_file "refmt" ".ast" in
  let ic = open_in source in
  Fun.protect
    ~finally:(fun () ->
      close_in_noerr ic;
      Sys.remove tmp)
    (fun () ->
       Location.input_name := source;
       let lb = Lexing.from_channel ic in
       Location.init lb source;
       (match kind with
        | Impl -> Pparse.write_ast Structure tmp (Parse.implementation lb)
        | Intf -> Pparse.write_ast Signature tmp (Parse.interface lb));
       write_stdout tmp)
;;

let write_parsed_binary_ast source kind =
  let ppf = Format.std_formatter in
  (match kind with
   | Impl -> Pparse.read_ast Structure source |> Pprintast.structure ppf
   | Intf -> Pparse.read_ast Signature source |> Pprintast.signature ppf);
  Format.pp_print_flush ppf ()
;;

let () =
  let mode = ref Format in
  let set_binary = function
    | "binary" -> ()
    | _ -> failwith "Only the value 'binary' is allowed for --parse / --print"
  in
  let args =
    [ ( "--print"
      , Arg.String
          (fun s ->
            set_binary s;
            mode := Print_binary)
      , "" )
    ; ( "--parse"
      , Arg.String
          (fun s ->
            set_binary s;
            mode := Parse_binary)
      , "" )
    ; "-i", Arg.Bool ignore, ""
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
  let kind = kind_of_filename source in
  match !mode with
  | Format -> write_stdout source
  | Parse_binary -> write_parsed_binary_ast source kind
  | Print_binary -> write_binary_ast source kind
;;
