let load filename =
  let buf = Buffer.create 16 in
  let ppf = Format.formatter_of_buffer buf in
  match Toploop.load_file ppf filename with
  | true -> ()
  | false ->
    Format.pp_print_flush ppf ();
    failwith
    @@ Format.asprintf "Failed to load file `%s': %s" filename
         (Buffer.contents buf)
