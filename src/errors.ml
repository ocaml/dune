open Stdune

exception Already_reported

let err_buf = Buffer.create 128
let err_ppf = Format.formatter_of_buffer err_buf
let kerrf fmt ~f =
  Format.kfprintf
    (fun ppf ->
       Format.pp_print_flush ppf ();
       let s = Buffer.contents err_buf in
       Buffer.clear err_buf;
       f s)
    err_ppf fmt

let die fmt =
  kerrf fmt ~f:(fun s -> raise (Exn.Fatal_error s))
