exception Fatal_error of string
exception Code_error of string
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

let code_errorf fmt =
  kerrf fmt ~f:(fun s -> raise (Code_error s))

let die fmt =
  kerrf fmt ~f:(fun s -> raise (Fatal_error s))
