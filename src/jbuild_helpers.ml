open Import
open Sexp.Of_sexp

let invalid_module_name sexp =
  of_sexp_error sexp "invalid module name"

let module_name sexp =
  match string sexp with
  | "" -> invalid_module_name sexp
  | s ->
    if s.[0] = '_' then invalid_module_name sexp;
    String.iter s ~f:(function
      | 'A'..'Z' | 'a'..'z' | '_' -> ()
      | _ -> invalid_module_name sexp);
    String.capitalize_ascii s

let module_names sexp = String_set.of_list (list module_name sexp)

let invalid_lib_name sexp =
  of_sexp_error sexp "invalid library name"

let library_name sexp =
  match string sexp with
  | "" -> invalid_lib_name sexp
  | s ->
    if s.[0] = '.' then invalid_lib_name sexp;
    String.iter s ~f:(function
      | 'A'..'Z' | 'a'..'z' | '_' | '.' | '0'..'9' -> ()
      | _ -> invalid_lib_name sexp);
    s

let file sexp =
  match string sexp with
  | "." | ".." ->
    of_sexp_error sexp "'.' and '..' are not valid filenames"
  | fn -> fn

let file_in_current_dir sexp =
  match string sexp with
  | "." | ".." ->
    of_sexp_error sexp "'.' and '..' are not valid filenames"
  | fn ->
    if Filename.dirname fn <> Filename.current_dir_name then
      of_sexp_error sexp "file in current directory expected";
    fn