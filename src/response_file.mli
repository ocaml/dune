(** Automatically create response files *)

open Import

(** If the list of arguments is too long and [arg_name] is [Some a],
    setup a rule to dump it to a response file [x] and return:

    {[
      S [A a_name; Dep x]
    ]}

    otherwise, return the argument specification unmodified. [key] is
    just any string that is used to choose a unique response file name,
    in case several response files are needed in the same directory.

    [dump_response_file] is used to create the contents of the response
    file from a list of strings.

    [exec_dir] must be the directory where the final command will be executed.
    [response_file_dir] is the directory where the command will be executed.
*)
val process
  :  Super_context.t
  -> key:string
  -> exec_dir:Path.t
  -> response_file_dir:Path.t
  -> arg_name:string option
  -> dump_response_file:(string list -> string)
  -> Arg_spec.Simple.t
  -> _ Arg_spec.t

(** Same [process] but specialized to calls to the OCaml compiler. *)
val process_ocaml_call
  :  Super_context.t
  -> key:string
  -> exec_dir:Path.t
  -> response_file_dir:Path.t
  -> Arg_spec.Simple.t
  -> _ Arg_spec.t
