let flag = ref false
let arg = ref ""

let () =
  Ppxlib.Driver.add_arg "-flag" (Arg.Set flag) ~doc:"";
  Ppxlib.Driver.add_arg "-arg" (Arg.Set_string arg) ~doc:"";
  Ppxlib.Driver.register_transformation
    "linter"
    ~impl:(fun s ->
       if not !flag then (
         Format.eprintf "pass -flag to fooppx@.%!";
         exit 1
       );
       if !arg = "" then (
         Format.eprintf "pass -arg to fooppx@.%!"
       );
       Format.eprintf "-arg: %s@." !arg;
       s
    )
