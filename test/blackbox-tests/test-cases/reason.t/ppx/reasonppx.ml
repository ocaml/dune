let lint = ref false

let () =
  Ppxlib.Driver.add_arg "-lint" (Arg.Bool (fun l -> lint := l)) ~doc:"";
  Ppxlib.Driver.register_transformation
    "reasonppx"
    ~impl:(fun s ->
       if !lint then (
         exit 0
       ) else (
         s
       )
    )
