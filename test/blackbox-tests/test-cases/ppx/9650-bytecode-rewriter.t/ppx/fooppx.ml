let () =
  Ppxlib.Driver.register_transformation
    "linter"
    ~impl:(fun s -> Format.eprintf "running rewriter@.%!"; s)
