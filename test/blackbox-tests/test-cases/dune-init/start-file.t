`dune init start-file` writes the standard helper file.

  $ dune init start-file
  Success: wrote start file to start/dune
  $ cat start/dune
  ;; To develop interactively, you are meant to start dune as:
  ;; $ dune build -w -alias start/build
  
  (alias
   (name build)
   (deps
    (alias_rec %{project_root}/check)
    ;; Targets should go here. A few common ones are commented out for your convenience:
    ;; (alias_rec %{project_root}/runtest)
    ;; (alias_rec %{project_root}/install)
    ;; (alias_rec %{project_root}/ocaml-index)
    ))

It also accepts an explicit directory path.

  $ dune init start-file local-dev
  Success: wrote start file to local-dev/dune
  $ cat local-dev/dune
  ;; To develop interactively, you are meant to start dune as:
  ;; $ dune build -w -alias start/build
  
  (alias
   (name build)
   (deps
    (alias_rec %{project_root}/check)
    ;; Targets should go here. A few common ones are commented out for your convenience:
    ;; (alias_rec %{project_root}/runtest)
    ;; (alias_rec %{project_root}/install)
    ;; (alias_rec %{project_root}/ocaml-index)
    ))

Running it again is a no-op when the file already matches.

  $ dune init start-file local-dev
  Success: wrote start file to local-dev/dune

It refuses to overwrite a different existing file.

  $ printf 'different\n' > local-dev/dune
  $ dune init start-file local-dev
  Error: Refusing to overwrite existing file local-dev/dune
  [1]
