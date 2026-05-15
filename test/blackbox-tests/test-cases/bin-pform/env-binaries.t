%{bin:...} for a binary added via (env (binaries ...)).

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > EOF
  $ cat >dune <<'EOF'
  > (executable (name mybin))
  > (env (_ (binaries (mybin.exe as myothername))))
  > (rule
  >  (deps %{bin:myothername})
  >  (action
  >   (progn
  >    (with-stdout-to bin-path
  >     (echo %{bin:myothername}))
  >    (with-stdout-to path-output
  >     (bash "echo $PATH")))))
  > EOF
  $ cat >mybin.ml <<'EOF'
  > let () = print_endline "hello"
  > EOF

  $ dune build bin-path

The pform resolves to the .bin/ symlink:

  $ cat _build/default/bin-path
  .bin/myothername

The rule depends on the .bin/ symlink:

  $ dune rules --format=json _build/default/bin-path \
  >   | jq 'include "dune"; .[] | ruleDepFilePaths'
  "_build/default/.bin/myothername"

The action's PATH gets the .bin/ symlink directory:

  $ env_added "$(cat _build/default/path-output)" "$PATH"
  $TESTCASE_ROOT/_build/default/.bin
