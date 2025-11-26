Exercise differences between the behavior of `dune fmt` when a lockdir is
present and a lockdir is absent.

  $ . ./helpers.sh
  $ mkrepo
  $ make_project_with_dev_tool_lockdir

Make a fake ocamlformat package that appends a comment to the end of its input.
  $ mkpkg ocamlformat <<EOF
  > install: [
  >   [ "sh" "-c" "echo '#!/bin/sh' > %{bin}%/ocamlformat" ]
  >   [ "sh" "-c" "echo 'cat \$2' >> %{bin}%/ocamlformat" ]
  >   [ "sh" "-c" "echo 'echo \$2 | grep .*.ml >/dev/null && echo \"(* formatted with fake ocamlformat *)\"' >> %{bin}%/ocamlformat" ]
  >   [ "sh" "-c" "chmod a+x %{bin}%/ocamlformat" ]
  > ]
  > EOF

The foo package depends on the bar package.
  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package
  >  (name foo)
  >  (depends bar))
  > EOF

The foo executable depends on the bar library.
  $ cat > dune <<EOF
  > (executable
  >  (public_name foo)
  >  (libraries bar))
  > EOF

Without a .ocamlformat `dune fmt` does nothing.
  $ touch .ocamlformat

Run `dune fmt` before creating a lockdir, and print the file foo.ml before and
after to demonstrate that it was formatted. Note that the package "bar" hasn't
yet been defined, so the fact that `dune fmt` works indicates that dune did not
attempt to build the package "foo".
  $ cat foo.ml
  let () = print_endline "Hello, world"
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt
  Solution for _build/.dev-tools.locks/ocamlformat:
  - ocamlformat.0.0.1
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  Promoting _build/default/.formatted/foo.ml to foo.ml.
  [1]
  $ cat foo.ml
  let () = print_endline "Hello, world"
  (* formatted with fake ocamlformat *)

Create a lockdir and define the package "bar". Note its install command is
`false` so it will fail to install.
  $ make_lockdir
  $ make_lockpkg bar <<EOF
  > (version 0.0.1)
  > (install (run false))
  > EOF

Now run `dune fmt` again. It attempts to build the project and its
dependencies, and fails to install the dependency "bar".
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt
  File "dune.lock/bar.pkg", line 2, characters 14-19:
  2 | (install (run false))
                    ^^^^^
  Error: Logs for package bar
  
  [1]
