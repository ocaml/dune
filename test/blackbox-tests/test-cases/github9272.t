Repro for #9272: when an executable that depends on dune-site is promoted to
the source tree, the executable in the source tree segfaults.

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > (using dune_site 0.1)
  > EOF

  $ cat > dune << EOF
  > (executable
  >  (name hello)
  >  (promote (until-clean))
  >  (libraries dune-site))
  > EOF

  $ touch hello.ml

  $ dune build

Test peculiarity: we can not do ./hello.exe directly, because the shell itself
(that runs the cram test) will display the pid of the crashing process.
Instead, we start and wait for it in an OCaml program and only display the
code.

Once the bug is fixed, this can be replaced by just `./hello.exe` and the test
can be enabled for all systems.

  $ cat > exec.ml << EOF
  > let () =
  >   let pid =
  >     Unix.create_process
  >       "./hello.exe" [|"./hello.exe"|]
  >       Unix.stdin Unix.stdout Unix.stderr
  >   in
  >   match Unix.waitpid [] pid with
  >   | _, WEXITED n -> Printf.printf "WEXITED %d" n
  >   | _, WSTOPPED n -> Printf.printf "WSTOPPED %d" n
  >   | _, WSIGNALED n -> Printf.printf "WSIGNALED %d" n
  > EOF

  $ ocaml -I +unix unix.cma exec.ml
  WEXITED 0
