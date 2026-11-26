Reproduce #10839.

Dune file in OCaml syntax and a files directory should work

  $ make_lockdir

  $ make_lockpkg base-bytes <<EOF
  > (version base)
  > 
  > (depends ocamlfind)
  > EOF

  $ make_lockpkg ocamlfind <<EOF
  > (version 1)
  > EOF

  $ make_dune_project 3.16

  $ cat >dune <<EOF
  > (* -*- tuareg -*- *)
  > let () = Jbuild_plugin.V1.send ""
  > EOF

  $ dune build

  $ mkdir ${default_lock_dir}/ocamlfind.files
  $ touch ${default_lock_dir}/ocamlfind.files/foo.patch

  $ dune build

A native dependency's OCaml-syntax Dune file must be evaluated with the locked
compiler. Restored from the mounted prototype: the opaque recipe must not run.
The mock compiler is local to each workspace, not a user-wide toolchain install.
Its toplevel wrapper leaves a marker to prove it evaluated the script.

  $ mkdir compiler-for-script
  $ cd compiler-for-script
  $ make_named_package_project workspace 3.25 scripted
  $ mkdir _scripted
  $ (cd _scripted && make_dune_project_with_package 3.25 scripted)
  $ cat > _scripted/dune <<'EOF'
  > (* -*- tuareg -*- *)
  > let () = Jbuild_plugin.V1.send {|
  > (rule (target result) (action (write-file %{target} evaluated)))
  > (install (package scripted) (section share) (files result))
  > |}
  > EOF
  $ make_lockdir
  $ echo '(ocaml script-compiler)' >> dune.lock/lock.dune
  $ mkdir _compiler
  $ cat > _compiler/ocaml <<EOF
  > #!/bin/sh
  > echo locked-compiler > "$PWD/compiler-used"
  > exec "$(command -v ocaml)" "\$@"
  > EOF
  $ chmod +x _compiler/ocaml
  $ make_lockpkg script-compiler <<EOF
  > (version 1.0)
  > (source (copy $PWD/_compiler))
  > (install
  >  (progn
  >   (system "mkdir -p %{bin}")
  >   (run cp $(command -v ocamlc) ocaml %{bin})))
  > EOF
  $ make_lockpkg scripted <<EOF
  > (version 1.0)
  > (depends script-compiler)
  > (source (copy $PWD/_scripted))
  > (build (run false))
  > EOF
  $ dune build @pkg-install
  $ cat _build/default/.pkg/scripted/result
  evaluated
  $ cat compiler-used
  locked-compiler

The workspace Dune file itself may also need the locked compiler. Its rules
must not be needed to materialize that compiler for the first time.

  $ mkdir ../workspace-script
  $ cd ../workspace-script
  $ make_dune_project 3.25
  $ cat > dune <<'EOF'
  > (* -*- tuareg -*- *)
  > let () = Jbuild_plugin.V1.send {|
  > (rule (target result) (action (write-file %{target} workspace-evaluated)))
  > |}
  > EOF
  $ make_lockdir
  $ echo '(ocaml script-compiler)' >> dune.lock/lock.dune
  $ cp ../compiler-for-script/dune.lock/script-compiler.pkg dune.lock/
  $ dune build result
  $ cat _build/default/result
  workspace-evaluated
