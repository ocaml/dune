Test for a custom context in dune-workspace with a different set of dependencies
than the default one.

We first create mock “compiler stacks”: one for 5.2.0 and one for 5.3.0. Each
ocaml package depends on a matching ocaml-base-compiler, which depends on a
matching ocaml-compiler, mimicking how the compiler packages are layered in
opam. We also define ocaml-option-tsan.0.0.1 as a compiler option package that
forces ocaml = 5.2.0, this gives us an option that can “pull” the solver toward
the 5.2.0 compiler stack when enabled.

  $ mkrepo
  $ mkpkg ocaml 5.2.0 <<EOF
  > depends: [ "ocaml-base-compiler" {= "5.2.0"} ]
  > EOF
  $ mkpkg ocaml-base-compiler 5.2.0 <<EOF
  > depends: [ "ocaml-compiler" {= "5.2.0"} ]
  > EOF
  $ mkpkg ocaml-compiler 5.2.0 <<EOF
  > depopts: ["ocaml-option-tsan"]
  > EOF
  $ mkpkg ocaml 5.3.0 <<EOF
  > depends: [ "ocaml-base-compiler" {= "5.3.0"} ]
  > EOF
  $ mkpkg ocaml-base-compiler 5.3.0 <<EOF
  > depends: [ "ocaml-compiler" {= "5.3.0"} ]
  > EOF
  $ mkpkg ocaml-compiler 5.3.0
  $ mkpkg ocaml-option-tsan 0.0.1 <<EOF
  > depends: [ "ocaml" {= "5.2.0" & post} ]
  > EOF

  $ add_mock_repo_if_needed

Next we create a minimal dune-project whose only dependency is ocaml. With no
additional constraints or optional dependencies enabled, `dune pkg lock` solves
the newest available ocaml, so the default lock directory ends up with the 5.3.0
compiler stack.

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (package
  >  (name tsan-check)
  >  (allow_empty)
  >  (depends ocaml))
  > EOF

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - ocaml.5.3.0
  - ocaml-base-compiler.5.3.0
  - ocaml-compiler.5.3.0

We add a special context `dune-new.lock` with dependency on ocaml-tsan, which in
turn depends on ocaml 5.2.0. This should force the 5.2.0 stack to be solved.

  $ cat >> dune-workspace <<EOF
  > (lock_dir
  >  (path dune-new.lock)
  >  (repositories mock)
  >  (constraints
  >    (ocaml-option-tsan (= 0.0.1)))
  >  (depopts ocaml-option-tsan))
  > 
  > (context
  >  (default
  >   (lock_dir dune.lock)))
  > 
  > (context
  >  (default
  >   (name new)
  >   (lock_dir dune-new.lock)))
  > EOF

  $ dune pkg lock dune-new.lock
  Solution for dune-new.lock
  
  Dependencies common to all supported platforms:
  - ocaml.5.2.0
  - ocaml-base-compiler.5.2.0
  - ocaml-compiler.5.2.0
  - ocaml-option-tsan.0.0.1

Finally, we declare an action that prints out its current context to show
building in different contexts.

  $ cat > dune <<'EOF'
  > (rule
  >  (target built-by.txt)
  >  (action
  >   (with-stdout-to %{target}
  >    (echo "built in context: %{context_name}"))))
  > EOF


  $ dune build _build/default/built-by.txt _build/new/built-by.txt
  $ cat _build/default/built-by.txt
  built in context: default

  $ cat _build/new/built-by.txt
  built in context: new
