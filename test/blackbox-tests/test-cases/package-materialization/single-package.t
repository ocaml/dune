(deps (package foo)) resolves to precise file-level deps in a materialized
install layout under _build/install/<context>/.packages/<digest>/.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name foo))
  > (package (name bar))
  > EOF

  $ mkdir src src2

  $ cat >src/dune <<EOF
  > (library (public_name foo))
  > EOF

  $ cat >src/mylib.ml <<EOF
  > let x = 1
  > EOF

  $ cat >src2/dune <<EOF
  > (library (public_name bar))
  > EOF

  $ cat >src2/mylib2.ml <<EOF
  > let y = 2
  > EOF

  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package foo))
  >  (action (with-stdout-to out1 (echo "ok"))))
  > (rule
  >  (deps (package foo) (package bar))
  >  (action (with-stdout-to out2 (echo "ok"))))
  > (rule
  >  (deps (package bar) (package foo))
  >  (action (with-stdout-to out_rev (echo "ok"))))
  > (rule
  >  (deps (package bar))
  >  (action (with-stdout-to out3 (echo "ok"))))
  > EOF

  $ dune build out1 out2 out_rev out3

Single package dep only includes that package:

  $ dune rules --format=json _build/default/out1 | jq 'include "dune"; .[] | ruleDepFilePaths' | censor | grep dune-package | sort
  "_build/install/default/.packages/$DIGEST/lib/foo/dune-package"

Two packages coalesce into a single layout directory:

  $ dune rules --format=json _build/default/out2 | jq 'include "dune"; .[] | ruleDepFilePaths' | censor | grep dune-package | sort
  "_build/install/default/.packages/$DIGEST/lib/bar/dune-package"
  "_build/install/default/.packages/$DIGEST/lib/foo/dune-package"

Order of package deps does not matter — same deps regardless of order:

  $ dune rules --format=json _build/default/out2 | jq 'include "dune"; .[] | ruleDepFilePaths' | sort > deps_fwd.txt
  $ dune rules --format=json _build/default/out_rev | jq 'include "dune"; .[] | ruleDepFilePaths' | sort > deps_rev.txt
  $ diff deps_fwd.txt deps_rev.txt

Different package set produces a different layout directory:

  $ dune rules --format=json _build/default/out3 | jq 'include "dune"; .[] | ruleDepFilePaths' | censor | grep dune-package | sort
  "_build/install/default/.packages/$DIGEST/lib/bar/dune-package"
