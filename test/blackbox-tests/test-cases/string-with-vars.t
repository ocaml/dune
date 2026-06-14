Tests for expansions of the `String_with_vars.t` type

  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name foo)
  >  (allow_empty))
  > EOF

Expanding a pform with multiple values:
  $ cat > dune <<EOF
  > (rule
  >  (targets foo.txt bar.txt)
  >  (action
  >   (progn
  >    (run touch bar.txt)
  >    (with-stdout-to foo.txt
  >     (progn
  >      (run echo %{targets})
  >      (run echo "%{targets}")
  >      (run echo before%{targets}after)
  >      (run echo "before%{targets}after")
  >      (run echo %{targets}%{targets})
  >      (run echo "%{targets}%{targets}"))))))
  > EOF

  $ dune build foo.txt
  $ cat _build/default/foo.txt
  ./foo.txt ./bar.txt
  ./foo.txt ./bar.txt
  before./foo.txt./bar.txtafter
  before./foo.txt ./bar.txtafter
  ./foo.txt./bar.txt./foo.txt./bar.txt
  ./foo.txt ./bar.txt./foo.txt ./bar.txt
