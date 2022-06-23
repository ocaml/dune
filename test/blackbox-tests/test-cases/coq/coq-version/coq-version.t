  $ cat >dune-project <<EOF
  > (lang dune 3.4)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (action (with-stdout-to file (echo "8.15.2")))
  >  (enabled_if (= %{coq_version} "8.15.2")))
  > (rule
  >  (action (with-stdout-to file (echo "8.15.1")))
  >  (enabled_if (= %{coq_version} "8.15.1")))
  > EOF

  $ dune build ./file
  File "dune", line 3, characters 16-30:
  3 |  (enabled_if (= %{coq_version} "8.15.2")))
                      ^^^^^^^^^^^^^^
  Error: Unknown variable %{coq_version}
  [1]

  $ cat _build/default/file
  cat: _build/default/file: No such file or directory
  [1]
