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

  $ cat _build/default/file
  8.15.2

  (>= 8.15.0-x12gn1 8.15.1) => false
