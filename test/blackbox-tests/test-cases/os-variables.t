Demonstrate the following variables: %{os}, %{os_version}, %{os_distribution}, %{os_family}

  $ cat >dune-project <<EOF
  > (lang dune 3.20)
  > EOF

  $ cat >dune <<EOF
  > (rule (write-file out "%{os}\n%{os_version}\n%{os_distribution}\n%{os_family}\n"))
  > EOF

  $ dune build out

The values are not going to be portable, so we just count how many we get:

  $ grep -c '.' _build/default/out
  4
