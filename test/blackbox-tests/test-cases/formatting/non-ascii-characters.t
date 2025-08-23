Utf8 characters are handled for now, this is also related to the issue #9728

  $ dune format-dune-file <<EOF
  > ("Éﬀ ĎúÑȨ")
  > EOF
  ("Éﬀ ĎúÑȨ")

  $ dune format-dune-file <<EOF
  > (run foo %{bin:é})
  > EOF
  File "", line 1, characters 15-16:
  Error: The character '\195' is not allowed inside %{...} forms
  [1]

  $ dune format-dune-file <<EOF
  > (echo "hÉllo")
  > EOF
  (echo "hÉllo")

  $ dune format-dune-file <<EOF
  > (echo "É")
  > EOF
  (echo "É")

  $ dune format-dune-file <<EOF
  > (Écho "hello")
  > EOF
  File "", line 1, characters 1-1:
  Error: Invalid . file
  [1]

  $ bash -c "printf '(echo \"%b\")' '\xc0'"| dune format-dune-file
  (echo "\192")
  $ bash -c "printf '(echo \"%b\")' '\xf0'"| dune format-dune-file
  (echo "\240")
