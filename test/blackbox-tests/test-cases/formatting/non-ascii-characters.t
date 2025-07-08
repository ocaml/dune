Utf8 characters are handled for now, this is also related to the issue #9728

  $ dune format-dune-file <<EOF
  > ("Éﬀ ĎúÑȨ")
  > EOF
  Warning: You are running 'dune format-dune-file' inside Dune. This may not
  give the expected results. It is recommended to use the '(format-dune-file)'
  action instead.
  ("Éﬀ ĎúÑȨ")

  $ dune format-dune-file <<EOF
  > (run foo %{bin:é})
  > EOF
  Warning: You are running 'dune format-dune-file' inside Dune. This may not
  give the expected results. It is recommended to use the '(format-dune-file)'
  action instead.
  File "", line 1, characters 15-16:
  Error: The character '\195' is not allowed inside %{...} forms
  [1]

  $ dune format-dune-file <<EOF
  > (echo "hÉllo")
  > EOF
  Warning: You are running 'dune format-dune-file' inside Dune. This may not
  give the expected results. It is recommended to use the '(format-dune-file)'
  action instead.
  (echo "hÉllo")

  $ dune format-dune-file <<EOF
  > (echo "É")
  > EOF
  Warning: You are running 'dune format-dune-file' inside Dune. This may not
  give the expected results. It is recommended to use the '(format-dune-file)'
  action instead.
  (echo "É")

  $ dune format-dune-file <<EOF
  > (Écho "hello")
  > EOF
  Warning: You are running 'dune format-dune-file' inside Dune. This may not
  give the expected results. It is recommended to use the '(format-dune-file)'
  action instead.
  File "", line 1, characters 1-1:
  Error: Invalid . file
  [1]

  $ bash -c "printf '(echo \"%b\")' '\xc0'"| dune format-dune-file
  Warning: You are running 'dune format-dune-file' inside Dune. This may not
  give the expected results. It is recommended to use the '(format-dune-file)'
  action instead.
  (echo "\192")
  $ bash -c "printf '(echo \"%b\")' '\xf0'"| dune format-dune-file
  Warning: You are running 'dune format-dune-file' inside Dune. This may not
  give the expected results. It is recommended to use the '(format-dune-file)'
  action instead.
  (echo "\240")
