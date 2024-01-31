How the non-ASCII characters are handled, this is also related to the issue #9728

  $ dune format-dune-file <<EOF
  > ("É")
  > EOF
  ("\195\137")

  $ dune format-dune-file <<EOF
  > (run foo %{bin:é})
  > EOF
  > (run foo %{bin:é})
  File "", line 1, characters 15-16:
  Error: The character '\195' is not allowed inside %{...} forms
  run: not found
  [127]

  $ dune format-dune-file <<EOF
  > (= "É" "\195\137")
  > EOF
  (= "\195\137" "\195\137")
