Testing %{arch_sixtyfour} in enabled_if

  $ cat > dune-project << EOF
  > (lang dune 3.11)
  > EOF

  $ dune exec -- ./hello.exe
  Hello, World!

Testing the version guard

  $ cat > dune-project << EOF
  > (lang dune 3.10)
  > EOF

  $ dune exec -- ./hello.exe
  File "dune", line 4, characters 5-22:
  4 |   (= %{arch_sixtyfour} %{arch_sixtyfour})))
           ^^^^^^^^^^^^^^^^^
  Error: %{arch_sixtyfour} is only available since version 3.11 of the dune
  language. Please update your dune-project file to have (lang dune 3.11).
  [1]
