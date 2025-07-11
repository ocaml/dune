Test that configurator always picks the value of the `c_libraries`
flag from `ocamlc -config`. If not, there's a failure to link a
configuration test program that uses functions from these libraries.
For that, we need functions outside of libc. On Unix, that would be
`sin(3)` that requires the `-lm` flag for the math library, and on
Windows `gethostname` that requires WinSock2 (`ws2_32.dll`).

link successfully
==================================

  $ dune exec -- ./discover.exe
  $ cat out
  1
