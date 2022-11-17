Test virtual lib in an exe / melange environment

Executable implementation works fine

  $ dune build native.exe
  $ dune exec ./native.exe
  Hello from native

Melange implementation works as well

  $ dune build output/mel.js
  $ node _build/default/output/mel.js
  Hello from melange
