Test copy_files

For the executable copy_files work fine

  $ dune build native.exe
  $ dune exec ./native.exe
  Hello world

For melange it works as well

  $ dune build output/mel.js
  $ node _build/default/output/mel.js
  Hello world
