Test virtual lib in an exe / melange environment

  $ dune build @melange
  $ output=_build/default/mel.js
  $ test -f "$output" && node "$output"
  Hello from melange
