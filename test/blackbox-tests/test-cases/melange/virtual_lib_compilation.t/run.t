Test virtual lib in an exe / melange environment

The native build passes

  $ dune exec ./ml.exe
  Hello from ml

Any module requiring a virtual module (including modules within the virtual
library itself) needs to consult the `.cmj` file for the concrete
implementation being seleced to know where to `import` from in the generated
JS. The following build works because Dune tracks concrete implementation
`.cmj` files as dependencies of the JS rules.

  $ dune build @mel

  $ output=_build/default/output/mel.js
  $ test -f "$output" && node "$output"
  Hello from melange

