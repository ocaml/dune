Show multiple module systems fighting for the same targets

When `(using melange 0.1)`, promoted targets will be in their `target/`
directories, so there's no overlap

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target target_esm)
  >  (emit_stdlib false)
  >  (module_systems (esm js))
  >  (compile_flags :standard --mel-no-version-header)
  >  (promote (until-clean))
  >  (libraries melange.js))
  > 
  > (melange.emit
  >  (target target_commonjs)
  >  (emit_stdlib false)
  >  (module_systems (commonjs js))
  >  (compile_flags :standard --mel-no-version-header)
  >  (promote (until-clean))
  >  (libraries melange.js))
  > EOF

  $ dune build

  $ cat target_esm/node_modules/melange.js/caml_string.js | head -n3
  
  
  import * as Caml_js_exceptions from "melange.js/caml_js_exceptions.js";

  $ cat target_commonjs/node_modules/melange.js/caml_string.js | head -n3
  'use strict';
  
  const Caml_js_exceptions = require("melange.js/caml_js_exceptions.js");

  $ dune clean

Switching to `(using melange 1.0)` overwrites files in `node_modules/`. Last
write wins

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (using melange 1.0)
  > EOF

  $ dune build
  $ cat node_modules/melange.js/caml_string.js | head -n3
  'use strict';
  
  const Caml_js_exceptions = require("melange.js/caml_js_exceptions.js");

  $ dune clean

Only one can be promoted:

  $ cat > dune <<EOF
  > (melange.emit
  >  (target target_esm)
  >  (emit_stdlib false)
  >  (module_systems (esm js))
  >  (compile_flags :standard --mel-no-version-header)
  >  (promote (until-clean))
  >  (libraries melange.js))
  > 
  > (melange.emit
  >  (target target_commonjs)
  >  (emit_stdlib false)
  >  (module_systems (commonjs js))
  >  (compile_flags :standard --mel-no-version-header)
  >  (libraries melange.js))
  > EOF

  $ dune build
  $ cat node_modules/melange.js/caml_string.js | head -n3
  
  
  import * as Caml_js_exceptions from "melange.js/caml_js_exceptions.js";
