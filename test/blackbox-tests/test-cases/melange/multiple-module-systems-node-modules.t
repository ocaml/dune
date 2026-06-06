Show multiple module systems fighting for the same targets

When `(using melange 0.1)`, promoted targets will be in their `target/`
directories, so there's no overlap

  $ make_melange_project 3.22 0.1

  $ write_module_systems_dune() {
  > local promote_commonjs="$1"
  > cat > dune <<'EOF'
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
  > EOF
  > if [ "$promote_commonjs" = "yes" ]; then
  > cat >> dune <<'EOF'
  >  (promote (until-clean))
  > EOF
  > fi
  > cat >> dune <<'EOF'
  >  (libraries melange.js))
  > EOF
  > }

  $ write_module_systems_dune yes

  $ dune build

  $ cat target_esm/node_modules/melange.js/caml_string.js | head -n3
  
  
  import * as Caml_js_exceptions from "melange.js/caml_js_exceptions.js";

  $ cat target_commonjs/node_modules/melange.js/caml_string.js | head -n3
  'use strict';
  
  const Caml_js_exceptions = require("melange.js/caml_js_exceptions.js");

  $ dune clean

Switching to `(using melange 1.0)` overwrites files in `node_modules/`. Last
write wins

  $ make_melange_project 3.22 1.0

  $ dune build
  $ cat node_modules/melange.js/caml_string.js | head -n3
  'use strict';
  
  const Caml_js_exceptions = require("melange.js/caml_js_exceptions.js");

  $ dune clean

Only one can be promoted:

  $ write_module_systems_dune no

  $ dune build
  $ cat node_modules/melange.js/caml_string.js | head -n3
  
  
  import * as Caml_js_exceptions from "melange.js/caml_js_exceptions.js";
