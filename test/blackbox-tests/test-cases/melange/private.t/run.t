Test the private libs flow when using `melange.emit` stanza

Cmj rules should include --bs-package-output
  $ dune rules inside/app/.app.objs/melange/app.cmj | 
  > grep -e "--bs-package-output" --after-context=1 
      --bs-package-output
      _build/default/inside/app

Cmj rules should not include --bs-package-name
  $ dune rules inside/app/.app.objs/melange/app.cmj | 
  > grep -ce "--bs-package-name"
  0
  [1]

Js rules should include module type
  $ dune rules inside/output/app/app__B.js | 
  > grep -e "--bs-module-type" --after-context=1 
      --bs-module-type
      es6

Build js files
  $ dune build inside/output/app/app__B.js
  $ node _build/default/inside/output/app/app__B.js
  buy it
