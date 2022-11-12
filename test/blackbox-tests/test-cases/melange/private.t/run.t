Test the private libs flow when using `melange.emit` stanza

Cmj rules should include --bs-package-output
  $ dune rules inside/app/.app.objs/melange/app.cmj | 
  > grep -e "--bs-package-output" --after-context=1 
      --bs-package-output
      inside/app

Cmj rules should not include --bs-package-name
  $ dune rules inside/app/.app.objs/melange/app.cmj | 
  > grep -ce "--bs-package-name"
  0
  [1]

  $ output=inside/output

Js rules should include module type
  $ dune rules $output/inside/app/app__B.js | 
  > grep -e "--bs-module-type" --after-context=1 
      --bs-module-type
      es6

Build js files
  $ dune build $output/inside/melange__C.js
  $ node _build/default/$output/inside/melange__C.js
  buy it
