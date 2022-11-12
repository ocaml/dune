Test the public libs flow when using `melange.emit` stanza

Cmj rules should include --bs-package-output
  $ dune rules my_project/app/.app.objs/melange/app.cmj | 
  > grep -e "--bs-package-output" --after-context=1 
      --bs-package-output
      my_project/app

Cmj rules should include --bs-package-name
  $ dune rules my_project/app/.app.objs/melange/app.cmj | 
  > grep -e "--bs-package-name" --after-context=1 
      --bs-package-name
      pkg

  $ output=my_project/output

Js rules should include --bs-module-type
  $ dune rules $output/my_project/app/app__B.js | 
  > grep -e "--bs-module-type" --after-context=1 
      --bs-module-type
      commonjs

Js rules should include --bs-package-name
  $ dune rules $output/my_project/app/app__B.js | 
  > grep -e "--bs-package-name" --after-context=1 
      --bs-package-name
      pkg

Build js files
  $ dune build $output/my_project/melange__C.js

Path to app_B is non-relative (broken)
  $ node _build/default/$output/my_project/melange__C.js
  buy it
