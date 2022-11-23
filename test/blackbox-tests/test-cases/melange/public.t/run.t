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
  $ dune rules $output/my_project/app/b.js | 
  > grep -e "--bs-module-type" --after-context=1 
      --bs-module-type
      commonjs

Js rules should include --bs-package-name
  $ dune rules $output/my_project/app/b.js | 
  > grep -e "--bs-package-name" --after-context=1 
      --bs-package-name
      pkg

Build js files
  $ dune build $output/my_project/c.js

Path to app_B is non-relative (broken)
  $ node _build/default/$output/my_project/c.js
  buy it

  $ dune clean
  $ dune build @all
  File "my_project/app/dune", line 1, characters 0-78:
  1 | (library
  2 |  (name app)
  3 |  (public_name pkg.app)
  4 |  (libraries lib)
  5 |  (modes melange))
  Error: No rule found for my_project/app/.app.objs/byte/app.cmi
  File "my_project/app/dune", line 1, characters 0-78:
  1 | (library
  2 |  (name app)
  3 |  (public_name pkg.app)
  4 |  (libraries lib)
  5 |  (modes melange))
  Error: No rule found for my_project/app/.app.objs/byte/app.cmt
  File "my_project/app/dune", line 1, characters 0-78:
  1 | (library
  2 |  (name app)
  3 |  (public_name pkg.app)
  4 |  (libraries lib)
  5 |  (modes melange))
  Error: No rule found for my_project/app/.app.objs/byte/app__B.cmi
  File "my_project/app/dune", line 1, characters 0-78:
  1 | (library
  2 |  (name app)
  3 |  (public_name pkg.app)
  4 |  (libraries lib)
  5 |  (modes melange))
  Error: No rule found for my_project/app/.app.objs/byte/app__B.cmt
  File "my_project/app/dune", line 1, characters 0-78:
  1 | (library
  2 |  (name app)
  3 |  (public_name pkg.app)
  4 |  (libraries lib)
  5 |  (modes melange))
  Error: No rule found for my_project/app/.app.objs/byte/app__B.cmti
  File "my_project/lib/dune", line 1, characters 0-61:
  1 | (library
  2 |  (name lib)
  3 |  (public_name pkg.lib)
  4 |  (modes melange))
  Error: No rule found for my_project/lib/.lib.objs/byte/lib.cmi
  File "my_project/lib/dune", line 1, characters 0-61:
  1 | (library
  2 |  (name lib)
  3 |  (public_name pkg.lib)
  4 |  (modes melange))
  Error: No rule found for my_project/lib/.lib.objs/byte/lib.cmt
  File "my_project/lib/dune", line 1, characters 0-61:
  1 | (library
  2 |  (name lib)
  3 |  (public_name pkg.lib)
  4 |  (modes melange))
  Error: No rule found for my_project/lib/.lib.objs/byte/lib__A.cmi
  File "my_project/lib/dune", line 1, characters 0-61:
  1 | (library
  2 |  (name lib)
  3 |  (public_name pkg.lib)
  4 |  (modes melange))
  Error: No rule found for my_project/lib/.lib.objs/byte/lib__A.cmt
  File "my_project/lib/dune", line 1, characters 0-61:
  1 | (library
  2 |  (name lib)
  3 |  (public_name pkg.lib)
  4 |  (modes melange))
  Error: No rule found for my_project/lib/.lib.objs/byte/lib__A.cmti
  [1]

