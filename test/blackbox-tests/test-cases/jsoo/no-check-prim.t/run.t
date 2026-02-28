Compilation using jsoo

  $ dune build bin/technologic.bc.js @install

  $ node ./_build/default/bin/technologic.bc.js
  buy it
  use it
  break it
  fix it
  $ dune build bin/technologic.bc.js @install --profile release
  $ node ./_build/default/bin/technologic.bc.js
  buy it
  use it
  break it
  fix it
  $ cat >dune-workspace <<EOF
  > (lang dune 2.0)
  > (context
  >   (default (disable_dynamically_linked_foreign_archives true)))
  > EOF
  $ dune build bin/technologic.bc.js @install --profile dev
  $ dune build bin/technologic.bc.js @install --profile release
