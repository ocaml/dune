Tests dune memory on the full build universe.

  $ setup_xdg_runtime_dir

  $ env DUNE_CACHE=enabled XDG_CACHE_HOME=$PWD/.xdg-cache dune build @x
  Hello, world!
  $ env DUNE_CACHE=enabled XDG_CACHE_HOME=$PWD/.xdg-cache dune build @x
  Hello, world!
  $ env DUNE_CACHE=enabled XDG_CACHE_HOME=$PWD/.xdg-cache dune build @x
  Hello, world!
  $ env DUNE_CACHE=enabled XDG_CACHE_HOME=$PWD/.xdg-cache dune build @x
  Hello, world!
  $ env DUNE_CACHE=enabled XDG_CACHE_HOME=$PWD/.xdg-cache dune build @x
  Hello, world!
