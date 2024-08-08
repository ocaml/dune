Test that the 'CACHEDIR.TAG' file is created in the default dune cache 
directory.

  $ export DUNE_CACHE=enabled
  $ export XDG_CACHE_HOME=$PWD/.xdg-cache

  $ cat > dune-project <<EOF
  > (lang dune 3.5)
  > EOF

  $ dune build

Check that the 'CACHEDIR.TAG' file was created and sits at the top level of the
default dune cache directory.

  $ ls $XDG_CACHE_HOME/dune
  CACHEDIR.TAG
  db

Check that the contents of the 'CACHEDIR.TAG' file conform to the specification
found at 'https://bford.info/cachedir/'

  $ cat $XDG_CACHE_HOME/dune/CACHEDIR.TAG
  Signature: 8a477f597d28d172789f06886806bc55

Set the enviornment variable 'DUNE_CACHE_ROOT' to a custom value overriding the
default dune cache directory. In this case the 'CACHEDIR.TAG' file should NOT
be created.

  $ export DUNE_CACHE_ROOT=$PWD/personal-cache
  $ dune build

  $ ls $DUNE_CACHE_ROOT
  files
  meta
  temp
  values
