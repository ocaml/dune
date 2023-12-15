Test that we can override the default local package version.

  $ . ./helpers.sh

  $ make_lockdir

  $ cat > dune-project <<EOF
  > (lang dune 3.12)
  > (package (name foo))
  > EOF

This command incidentally prints the local package version:
  $ dune describe pkg list-locked-dependencies
  Dependencies of local packages locked in dune.lock
  - Immediate dependencies of local package foo.dev
    
    

Override the default local package version for the default lockdir:
  $ cat > dune-workspace <<EOF
  > (lang dune 3.12)
  > (lock_dir
  >  (default_local_package_version bar))
  > EOF

  $ dune describe pkg list-locked-dependencies
  Dependencies of local packages locked in dune.lock
  - Immediate dependencies of local package foo.bar
    
    

Override the default local package version for an alternative lockdir:
  $ cat > dune-workspace <<EOF
  > (lang dune 3.12)
  > (lock_dir
  >  (path dune.alternative.lock)
  >  (default_local_package_version baz))
  > (context
  >  (default
  >   (name alternative)
  >   (lock_dir dune.alternative.lock)))
  > EOF

  $ make_lockdir dune.alternative.lock

  $ dune describe pkg list-locked-dependencies dune.alternative.lock
  Dependencies of local packages locked in dune.alternative.lock
  - Immediate dependencies of local package foo.baz
    
    
