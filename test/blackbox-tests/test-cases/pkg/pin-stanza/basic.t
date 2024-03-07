The pin stanza allows us to define packages that are not available
in any repository

  $ . ../helpers.sh

  $ mkrepo
  $ add_mock_repo_if_needed

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "file://$PWD/_extra_source")
  >  (package
  >   (name foo)
  >   (version 1.0.0)))
  > (package
  >  (name main)
  >  (depends foo))
  > EOF

  $ mkdir _extra_source
  $ cat >_extra_source/dune-project <<EOF
  > (lang dune 3.12)
  > (package (name foo))
  > EOF

  $ dune pkg lock
  Solution for dune.lock:
  - foo.1.0.0


Now we verify the metadata we generated for the package. First we verify the
build instructions and version are set correctly.

We print the source separately for ease of post processing the output.
  $ cat dune.lock/foo.pkg | sed "/source/,//d"
  (version 1.0.0)
  
  (dune)
  
  
  (dev)

Now we make sure that the source is set correctly.

  $ print_source "foo"
  (source (fetch (url file://PWD/_extra_source))) 
