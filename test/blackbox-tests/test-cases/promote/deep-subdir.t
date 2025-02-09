Tests for promoting directory targets
-------------------------------------

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > (using directory-targets 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (deps
  >   (source_tree deep))
  >  (targets
  >   (dir deep_copied))
  >  (mode promote)
  >  (action
  >   (run cp -r deep deep_copied)))
  > EOF

Let's create the directory structure we are going to promote (as a copy on
another dir):

  $ mkdir -p deep/a/
  $ touch deep/a/deep_file
  $ touch deep/base_file

Let's try this:

  $ dune build deep_copied

This one works. Now, let's add a layer between base_file and deep_file:

  $ rm -rf deep
  $ mkdir -p deep/a/b/
  $ touch deep/a/b/deep_file
  $ touch deep/base_file

  $ dune build deep_copied

It now works!
