 This test checks if rule that do not list all of its dependencies cannot
 permanently corrupt internal data structures, and by that we mean that after
 the rule is replaced with a valid one the target will be rebuild.

  $ mkdir some_directory
  $ touch some_directory/foo.1
  $ touch some_directory/foo.2

 Create a dune file with a rule that depends on 'some_directory/' directory
 listing but reports that depends only on listing of 'some_directory/*.1'.

  $ cat> dune <<EOF
  > (rule
  >  (target some_target)
  >  (deps (glob_files some_directory/*.1))
  >  (action
  >   (progn
  >    (bash "ls -1 some_directory > some_target"))))
  > \
  > (rule
  >  (alias some_alias)
  >  (deps (universe))
  >  (action (cat some_target)))
  > EOF

 Force foo.{1,2} to be copied to _build directory. That can
 happen in "real world" as a part of building other rules.

  $ dune build some_directory/foo.1 some_directory/foo.2

 Build some_target and print its content.

  $ dune build @some_alias
  foo.1
  foo.2

 Replace the rule with valid one and remove the some_directory/foo.1 file
 so that list of files "seen" by glob does not change, but the content
 of some_target should change.

  $ cat> dune <<EOF
  > (rule
  >  (target some_target)
  >  (deps (glob_files some_directory/*))
  >  (action
  >   (progn
  >    (bash "ls -1 some_directory > some_target"))))
  > \
  > (rule
  >  (alias some_alias)
  >  (deps (universe))
  >  (action (cat some_target)))
  > EOF

  $ rm some_directory/foo.2

 Rule should be rerun and some_target should change.

  $ dune build @some_alias
  foo.1
