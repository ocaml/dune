Create a config file to use in all test that follow, adding the 
'project_defaults' stanza to specify default values for various fields of the
generated 'dune-project' file.

  $ touch dune-config 
  $ cat >dune-config <<EOF
  > (lang dune 3.18)
  > (project_defaults
  >  (authors AuthorTest)
  >  (maintainers MaintainerTest)
  >  (maintenance_intent "(latest)")
  >  (license MIT))
  > EOF

Initialize a new dune project providing the config file we just created and 
check each of the stanzas that we set defaults for in the config file.

  $ dune init proj test_proj --config-file=dune-config
  Entering directory 'test_proj'
  Success: initialized project component named test_proj
  Leaving directory 'test_proj'

  $ cat test_proj/dune-project | grep -i authors
  (authors AuthorTest)

  $ cat test_proj/dune-project | grep -i maintainers
  (maintainers MaintainerTest)

  $ cat test_proj/dune-project | grep -i license
  (license MIT)

Change the version of the config file to one which does not support the
'project_defaults' stanza to ensure the proper error is raised.

  $ sed -i -e '1s|.*|(lang dune 3.16)|' dune-config
  $ dune init proj test_proj1 --config-file=dune-config
  File "$TESTCASE_ROOT/dune-config", lines 2-6, characters 0-118:
  2 | (project_defaults
  3 |  (authors AuthorTest)
  4 |  (maintainers MaintainerTest)
  5 |  (maintenance_intent "(latest)")
  6 |  (license MIT))
  Error: 'project_defaults' is only available since version 3.17 of the dune
  language. Please update your dune config file to have (lang dune 3.17).
  [1]

  $ sed -i -e '1s|.*|(lang dune 3.17)|' dune-config

Check to ensure that the default values are used when optional stanzas are 
removed/not used.

  $ sed -i -e '3,6c\
  > )' dune-config
  $ dune init proj test_proj1 --config-file=dune-config
  Entering directory 'test_proj1'
  Success: initialized project component named test_proj1
  Leaving directory 'test_proj1'

  $ cat test_proj1/dune-project | grep -i authors
  (authors "Author Name <author@example.com>")

  $ cat test_proj1/dune-project | grep -i maintainers
  (maintainers "Maintainer Name <maintainer@example.com>")

  $ cat test_proj1/dune-project | grep -i license
  (license LICENSE)

In the previous test all sub stanzas of the 'project_default' stanza where
removed so we will create a new config file continue testing. This time we will
used quoted string values and test the ability to add multiple
authors/maintainers.

  $ rm dune-config; touch dune-config
  $ cat >dune-config <<EOF
  > (lang dune 3.17)
  > (project_defaults
  >  (authors "AuthorTest1" "AuthorTest2")
  >  (maintainers "Maintainer1" "Maintainer2" "Maintainer3")
  >  (license "BSD"))
  > EOF

Now we test to see if quoted list values are properly generated in the
dune-project file.

  $ dune init proj test_proj2 --config-file=dune-config
  Entering directory 'test_proj2'
  Success: initialized project component named test_proj2
  Leaving directory 'test_proj2'

  $ cat test_proj2/dune-project | grep -i authors
  (authors AuthorTest1 AuthorTest2)

  $ cat test_proj2/dune-project | grep -i maintainers
  (maintainers Maintainer1 Maintainer2 Maintainer3)

  $ cat test_proj2/dune-project | grep -i license
  (license BSD)
