Create a config file to use in all test that follow, adding the 
'project_defaults' stanza to specify default values for various fields of the
generated 'dune-project' file.

  $ touch dune-config 
  $ cat >dune-config <<EOF
  > (lang dune 3.17)
  > (project_defaults
  >  (authors AuthorTest)
  >  (maintainers MaintainerTest)
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
  File "$TESTCASE_ROOT/dune-config", lines 2-5, characters 0-85:
  2 | (project_defaults
  3 |  (authors AuthorTest)
  4 |  (maintainers MaintainerTest)
  5 |  (license MIT))
  Error: 'project_defaults' is only available since version 3.17 of the dune
  language. Please update your dune-project file to have (lang dune 3.17).
  [1]

  $ sed -i -e '1s|.*|(lang dune 3.17)|' dune-config

Check to ensure that the default values are used when optional stanzas are 
removed/not used.

  $ sed -i -e '3,5c\)' dune-config
  $ dune init proj test_proj1 --config-file=dune-config
  Entering directory 'test_proj1'
  Success: initialized project component named test_proj1
  Leaving directory 'test_proj1'

  $ cat test_proj1/dune-project | grep -i authors
  (authors "Author Name")

  $ cat test_proj1/dune-project | grep -i maintainers
  (maintainers "Maintainer Name")

  $ cat test_proj1/dune-project | grep -i license
  (license LICENSE)
