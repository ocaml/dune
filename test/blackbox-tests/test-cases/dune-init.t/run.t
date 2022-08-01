Setup mutable files
-------------------

  $ mkdir -p existing_project/src
  $ cat > existing_project/src/dune << EOF
  > ; A comment
  > (library
  >   ; Another comment
  >   (name test_lib))
  > EOF

Adding a library
----------------

Can init a public library

  $ dune init lib test_lib ./_test_lib_dir --public
  Success: initialized library component named test_lib

Can build the public library

  $ (cd _test_lib_dir && touch test_lib.opam && dune build)
  Warning: No dune-project file has been found. A default one is assumed but
  the project might break when dune is upgraded. Please create a dune-project
  file.
  Hint: generate the project file with: $ dune init project <name>
  $ cat ./_test_lib_dir/dune
  (library
   (public_name test_lib)
   (name test_lib))

Clean up the library tests

  $ rm -rf ./_test_lib_dir

Can init library with a specified public name

  $ dune init lib test_lib ./_test_lib_dir --public test_lib_public_name
  Success: initialized library component named test_lib
  $ cat ./_test_lib_dir/dune
  (library
   (public_name test_lib_public_name)
   (name test_lib))

Clean up library with specified public name

  $ rm -rf ./_test_lib_dir

Can add a library with inline tests

  $ dune init lib test_lib ./_inline_tests_lib --inline-tests --ppx ppx_inline_test
  Success: initialized library component named test_lib
  $ cat ./_inline_tests_lib/dune
  (library
   (inline_tests)
   (name test_lib)
   (preprocess
    (pps ppx_inline_test)))

Clean up library with inlines tests

  $ rm -rf ./_inline_tests_lib

Adding an executable
--------------------

Can init a public executable

  $ dune init exe test_bin ./_test_bin_dir --public
  Success: initialized executable component named test_bin

Can build an executable

  $ (cd _test_bin_dir && touch test_bin.opam && dune build)
  Warning: No dune-project file has been found. A default one is assumed but
  the project might break when dune is upgraded. Please create a dune-project
  file.
  Hint: generate the project file with: $ dune init project <name>

Can run the created executable

  $ (cd _test_bin_dir && dune exec test_bin)
  Warning: No dune-project file has been found. A default one is assumed but
  the project might break when dune is upgraded. Please create a dune-project
  file.
  Hint: generate the project file with: $ dune init project <name>
  Hello, World!

Clean up the executable tests

  $ rm -rf ./_test_bin_dir

Adding tests
------------

Can init tests

  $ dune init test test_tests ./_test_tests_dir --libs foo,bar
  Success: initialized test component named test_tests
  $ ls ./_test_tests_dir
  dune
  test_tests.ml
  $ cat ./_test_tests_dir/dune
  (test
   (name test_tests)
   (libraries foo bar))

Clean up the test tests

  $ rm -rf ./_test_tests_dir

Adding components to default and non-standard places
---------------------------------------------------

Add a library in the current working directory

  $ dune init lib test_lib
  Success: initialized library component named test_lib
  $ cat dune
  (library
   (name test_lib))

Clean the library creation

  $ rm ./dune

Add a library to a dune file in a specified directory

  $ dune init lib test_lib ./_test_dir
  Success: initialized library component named test_lib
  $ test -f ./_test_dir/dune

Clean up from the dune file created in ./_test_dir

  $ rm -rf ./_test_dir

Add a library to a dune file in a directory specified with an absolute path

  $ dune init lib test_lib $PWD/_test_dir
  Success: initialized library component named test_lib
  $ test -f $PWD/_test_dir/dune

Clean up from the dune file created at an absolute path

  $ rm -rf $PWD/_test_dir

Adding a library and an executable dependent on that library
------------------------------------------------------------

Can init a library and dependent executable in a combo project

  $ dune init lib test_lib ./_test_lib_exe_dir/src
  Success: initialized library component named test_lib
  $ dune init exe test_bin ./_test_lib_exe_dir/bin --libs test_lib --public
  Success: initialized executable component named test_bin

Can build the combo project

  $ (cd _test_lib_exe_dir && touch test_bin.opam && dune build)
  Warning: No dune-project file has been found. A default one is assumed but
  the project might break when dune is upgraded. Please create a dune-project
  file.
  Hint: generate the project file with: $ dune init project <name>

Can run the combo project

  $ (cd _test_lib_exe_dir && dune exec test_bin)
  Warning: No dune-project file has been found. A default one is assumed but
  the project might break when dune is upgraded. Please create a dune-project
  file.
  Hint: generate the project file with: $ dune init project <name>
  Hello, World!

Clean up the combo project

  $ rm -rf ./_test_lib_exe_dir

Adding libraries in a single directory
--------------------------------------

Can add multiple libraries in the same directory

  $ dune init lib test_lib1 ./_test_lib
  Success: initialized library component named test_lib1
  $ dune init lib test_lib2 ./_test_lib --libs test_lib1
  Success: initialized library component named test_lib2
  $ cat ./_test_lib/dune
  (library
   (name test_lib1))
  
  (library
   (name test_lib2)
   (libraries test_lib1))

Can build the multiple library project

  $ (cd _test_lib && touch test_lib1.opam && dune build)
  Warning: No dune-project file has been found. A default one is assumed but
  the project might break when dune is upgraded. Please create a dune-project
  file.
  Hint: generate the project file with: $ dune init project <name>

Clan up the multiple library project

  $ rm -rf ./_test_lib

Multiple ppxs and library dependencies
--------------------------------------

Can add multiple library dependencies in one command

  $ dune init lib test_lib ./_test_lib --libs foo,bar --ppx ppx_foo,ppx_bar
  Success: initialized library component named test_lib
  $ cat _test_lib/dune
  (library
   (name test_lib)
   (libraries foo bar)
   (preprocess
    (pps ppx_foo ppx_bar)))

Clean up the multiple dependencies project

  $ rm -rf ./_test_lib

Safety and Validation
---------------------

Will not overwrite existing files

  $ dune init exe main ./existing_project/bin
  Warning: File existing_project/bin/main.ml was not created because it already
  exists
  Success: initialized executable component named main
  $ cat ./existing_project/bin/main.ml
  () = print_endline "Goodbye"

Comments in dune files are preserved

  $ dune init lib test_lib2 ./existing_project/src
  Success: initialized library component named test_lib2
  $ cat ./existing_project/src/dune
  ; A comment
  
  (library
   ; Another comment
   (name test_lib))
  
  (library
   (name test_lib2))

Will not create components with invalid names

  $ dune init lib invalid-component-name ./_test_lib
  dune init: NAME argument: invalid component name
             `invalid-component-name'
             Library names must be non-empty and composed only of the
             following
             characters: 'A'..'Z', 'a'..'z', '_' or '0'..'9'.
  Usage: dune init [OPTION]... COMPONENT NAME [PATH]
  Try `dune init --help' or `dune --help' for more information.
  [1]
  $ test -f ./_test_lib
  [1]

Will fail and inform user when invalid component command is given

  $ dune init foo blah
  dune init: COMPONENT argument: invalid value `foo', expected one of
             `executable', `library', `project' or `test'
  Usage: dune init [OPTION]... COMPONENT NAME [PATH]
  Try `dune init --help' or `dune --help' for more information.
  [1]

Will fail and inform user when an invalid option is given to a component

  $ dune init test test_foo --public
  Error: The `test' component does not support the `--public' option
  [1]
  $ dune init exe test_exe --inline-tests
  Error: The `executable' component does not support the `--inline-tests'
  option
  [1]

Adding fields to existing stanzas
---------------------------------

# TODO(shonfeder)
Adding fields to existing stanzas is currently not supported

  $ dune init exe test_bin ./_test_bin --libs test_lib1
  Success: initialized executable component named test_bin
  $ dune init exe test_bin ./_test_bin --libs test_lib2
  Error: Updating existing stanzas is not yet supported.
  A preexisting dune stanza conflicts with a generated stanza:
  
  Generated stanza:
  (executable (name test_bin) (libraries test_lib2))
  
  Pre-existing stanza:
  (executable (name test_bin) (libraries test_lib1))
  [1]
  $ cat ./_test_bin/dune
  (executable
   (name test_bin)
   (libraries test_lib1))

Creating projects
-----------------

Initializing executable projects
================================

We can init a new executable project:

  $ dune init proj test_exec_proj
  Success: initialized project component named test_exec_proj

The generated project contains all expected sub-components:

  $ ls test_exec_proj/**
  test_exec_proj/dune-project
  test_exec_proj/test_exec_proj.opam
  
  test_exec_proj/bin:
  dune
  main.ml
  
  test_exec_proj/lib:
  dune
  
  test_exec_proj/test:
  dune
  test_exec_proj.ml

In particular, the dune-project file has the expected content:

  $ cat test_exec_proj/dune-project | sed 's/dune [0-9].[0-9]/dune $version/g'
  (lang dune $version)
  
  (name test_exec_proj)
  
  (generate_opam_files true)
  
  (source
   (github username/reponame))
  
  (authors "Author Name")
  
  (maintainers "Maintainer Name")
  
  (license LICENSE)
  
  (documentation https://url/to/documentation)
  
  (package
   (name test_exec_proj)
   (synopsis "A short synopsis")
   (description "A longer description")
   (depends ocaml dune)
   (tags
    (topics "to describe" your project)))
  
  ; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

We can build the project:

  $ dune build --root test_exec_proj
  Entering directory 'test_exec_proj'

And the opam file will be generated as expected

  $ cat test_exec_proj/test_exec_proj.opam | sed 's/"dune"/$dune/'
  # This file is generated by dune, edit dune-project instead
  opam-version: "2.0"
  synopsis: "A short synopsis"
  description: "A longer description"
  maintainer: ["Maintainer Name"]
  authors: ["Author Name"]
  license: "LICENSE"
  tags: ["topics" "to describe" "your" "project"]
  homepage: "https://github.com/username/reponame"
  doc: "https://url/to/documentation"
  bug-reports: "https://github.com/username/reponame/issues"
  depends: [
    "ocaml"
    $dune {>= "3.5"}
    "odoc" {with-doc}
  ]
  build: [
    [$dune "subst"] {dev}
    [
      $dune
      "build"
      "-p"
      name
      "-j"
      jobs
      "@install"
      "@runtest" {with-test}
      "@doc" {with-doc}
    ]
  ]
  dev-repo: "git+https://github.com/username/reponame.git"

We can build and run the resulting executable:

  $ dune exec --root test_exec_proj ./bin/main.exe
  Entering directory 'test_exec_proj'
  Hello, World!

We can build and run the project's tests:

  $ dune exec --root test_exec_proj ./test/test_exec_proj.exe
  Entering directory 'test_exec_proj'

Initializing library projects
================================

We can init a new library project:

  $ dune init proj test_lib_proj --kind lib
  Success: initialized project component named test_lib_proj

The generated project contains all expected sub-components:

  $ ls test_lib_proj/**
  test_lib_proj/dune-project
  test_lib_proj/test_lib_proj.opam
  
  test_lib_proj/lib:
  dune
  
  test_lib_proj/test:
  dune
  test_lib_proj.ml

In particular, the dune-project file has the expected content:

  $ cat test_lib_proj/dune-project | sed 's/dune [0-9].[0-9]/dune $version/g'
  (lang dune $version)
  
  (name test_lib_proj)
  
  (generate_opam_files true)
  
  (source
   (github username/reponame))
  
  (authors "Author Name")
  
  (maintainers "Maintainer Name")
  
  (license LICENSE)
  
  (documentation https://url/to/documentation)
  
  (package
   (name test_lib_proj)
   (synopsis "A short synopsis")
   (description "A longer description")
   (depends ocaml dune)
   (tags
    (topics "to describe" your project)))
  
  ; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

We can build and install the project:

  $ dune build --root test_lib_proj @install --display short
  Entering directory 'test_lib_proj'
        ocamlc lib/.test_lib_proj.objs/byte/test_lib_proj.{cmi,cmo,cmt}
      ocamlopt lib/.test_lib_proj.objs/native/test_lib_proj.{cmx,o}
        ocamlc lib/test_lib_proj.cma
      ocamlopt lib/test_lib_proj.{a,cmxa}
      ocamlopt lib/test_lib_proj.cmxs

And the opam file will be generated as expected

  $ cat test_lib_proj/test_lib_proj.opam
  # This file is generated by dune, edit dune-project instead
  opam-version: "2.0"
  synopsis: "A short synopsis"
  description: "A longer description"
  maintainer: ["Maintainer Name"]
  authors: ["Author Name"]
  license: "LICENSE"
  tags: ["topics" "to describe" "your" "project"]
  homepage: "https://github.com/username/reponame"
  doc: "https://url/to/documentation"
  bug-reports: "https://github.com/username/reponame/issues"
  depends: [
    "ocaml"
    "dune" {>= "3.5"}
    "odoc" {with-doc}
  ]
  build: [
    ["dune" "subst"] {dev}
    [
      "dune"
      "build"
      "-p"
      name
      "-j"
      jobs
      "@install"
      "@runtest" {with-test}
      "@doc" {with-doc}
    ]
  ]
  dev-repo: "git+https://github.com/username/reponame.git"

And we we can run the tests:

  $ dune runtest --root test_lib_proj --display short
  Entering directory 'test_lib_proj'
        ocamlc test/.test_lib_proj.eobjs/byte/dune__exe__Test_lib_proj.{cmi,cmti}
      ocamlopt test/.test_lib_proj.eobjs/native/dune__exe__Test_lib_proj.{cmx,o}
      ocamlopt test/test_lib_proj.exe
  test_lib_proj alias test/runtest

Initializing projects using Esy
===============================

We can init a project using Esy:

  $ dune init proj test_esy_proj --pkg esy
  Success: initialized project component named test_esy_proj

The esy project contains all expected sub-components:

  $ ls test_esy_proj/**
  test_esy_proj/dune-project
  test_esy_proj/package.json
  
  test_esy_proj/bin:
  dune
  main.ml
  
  test_esy_proj/lib:
  dune
  
  test_esy_proj/test:
  dune
  test_esy_proj.ml

And the dune-project file does NOT specify generation of an opam file:

  $ cat test_esy_proj/dune-project | grep "generate_opam_files"
  (generate_opam_files false)
