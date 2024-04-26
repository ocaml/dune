Setup mutable files
-------------------

  $ mkdir -p existing_project/src
  $ cat > existing_project/src/dune << EOF
  > ; A comment
  > (library
  >   ; Another comment
  >   (name test_lib))
  > EOF

Adding a Library
----------------

Can init a public library

  $ dune init lib test_lib ./_test_lib_dir --public
  Success: initialized library component named test_lib

Can build the public library

  $ (cd _test_lib_dir && touch test_lib.opam && dune build)
  File ".", line 1, characters 0-0:
  Warning: No dune-project file has been found in directory ".". A default one
  is assumed but the project might break when dune is upgraded. Please create a
  dune-project file.
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

Adding an Executable
--------------------

Can init a public executable

  $ dune init exe test_bin ./_test_bin_dir --public
  Success: initialized executable component named test_bin

Can build an executable

  $ (cd _test_bin_dir && touch test_bin.opam && dune build)
  File ".", line 1, characters 0-0:
  Warning: No dune-project file has been found in directory ".". A default one
  is assumed but the project might break when dune is upgraded. Please create a
  dune-project file.
  Hint: generate the project file with: $ dune init project <name>

Can run the created executable

  $ (cd _test_bin_dir && dune exec test_bin)
  File ".", line 1, characters 0-0:
  Warning: No dune-project file has been found in directory ".". A default one
  is assumed but the project might break when dune is upgraded. Please create a
  dune-project file.
  Hint: generate the project file with: $ dune init project <name>
  Hello, World!

Clean up the executable tests

  $ rm -rf ./_test_bin_dir

Adding Tests
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

Adding Components to Default and Non-Standard Places
---------------------------------------------------

Add a library in the current working directory

  $ dune init lib test_lib
  Success: initialized library component named test_lib
  $ cat dune
  (library
   (name test_lib))

Clean the library creation

  $ rm ./dune

Add a library to a `dune` file in a specified directory

  $ dune init lib test_lib ./_test_dir
  Success: initialized library component named test_lib
  $ test -f ./_test_dir/dune

Clean up from the `dune` file created in ./_test_dir

  $ rm -rf ./_test_dir

Add a library to a `dune` file in a directory specified with an absolute path

  $ dune init lib test_lib $PWD/_test_dir
  Success: initialized library component named test_lib
  $ test -f $PWD/_test_dir/dune

Clean up from the `dune` file created at an absolute path

  $ rm -rf $PWD/_test_dir

Add a library in the current working directory

  $ dune init lib test_cwd_lib .
  Success: initialized library component named test_cwd_lib
  $ test -f dune

Clean up from the `dune` file created in the current workding dir

  $ rm dune

Adding a Library and an Executable Dependent on that Library
------------------------------------------------------------

Can init a library and dependent executable in a combo project

  $ dune init lib test_lib ./_test_lib_exe_dir/src
  Success: initialized library component named test_lib
  $ dune init exe test_bin ./_test_lib_exe_dir/bin --libs test_lib --public
  Success: initialized executable component named test_bin

Can build the combo project

  $ (cd _test_lib_exe_dir && touch test_bin.opam && dune build)
  File ".", line 1, characters 0-0:
  Warning: No dune-project file has been found in directory ".". A default one
  is assumed but the project might break when dune is upgraded. Please create a
  dune-project file.
  Hint: generate the project file with: $ dune init project <name>

Can run the combo project

  $ (cd _test_lib_exe_dir && dune exec test_bin)
  File ".", line 1, characters 0-0:
  Warning: No dune-project file has been found in directory ".". A default one
  is assumed but the project might break when dune is upgraded. Please create a
  dune-project file.
  Hint: generate the project file with: $ dune init project <name>
  Hello, World!

Clean up the combo project

  $ rm -rf ./_test_lib_exe_dir

Adding Libraries in a Single Directory
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
  File ".", line 1, characters 0-0:
  Warning: No dune-project file has been found in directory ".". A default one
  is assumed but the project might break when dune is upgraded. Please create a
  dune-project file.
  Hint: generate the project file with: $ dune init project <name>

Clan up the multiple library project

  $ rm -rf ./_test_lib

Multiple PPXs and Library Dependencies
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

Comments in `dune` files are preserved

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
  dune: NAME argument: invalid component name `invalid-component-name'
        Library names must be non-empty and composed only of the
        following
        characters: 'A'..'Z', 'a'..'z', '_' or '0'..'9'.
  Usage: dune init library [OPTION]… NAME [PATH]
  Try 'dune init library --help' or 'dune --help' for more information.
  [1]
  $ test -f ./_test_lib
  [1]

Adding Fields to Existing Stanzas
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

Creating Projects
-----------------

Initializing Executable Projects
================================

We can init a new executable project:

  $ dune init proj new_exec_proj
  Entering directory 'new_exec_proj'
  Success: initialized project component named new_exec_proj
  Leaving directory 'new_exec_proj'

The generated project contains all expected subcomponents:

  $ ls new_exec_proj/**
  new_exec_proj/dune-project
  new_exec_proj/new_exec_proj.opam
  
  new_exec_proj/_build:
  log
  
  new_exec_proj/bin:
  dune
  main.ml
  
  new_exec_proj/lib:
  dune
  
  new_exec_proj/test:
  dune
  test_new_exec_proj.ml

In particular, the `dune-project` file has the expected content:

  $ cat new_exec_proj/dune-project | sed 's/dune [0-9].[0-9]*/dune $version/g'
  (lang dune $version)
  
  (name new_exec_proj)
  
  (generate_opam_files true)
  
  (source
   (github username/reponame))
  
  (authors "Author Name")
  
  (maintainers "Maintainer Name")
  
  (license LICENSE)
  
  (documentation https://url/to/documentation)
  
  (package
   (name new_exec_proj)
   (synopsis "A short synopsis")
   (description "A longer description")
   (depends ocaml dune)
   (tags
    (topics "to describe" your project)))
  
  ; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

We can build the project:

  $ dune build --root new_exec_proj
  Entering directory 'new_exec_proj'
  Leaving directory 'new_exec_proj'

And the opam file will be generated as expected

  $ cat new_exec_proj/new_exec_proj.opam | sed 's/"dune"/$dune/'
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
    $dune {>= "3.16"}
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

  $ dune exec --root new_exec_proj ./bin/main.exe
  Entering directory 'new_exec_proj'
  Leaving directory 'new_exec_proj'
  Hello, World!

We can build and run the project's tests:

  $ dune exec --root new_exec_proj ./test/test_new_exec_proj.exe
  Entering directory 'new_exec_proj'
  Leaving directory 'new_exec_proj'

Initializing Library Projects
================================

We can init a new library project:

  $ dune init proj new_lib_proj --kind lib
  Entering directory 'new_lib_proj'
  Success: initialized project component named new_lib_proj
  Leaving directory 'new_lib_proj'

The generated project contains all expected subcomponents:

  $ ls new_lib_proj/**
  new_lib_proj/dune-project
  new_lib_proj/new_lib_proj.opam
  
  new_lib_proj/_build:
  log
  
  new_lib_proj/lib:
  dune
  
  new_lib_proj/test:
  dune
  test_new_lib_proj.ml

In particular, the `dune-project` file has the expected content:

  $ cat new_lib_proj/dune-project | sed 's/dune [0-9].[0-9]*/dune $version/g'
  (lang dune $version)
  
  (name new_lib_proj)
  
  (generate_opam_files true)
  
  (source
   (github username/reponame))
  
  (authors "Author Name")
  
  (maintainers "Maintainer Name")
  
  (license LICENSE)
  
  (documentation https://url/to/documentation)
  
  (package
   (name new_lib_proj)
   (synopsis "A short synopsis")
   (description "A longer description")
   (depends ocaml dune)
   (tags
    (topics "to describe" your project)))
  
  ; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

We can build and install the project:

  $ dune build --root new_lib_proj @install
  Entering directory 'new_lib_proj'
  Leaving directory 'new_lib_proj'

And the opam file will be generated as expected

  $ cat new_lib_proj/new_lib_proj.opam
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
    "dune" {>= "3.16"}
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

  $ dune runtest --root new_lib_proj
  Entering directory 'new_lib_proj'
  Leaving directory 'new_lib_proj'


Initializing Projects Using the PATH Argument
=============================================

We can init a project in a specified directory:

  $ dune init proj proj_at_path path/to/project
  Entering directory 'path/to/project'
  Success: initialized project component named proj_at_path
  Leaving directory 'path/to/project'
  $ test -f path/to/project/dune-project

A project can be initialized in the current directory (addressing
https://github.com/ocaml/dune/issues/9209):

  $ mkdir a-new-dir
  $ cd a-new-dir
  $ dune init proj project_in_cwd .
  Success: initialized project component named project_in_cwd
  $ test -f dune-project

Initializing Projects Using Esy
===============================

We can init a project using Esy:

  $ dune init proj new_esy_proj --pkg esy
  Entering directory 'new_esy_proj'
  Success: initialized project component named new_esy_proj
  Leaving directory 'new_esy_proj'

The `esy` project contains all expected subcomponents:

  $ ls new_esy_proj/**
  new_esy_proj/dune-project
  new_esy_proj/package.json
  
  new_esy_proj/_build:
  log
  
  new_esy_proj/bin:
  dune
  main.ml
  
  new_esy_proj/lib:
  dune
  
  new_esy_proj/test:
  dune
  test_new_esy_proj.ml

And the `dune-project` file does NOT specify generation of an opam file:

  $ cat new_esy_proj/dune-project | grep "generate_opam_files"
  (generate_opam_files false)
