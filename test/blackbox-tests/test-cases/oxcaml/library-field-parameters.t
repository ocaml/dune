Testing the `parameters` field in library stanzas.

  $ cat >> dune-project <<EOF
  > (lang dune 3.20)
  > EOF

It should fail because the syntax extension wasn't enabled in `dune-project`:

  $ mkdir lib
  $ echo 'let test () = print_endline A.foo' > lib/lib.ml
  $ cat >lib/dune <<EOF
  > (library (name lib) (parameters a))
  > EOF

  $ dune build
  File "lib/dune", line 1, characters 20-34:
  1 | (library (name lib) (parameters a))
                          ^^^^^^^^^^^^^^
  Error: 'parameters' is available only when oxcaml is enabled in the
  dune-project file. You must enable it using (using oxcaml 0.1) in your
  dune-project file.
  Note however that oxcaml is experimental and might change without notice in
  the future.
  [1]

After enabling the feature, it should fail reporting an unknown library
parameter `a`:

  $ cat >> dune-project <<EOF
  > (using oxcaml 0.1)
  > EOF

  $ dune build
  File "lib/dune", line 1, characters 32-33:
  1 | (library (name lib) (parameters a))
                                      ^
  Error: Library "a" not found.
  -> required by alias default
  [1]

We introduce a package to also test the public names:

  $ cat >> dune-project <<EOF
  > (package (name project))
  > EOF

It should work once we define the library parameter `a`:

  $ mkdir a
  $ echo 'val foo : string' > a/a.mli
  $ cat >a/dune <<EOF
  > (library_parameter (public_name project.a) (name a))
  > EOF

  $ dune build

It should fail if `parameters` list libraries that are not parameters:

  $ mkdir b
  $ echo 'let b = "not a parameter"' > b/b.ml
  $ cat >b/dune <<EOF
  > (library (name b))
  > EOF

  $ cat >lib/dune <<EOF
  > (library (name lib) (parameters project.a b))
  > EOF

  $ dune build
  File "lib/dune", line 1, characters 42-43:
  1 | (library (name lib) (parameters project.a b))
                                                ^
  Error: Expected "b" to be a library parameter.
  -> required by alias default
  [1]

It should work if `b` is defined as a parameter:

  $ rm b/b.ml
  $ echo 'val bar : string' > b/b.mli
  $ cat >b/dune <<EOF
  > (library_parameter (name b))
  > EOF

  $ dune build

It should fail on duplicate parameters:

  $ cat >lib/dune <<EOF
  > (library (name lib) (parameters project.a b a))
  > EOF

  $ dune build
  File "lib/dune", line 1, characters 44-45:
  1 | (library (name lib) (parameters project.a b a))
                                                  ^
  Error: Duplicate library parameters: "project.a" and "a".
  -> required by alias default
  [1]

Once the error is fixed, it should work:

  $ cat >lib/dune <<EOF
  > (library (name lib) (parameters b project.a))
  > EOF

  $ dune build

A library can have multiple modules, and each one must be compiled with the
parameters:

  $ echo 'let foo_bar = A.foo ^ B.bar' > lib/lib_util.ml
  $ echo 'let test () = B.bar, Lib_util.foo_bar' > lib/lib.ml
  $ dune build

We can by inspecting the `ocamlobjinfo` for the "Runtime parameters:" field,
which lists the parameters on the following indented lines:

  $ alias runtime_parameters="sed -ne '/parameter/,/^[^\t]/{/^\t/p}'"

  $ ocamlobjinfo _build/default/lib/.lib.objs/native/lib__Lib_util.cmx | runtime_parameters
  	Lib__
  	A
  	B

  $ ocamlobjinfo _build/default/lib/.lib.objs/byte/lib__Lib_util.cmo | runtime_parameters
  	Lib__
  	A
  	B

The output of `ocamlobjinfo` is not exactly 1:1 with the flags given to the
compiler.  It only lists the parameters that are actually used (`A` is not used
by `Lib`), but also the parameterized modules that are depended upon,
`Lib_util` and `Lib__`:

  $ ocamlobjinfo _build/default/lib/.lib.objs/native/lib.cmx | runtime_parameters
  	Lib__
  	Lib__Lib_util
  	B

  $ ocamlobjinfo _build/default/lib/.lib.objs/byte/lib.cmo | runtime_parameters
  	Lib__
  	Lib__Lib_util
  	B

It's an error for a public library to depend on a private parameter:

  $ cat >lib/dune <<EOF
  > (library (public_name project.lib) (name lib) (parameters project.a b))
  > EOF

  $ dune build
  File "lib/dune", line 1, characters 68-69:
  1 | (library (public_name project.lib) (name lib) (parameters project.a b))
                                                                          ^
  Error: Library "b" is private, it cannot be a dependency of a public library.
  You need to give "b" a public name.
  [1]

Giving a public name to `b` fixes the error:

  $ cat >b/dune <<EOF
  > (library_parameter (public_name project.b) (name b))
  > EOF

  $ dune build

A library parameter can depend on other libraries when defining its interface:

  $ mkdir utils
  $ cat > utils/utils.ml <<EOF
  > type t = string
  > let to_string x = x
  > EOF
  $ cat > utils/utils.mli <<EOF
  > type t
  > val to_string : t -> string
  > EOF
  $ cat > utils/dune <<EOF
  > (library (public_name project.utils) (name utils))
  > EOF

  $ echo 'val foo : Utils.t' > a/a.mli
  $ cat > a/dune <<EOF
  > (library_parameter (public_name project.a) (name a) (libraries utils))
  > EOF

Since the type of `A.foo` has changed, we must update `lib_util` before we
attempt to build:

  $ echo 'let foo_bar = Utils.to_string A.foo ^ B.bar' > lib/lib_util.ml

  $ dune build

We check that the opam installation will preserve the parameters metadata, both
at the level of the library `project.lib` and for each of its parameterized
modules:

  $ dune build @install
  $ cat _build/install/default/lib/project/dune-package | grep -v 'lang dune'
  (name project)
  (sections (lib .) (libexec .))
  (files
   (lib
    (META
     a/a.cmi
     a/a.cmti
     a/a.mli
     b/b.cmi
     b/b.cmti
     b/b.mli
     dune-package
     lib/lib.a
     lib/lib.cma
     lib/lib.cmi
     lib/lib.cmt
     lib/lib.cmx
     lib/lib.cmxa
     lib/lib.ml
     lib/lib__.cmi
     lib/lib__.cmt
     lib/lib__.cmx
     lib/lib__.ml
     lib/lib__Lib_util.cmi
     lib/lib__Lib_util.cmt
     lib/lib__Lib_util.cmx
     lib/lib_util.ml
     utils/utils.a
     utils/utils.cma
     utils/utils.cmi
     utils/utils.cmt
     utils/utils.cmti
     utils/utils.cmx
     utils/utils.cmxa
     utils/utils.ml
     utils/utils.mli))
   (libexec (lib/lib.cmxs utils/utils.cmxs)))
  (library
   (name project.a)
   (kind parameter)
   (requires project.utils)
   (main_module_name A)
   (modes byte)
   (modules
    (singleton
     (obj_name a)
     (visibility public)
     (kind parameter)
     (source (path A) (intf (path a/a.mli))))))
  (library
   (name project.b)
   (kind parameter)
   (main_module_name B)
   (modes byte)
   (modules
    (singleton
     (obj_name b)
     (visibility public)
     (kind parameter)
     (source (path B) (intf (path b/b.mli))))))
  (library
   (name project.lib)
   (kind normal)
   (archives (byte lib/lib.cma) (native lib/lib.cmxa))
   (plugins (byte lib/lib.cma) (native lib/lib.cmxs))
   (native_archives lib/lib.a)
   (requires project.a project.b)
   (parameters project.a project.b)
   (main_module_name Lib)
   (modes byte native)
   (modules
    (wrapped
     (group
      (alias
       (obj_name lib__)
       (visibility public)
       (kind alias)
       (source (path Lib__) (impl (path lib/lib__.ml-gen))))
      (name Lib)
      (modules
       (module
        (obj_name lib)
        (visibility public)
        (source (path Lib) (impl (path lib/lib.ml))))
       (module
        (obj_name lib__Lib_util)
        (visibility public)
        (source (path Lib_util) (impl (path lib/lib_util.ml))))))
     (wrapped true))))
  (library
   (name project.utils)
   (kind normal)
   (archives (byte utils/utils.cma) (native utils/utils.cmxa))
   (plugins (byte utils/utils.cma) (native utils/utils.cmxs))
   (native_archives utils/utils.a)
   (main_module_name Utils)
   (modes byte native)
   (modules
    (singleton
     (obj_name utils)
     (visibility public)
     (source
      (path Utils)
      (intf (path utils/utils.mli))
      (impl (path utils/utils.ml))))))

It's not possible to use the `parameters` fields in other stanzas than
`(library)`:

  $ mkdir bin
  $ echo 'let () = Lib.test ()' > bin/bin.ml
  $ cat > bin/dune <<EOF
  > (executable (name bin) (parameters a b) (libraries lib))
  > EOF

  $ dune build
  File "bin/dune", line 1, characters 24-34:
  1 | (executable (name bin) (parameters a b) (libraries lib))
                              ^^^^^^^^^^
  Error: Unknown field "parameters"
  [1]

It's incorrect to depend on a parameterized library without providing the
required parameters.

  $ cat > bin/dune <<EOF
  > (executable (name bin) (libraries lib))
  > EOF

  $ dune build
  File "bin/dune", line 1, characters 34-37:
  1 | (executable (name bin) (libraries lib))
                                        ^^^
  Error: Parameter "project.a" is missing.
  -> required by _build/default/bin/.bin.eobjs/native/dune__exe__Bin.cmx
  -> required by _build/default/bin/bin.exe
  -> required by alias bin/all
  -> required by alias default
  Hint: Add (parameters project.a)
  [1]

  $ rm -r bin

Same for libraries:

  $ mkdir lib2
  $ echo 'let test2 = Lib.test ()' > lib2/lib2.ml
  $ cat > lib2/dune <<EOF
  > (library (name lib2) (libraries lib))
  > EOF
  $ dune build
  File "lib2/dune", line 1, characters 32-35:
  1 | (library (name lib2) (libraries lib))
                                      ^^^
  Error: Parameter "project.a" is missing.
  -> required by library "lib2" in _build/default/lib2
  -> required by _build/default/lib2/.lib2.objs/native/lib2.cmx
  -> required by _build/default/lib2/lib2.a
  -> required by alias lib2/all
  -> required by alias default
  Hint: Add (parameters project.a)
  [1]

It works if `lib2` is itself parameterized with the same parameters as `lib`:

  $ cat > lib2/dune <<EOF
  > (library (name lib2) (parameters a b) (libraries lib))
  > EOF

A library can have more parameters than its dependencies:

  $ mkdir c
  $ echo 'val c : string' > c/c.mli
  $ cat > c/dune <<EOF
  > (library_parameter (public_name project.c) (name c))
  > EOF

  $ cat > lib2/dune <<EOF
  > (library (name lib2) (parameters a b c) (libraries lib))
  > EOF

  $ dune build
  $ dune ocaml dump-dot-merlin lib2
  EXCLUDE_QUERY_DIR
  STDLIB /home/sudha/.opam/oxcaml2/lib/ocaml
  SOURCE_ROOT $TESTCASE_ROOT
  B $TESTCASE_ROOT/_build/default/a/.a.objs/byte
  B $TESTCASE_ROOT/_build/default/b/.b.objs/byte
  B $TESTCASE_ROOT/_build/default/c/.c.objs/byte
  B $TESTCASE_ROOT/_build/default/lib/.lib.objs/byte
  B $TESTCASE_ROOT/_build/default/lib2/.lib2.objs/byte
  B $TESTCASE_ROOT/_build/default/utils/.utils.objs/byte
  S $TESTCASE_ROOT/a
  S $TESTCASE_ROOT/b
  S $TESTCASE_ROOT/c
  S $TESTCASE_ROOT/lib
  S $TESTCASE_ROOT/lib2
  S $TESTCASE_ROOT/utils
  INDEX $TESTCASE_ROOT/_build/default/a/.a.objs/cctx.ocaml-index
  INDEX $TESTCASE_ROOT/_build/default/b/.b.objs/cctx.ocaml-index
  INDEX $TESTCASE_ROOT/_build/default/c/.c.objs/cctx.ocaml-index
  INDEX $TESTCASE_ROOT/_build/default/lib/.lib.objs/cctx.ocaml-index
  INDEX $TESTCASE_ROOT/_build/default/lib2/.lib2.objs/cctx.ocaml-index
  INDEX $TESTCASE_ROOT/_build/default/utils/.utils.objs/cctx.ocaml-index
  # FLG -w @1..3@5..28@31..39@43@46..47@49..57@61..62@67@69-40 -strict-sequence -strict-formats -short-paths -keep-locs -g
  # FLG -parameter A -parameter B -parameter C
  
  $ dune ocaml merlin dump-config lib2
  Lib2: _build/default/lib2/lib2
  ((INDEX $TESTCASE_ROOT/_build/default/a/.a.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/b/.b.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/c/.c.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/lib/.lib.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/lib2/.lib2.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/utils/.utils.objs/cctx.ocaml-index)
   (STDLIB /home/sudha/.opam/oxcaml2/lib/ocaml)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B $TESTCASE_ROOT/_build/default/a/.a.objs/byte)
   (B $TESTCASE_ROOT/_build/default/b/.b.objs/byte)
   (B $TESTCASE_ROOT/_build/default/c/.c.objs/byte)
   (B $TESTCASE_ROOT/_build/default/lib/.lib.objs/byte)
   (B $TESTCASE_ROOT/_build/default/lib2/.lib2.objs/byte)
   (B $TESTCASE_ROOT/_build/default/utils/.utils.objs/byte)
   (S $TESTCASE_ROOT/a)
   (S $TESTCASE_ROOT/b)
   (S $TESTCASE_ROOT/c)
   (S $TESTCASE_ROOT/lib)
   (S $TESTCASE_ROOT/lib2)
   (S $TESTCASE_ROOT/utils)
   (FLG (-w @1..3@5..28@31..39@43@46..47@49..57@61..62@67@69-40 -strict-sequence -strict-formats -short-paths -keep-locs -g))
   (FLG (-parameter A -parameter B -parameter C))
   (UNIT_NAME lib2))
  Lib2: _build/default/lib2/lib2.ml
  ((INDEX $TESTCASE_ROOT/_build/default/a/.a.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/b/.b.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/c/.c.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/lib/.lib.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/lib2/.lib2.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/utils/.utils.objs/cctx.ocaml-index)
   (STDLIB /home/sudha/.opam/oxcaml2/lib/ocaml)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B $TESTCASE_ROOT/_build/default/a/.a.objs/byte)
   (B $TESTCASE_ROOT/_build/default/b/.b.objs/byte)
   (B $TESTCASE_ROOT/_build/default/c/.c.objs/byte)
   (B $TESTCASE_ROOT/_build/default/lib/.lib.objs/byte)
   (B $TESTCASE_ROOT/_build/default/lib2/.lib2.objs/byte)
   (B $TESTCASE_ROOT/_build/default/utils/.utils.objs/byte)
   (S $TESTCASE_ROOT/a)
   (S $TESTCASE_ROOT/b)
   (S $TESTCASE_ROOT/c)
   (S $TESTCASE_ROOT/lib)
   (S $TESTCASE_ROOT/lib2)
   (S $TESTCASE_ROOT/utils)
   (FLG (-w @1..3@5..28@31..39@43@46..47@49..57@61..62@67@69-40 -strict-sequence -strict-formats -short-paths -keep-locs -g))
   (FLG (-parameter A -parameter B -parameter C))
   (UNIT_NAME lib2))
