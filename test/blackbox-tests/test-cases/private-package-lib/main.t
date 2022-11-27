This test demonstrates private libraries that belong to a package. Such
libraries are installed as public libraries under the package.__private__.<name>
findlib name, and are only available to libraries and executables in the same
package.

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (package (name foo))
  > (package (name bar))
  > EOF

First, we define a private library:

  $ mkdir private
  $ cat >private/secret.ml <<EOF
  > let secret = "secret string"
  > EOF
  $ cat >private/dune <<EOF
  > (library
  >  (name secret)
  >  (package foo))
  > EOF
  $ dune build @all

A public library may build against it:

  $ mkdir public
  $ cat >public/dune <<EOF
  > (library
  >  (name foo)
  >  (libraries secret)
  >  (public_name foo.bar))
  > EOF
  $ cat >public/foo.ml <<EOF
  > let foo = "from library foo " ^ Secret.secret
  > EOF
  $ dune build @install

The naming convention puts the artifacts of private libs under __private__:

  $ ls _build/install/default/lib/foo/__private__/secret | grep -i \.cm
  secret.cma
  secret.cmx
  secret.cmxa
  secret.cmxs

Note the name mangling convention in the META file:

  $ cat _build/install/default/lib/foo/META
  package "__private__" (
    directory = "__private__"
    package "secret" (
      directory = "secret"
      description = ""
      requires = ""
      archive(byte) = "secret.cma"
      archive(native) = "secret.cmxa"
      plugin(byte) = "secret.cma"
      plugin(native) = "secret.cmxs"
    )
  )
  package "bar" (
    directory = "bar"
    description = ""
    requires = "foo.__private__.secret"
    archive(byte) = "foo.cma"
    archive(native) = "foo.cmxa"
    plugin(byte) = "foo.cma"
    plugin(native) = "foo.cmxs"
  )

We want to see mangled names in the dune-package file as well:

  $ cat _build/install/default/lib/foo/dune-package | grep __private__
     __private__/secret/.public_cmi/secret.cmi
     __private__/secret/.public_cmi/secret.cmt
     __private__/secret/secret.a
     __private__/secret/secret.cma
     __private__/secret/secret.cmx
     __private__/secret/secret.cmxa
     __private__/secret/secret.ml
   (libexec (__private__/secret/secret.cmxs bar/foo.cmxs)))
   (requires foo.__private__.secret)
   (name foo.__private__.secret)
    (byte __private__/secret/secret.cma)
    (native __private__/secret/secret.cmxa))
    (byte __private__/secret/secret.cma)
    (native __private__/secret/secret.cmxs))
   (native_archives __private__/secret/secret.a)

Cmi for secret library must not be visible for normal users. Hence they must be
hidden.

  $ grep __private__ _build/default/foo.install
    "_build/install/default/lib/foo/__private__/secret/.public_cmi/secret.cmi" {"__private__/secret/.public_cmi/secret.cmi"}
    "_build/install/default/lib/foo/__private__/secret/.public_cmi/secret.cmt" {"__private__/secret/.public_cmi/secret.cmt"}
    "_build/install/default/lib/foo/__private__/secret/secret.a" {"__private__/secret/secret.a"}
    "_build/install/default/lib/foo/__private__/secret/secret.cma" {"__private__/secret/secret.cma"}
    "_build/install/default/lib/foo/__private__/secret/secret.cmx" {"__private__/secret/secret.cmx"}
    "_build/install/default/lib/foo/__private__/secret/secret.cmxa" {"__private__/secret/secret.cmxa"}
    "_build/install/default/lib/foo/__private__/secret/secret.ml" {"__private__/secret/secret.ml"}
    "_build/install/default/lib/foo/__private__/secret/secret.cmxs" {"__private__/secret/secret.cmxs"}

We make sure that executables can use the secret library like they can use any other private library

  $ mkdir bar
  $ cat >bar/dune <<EOF
  > (executable
  >  (name bar)
  >  (libraries secret))
  > EOF
  $ cat >bar/bar.ml <<EOF
  > print_endline "from bar.ml"
  > EOF
  $ dune exec ./bar/bar.exe
  from bar.ml

Now we try to use the library in a subproject:

  $ mkdir subproj
  $ echo "(lang dune 2.8)" > subproj/dune-project
  $ cat >subproj/dune <<EOF
  > (executable
  >  (name subproj)
  >  (libraries foo.bar))
  > EOF
  $ cat >subproj/subproj.ml <<EOF
  > print_endline Foo.foo
  > EOF
  $ dune exec ./subproj/subproj.exe
  from library foo secret string

But we shouldn't be able to access it directly:

  $ cat >subproj/subproj.ml <<EOF
  > print_endline Secret.secret
  > EOF
  $ dune exec ./subproj/subproj.exe
  File "subproj/subproj.ml", line 1, characters 14-27:
  1 | print_endline Secret.secret
                    ^^^^^^^^^^^^^
  Error: Unbound module Secret
  [1]

Now we make sure such libraries are transitively usable when installed:

  $ mkdir use
  $ cat >use/dune <<EOF
  > (executable
  >  (name run)
  >  (libraries foo.bar))
  > EOF
  $ cat >use/run.ml <<EOF
  > print_endline ("Using library foo: " ^ Foo.foo)
  > EOF
  $ echo "(lang dune 2.8)" > use/dune-project
  $ export OCAMLPATH=$PWD/_build/install/default/lib
  $ dune exec --root use -- ./run.exe
  Entering directory 'use'
  Leaving directory 'use'
  Using library foo: from library foo secret string

But we cannot use such libraries directly:

  $ cat >use/run.ml <<EOF
  > print_endline ("direct access attempt: " ^ Secret.secret)
  > EOF
  $ dune exec --root use -- ./run.exe
  Entering directory 'use'
  File "run.ml", line 1, characters 43-56:
  1 | print_endline ("direct access attempt: " ^ Secret.secret)
                                                 ^^^^^^^^^^^^^
  Error: Unbound module Secret
  Leaving directory 'use'
  [1]
