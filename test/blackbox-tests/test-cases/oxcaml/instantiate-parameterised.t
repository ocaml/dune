Testing the instantiation of parameterised libraries. This feature requires oxcaml:

  $ cat >> dune-project <<EOF
  > (lang dune 3.20)
  > (using oxcaml 0.1)
  > (package (name project))
  > EOF

We add two parameters:

  $ mkdir a
  $ echo 'val a : string' > a/a.mli
  $ cat >a/dune <<EOF
  > (library_parameter (public_name project.a) (name a))
  > EOF

  $ mkdir b
  $ echo 'val b : string' > b/b.mli
  $ cat >b/dune <<EOF
  > (library_parameter (public_name project.b) (name b))
  > EOF

And two implementations, one with a singleton module and the other with more:

  $ mkdir a_impl
  $ echo 'let a = "a"' > a_impl/a_impl.ml
  $ cat >a_impl/dune <<EOF
  > (library (public_name project.a_impl) (name a_impl) (implements a))
  > EOF

  $ mkdir b_impl
  $ echo 'let b = "b"' > b_impl/b_dep.ml
  $ echo 'let b = B_dep.b' > b_impl/b_impl.ml
  $ cat >b_impl/dune <<EOF
  > (library (public_name project.b_impl) (name b_impl) (implements b))
  > EOF

And a parameterised library:

  $ mkdir lib_ab
  $ echo 'let ab = A.a ^ B.b' > lib_ab/lib_ab.ml
  $ cat >lib_ab/dune <<EOF
  > (library (public_name project.lib_ab) (name lib_ab) (parameters a b))
  > EOF

  $ dune build

Finally the binary can instantiate `lib_ab` by providing all its parameters:

  $ mkdir bin
  $ echo 'let () = print_endline Lib_ab.ab' > bin/bin.ml
  $ cat >bin/dune <<EOF
  > (executable
  >   (public_name project.bin) (name bin)
  >   (libraries (instantiate lib_ab b_impl a_impl)))
  > EOF

  $ dune exec project.bin
  ab

It's an error for the binary to partially instantiate `lib_ab`:

  $ cat >bin/dune <<EOF
  > (executable
  >   (public_name project.bin) (name bin)
  >   (libraries (instantiate lib_ab b_impl))) ; missing a_impl
  > EOF

  $ dune exec project.bin
  File "bin/dune", line 3, characters 26-32:
  3 |   (libraries (instantiate lib_ab b_impl))) ; missing a_impl
                                ^^^^^^
  Error: Missing argument for parameter "project.a".
  -> required by _build/default/bin/bin.exe
  -> required by _build/install/default/bin/project.bin
  Hint: Pass an argument implementing "project.a" to the dependency.
  [1]

It's an error to instantiate twice without renamming: (dune might be able to
catch this, but currently it doesn't check that libraries don't have
overlapping modules)

  $ cat >bin/dune <<EOF
  > (executable
  >   (public_name project.bin) (name bin)
  >   (libraries
  >     a_impl
  >     project.a_impl
  >     (instantiate lib_ab a_impl b_impl)
  >     (instantiate lib_ab a_impl b_impl)))
  > EOF

  $ dune exec project.bin
  File "bin/dune", line 7, characters 17-23:
  7 |     (instantiate lib_ab a_impl b_impl)))
                       ^^^^^^
  Error: The instance name Lib_ab is already used.
  -> required by _build/default/bin/.bin.eobjs/dune__exe.ml-gen
  -> required by _build/default/bin/.bin.eobjs/byte/dune__exe.cmi
  -> required by _build/default/bin/.bin.eobjs/native/dune__exe.cmx
  -> required by _build/default/bin/bin.exe
  -> required by _build/install/default/bin/project.bin
  [1]

We add another way to implement the parameter `b` from the parameter `a`:

  $ mkdir a_of_b
  $ echo 'let a = "a_of_b(" ^ B.b ^ ")"' > a_of_b/a_of_b.ml
  $ cat >a_of_b/dune <<EOF
  > (library
  >   (public_name project.a_of_b)
  >   (name a_of_b)
  >   (parameters b)
  >   (implements a))
  > EOF

It's an error to use `a_of_b` as an argument for `lib_ab` in the executable
dependencies, because its parameter `b` is missing:

  $ cat >bin/dune <<EOF
  > (executable
  >   (public_name project.bin) (name bin)
  >   (libraries
  >     a_impl
  >     project.a_impl
  >     (instantiate lib_ab a_impl a_of_b)))
  > EOF

  $ dune exec project.bin
  File "bin/dune", line 6, characters 31-37:
  6 |     (instantiate lib_ab a_impl a_of_b)))
                                     ^^^^^^
  Error: Duplicate arguments project.a_impl and project.a_of_b for parameter
  project.a.
  -> required by _build/default/bin/.bin.eobjs/dune__exe.ml-gen
  -> required by _build/default/bin/.bin.eobjs/byte/dune__exe.cmi
  -> required by _build/default/bin/.bin.eobjs/native/dune__exe.cmx
  -> required by _build/default/bin/bin.exe
  -> required by _build/install/default/bin/project.bin
  File "bin/dune", line 6, characters 31-37:
  6 |     (instantiate lib_ab a_impl a_of_b)))
                                     ^^^^^^
  Error: Missing argument for parameter "project.b".
  -> required by _build/default/bin/.bin.eobjs/native/dune__exe__Bin.cmx
  -> required by _build/default/bin/bin.exe
  -> required by _build/install/default/bin/project.bin
  Hint: Pass an argument implementing "project.b" to the dependency.
  [1]

However `lib_ab` can depend on `a_of_b`, such that the parameter `b` will be
implicitly passed to it:

  $ echo 'let ab = A.a ^ "," ^ A_of_b.a ^ "," ^ B.b' > lib_ab/lib_ab.ml
  $ cat >lib_ab/dune <<EOF
  > (library (public_name project.lib_ab) (name lib_ab)
  >   (parameters a b)
  >   (libraries a_of_b))
  > EOF

  $ cat >bin/dune <<EOF
  > (executable
  >   (public_name project.bin) (name bin)
  >   (libraries
  >     (instantiate lib_ab a_impl b_impl)))
  > EOF

  $ dune exec project.bin
  a,a_of_b(b),b

We can also do a partial application of `lib_ab`:

  $ mkdir lib_apply
  $ echo 'let ab = "lib_apply(" ^ Lib_ab.ab ^ ")"' > lib_apply/lib_apply.ml
  $ cat >lib_apply/dune <<EOF
  > (library
  >   (public_name project.lib_apply)
  >   (name lib_apply)
  >   (parameters b)
  >   (libraries (instantiate lib_ab a_impl)))
  > EOF

And use renaming with `:as` in the executable:

  $ cat >bin/dune <<EOF
  > (executable
  >   (public_name project.bin) (name bin)
  >   (libraries
  >     (instantiate lib_apply b_impl :as lib_ab)))
  > EOF

  $ dune exec project.bin
  lib_apply(a,a_of_b(b),b)

It's an error to provide a non-required parameter:

  $ cat >bin/dune <<EOF
  > (executable
  >   (public_name project.bin) (name bin)
  >   (libraries
  >     (instantiate lib_apply a_impl b_impl :as lib_ab)))
  > EOF

  $ dune exec project.bin
  File "bin/dune", line 4, characters 27-33:
  4 |     (instantiate lib_apply a_impl b_impl :as lib_ab)))
                                 ^^^^^^
  Error: Argument project.a implements unexpected parameter project.a_impl
  -> required by _build/default/bin/.bin.eobjs/dune__exe.ml-gen
  -> required by _build/default/bin/.bin.eobjs/byte/dune__exe.cmi
  -> required by _build/default/bin/.bin.eobjs/native/dune__exe.cmx
  -> required by _build/default/bin/bin.exe
  -> required by _build/install/default/bin/project.bin
  Hint: Remove this argument
  [1]

Given another implementation of a parameter,

  $ mkdir b_impl2
  $ echo 'let b = "b2"' > b_impl2/b_impl2.ml
  $ cat >b_impl2/dune <<EOF
  > (library (public_name project.b_impl2) (name b_impl2) (implements b))
  > EOF

It's an error to instantiate a library with arguments that
implement the same parameter `b`, because it would be ambiguous
which one to use:

  $ cat >bin/dune <<EOF
  > (executable
  >   (public_name project.bin) (name bin)
  >   (libraries
  >     (instantiate lib_apply b_impl b_impl2)))
  > EOF
  $ dune exec project.bin
  File "bin/dune", line 4, characters 34-41:
  4 |     (instantiate lib_apply b_impl b_impl2)))
                                        ^^^^^^^
  Error: Duplicate arguments project.b_impl and project.b_impl2 for parameter
  project.b.
  -> required by _build/default/bin/.bin.eobjs/dune__exe.ml-gen
  -> required by _build/default/bin/.bin.eobjs/byte/dune__exe.cmi
  -> required by _build/default/bin/.bin.eobjs/native/dune__exe.cmx
  -> required by _build/default/bin/bin.exe
  -> required by _build/install/default/bin/project.bin
  [1]

Same error if the argument is repeated:

  $ cat >bin/dune <<EOF
  > (executable
  >   (public_name project.bin) (name bin)
  >   (libraries
  >     (instantiate lib_apply b_impl b_impl)))
  > EOF
  $ dune exec project.bin
  File "bin/dune", line 4, characters 34-40:
  4 |     (instantiate lib_apply b_impl b_impl)))
                                        ^^^^^^
  Error: Duplicate arguments project.b_impl and project.b_impl for parameter
  project.b.
  -> required by _build/default/bin/.bin.eobjs/dune__exe.ml-gen
  -> required by _build/default/bin/.bin.eobjs/byte/dune__exe.cmi
  -> required by _build/default/bin/.bin.eobjs/native/dune__exe.cmx
  -> required by _build/default/bin/bin.exe
  -> required by _build/install/default/bin/project.bin
  [1]

We can instantiate the same library multiple times by giving it different names:

  $ cat >bin/dune <<EOF
  > (executable
  >   (public_name project.bin) (name bin)
  >   (libraries
  >     (instantiate lib_apply b_impl  :as applied_b)
  >     (instantiate lib_apply b_impl2 :as applied_b2)))
  > EOF

  $ echo 'let () = print_endline Applied_b.ab'  >  bin/bin.ml
  $ echo 'let () = print_endline Applied_b2.ab' >> bin/bin.ml

  $ dune exec project.bin
  lib_apply(a,a_of_b(b),b)
  lib_apply(a,a_of_b(b2),b2)

We can also instantiate multiple times at the level of libraries:

  $ cat > lib_apply/lib_apply.ml <<EOF
  > let ab = "lib_ab[a,?](" ^ Lib_a_.ab ^ ")"
  >       ^ " lib_ab[a,b](" ^ Lib_a_b.ab ^ ")"
  >       ^ " lib_ab[a_of_b[?],?](" ^ Lib_aofb_.ab ^ ")"
  >       ^ " lib_ab[a_of_b[?],b](" ^ Lib_aofb_b.ab ^ ")"
  > EOF
  $ cat >lib_apply/dune <<EOF
  > (library (public_name project.lib_apply) (name lib_apply)
  >   (parameters b)
  >   (libraries
  >     (instantiate lib_ab a_impl        :as lib_a_)     ; partial application
  >     (instantiate lib_ab a_impl b_impl :as lib_a_b)    ; full application
  >     (instantiate lib_ab a_of_b        :as lib_aofb_)  ; double partial application
  >     (instantiate lib_ab a_of_b b_impl :as lib_aofb_b) ; partial application on first argument
  > ))
  > EOF

  $ dune exec project.bin
  lib_ab[a,?](a,a_of_b(b),b) lib_ab[a,b](a,a_of_b(b),b) lib_ab[a_of_b[?],?](a_of_b(b),a_of_b(b),b) lib_ab[a_of_b[?],b](a_of_b(b),a_of_b(b),b)
  lib_ab[a,?](a,a_of_b(b2),b2) lib_ab[a,b](a,a_of_b(b),b) lib_ab[a_of_b[?],?](a_of_b(b2),a_of_b(b2),b2) lib_ab[a_of_b[?],b](a_of_b(b2),a_of_b(b),b)

The same but using multiple files in the definition of `lib_apply`, with
arbitrary module dependencies: (as behind the scene, to instantiate the
library, each module must be instantiated separately and in dependency order)

  $ echo 'let ab = X.x ^ " " ^ Z.z' > lib_apply/lib_apply.ml
  $ echo 'let x = "lib_ab[a,?](" ^ Lib_a_.ab ^ ") " ^ Y.y' > lib_apply/x.ml
  $ echo 'let y = "lib_ab[a,b](" ^ Lib_a_b.ab ^ ") " ^ F.f' > lib_apply/y.ml
  $ echo 'let f = "lib_ab[a_of_b[?],?](" ^ Lib_aofb_.ab ^ ")"' > lib_apply/f.ml
  $ echo 'let z = "lib_ab[a_of_b[?],b](" ^ Lib_aofb_b.ab ^ ")"' > lib_apply/z.ml
  $ dune exec project.bin
  lib_ab[a,?](a,a_of_b(b),b) lib_ab[a,b](a,a_of_b(b),b) lib_ab[a_of_b[?],?](a_of_b(b),a_of_b(b),b) lib_ab[a_of_b[?],b](a_of_b(b),a_of_b(b),b)
  lib_ab[a,?](a,a_of_b(b2),b2) lib_ab[a,b](a,a_of_b(b),b) lib_ab[a_of_b[?],?](a_of_b(b2),a_of_b(b2),b2) lib_ab[a_of_b[?],b](a_of_b(b2),a_of_b(b),b)

The same compilation also works in bytecode:

  $ cat >bin/dune <<EOF
  > (executable
  >   (public_name project.bin) (name bin)
  >   (modes byte)
  >   (libraries
  >     (instantiate lib_apply b_impl :as applied_b)
  >     (instantiate lib_apply b_impl2 :as applied_b2)))
  > EOF
  $ dune exec project.bin
  lib_ab[a,?](a,a_of_b(b),b) lib_ab[a,b](a,a_of_b(b),b) lib_ab[a_of_b[?],?](a_of_b(b),a_of_b(b),b) lib_ab[a_of_b[?],b](a_of_b(b),a_of_b(b),b)
  lib_ab[a,?](a,a_of_b(b2),b2) lib_ab[a,b](a,a_of_b(b),b) lib_ab[a_of_b[?],?](a_of_b(b2),a_of_b(b2),b2) lib_ab[a_of_b[?],b](a_of_b(b2),a_of_b(b),b)

We check the installation:

  $ dune build @install
  $ cat _build/install/default/lib/project/META
  package "a" (
    directory = "a"
    description = ""
    requires = ""
    archive(byte) = ""
    archive(native) = ""
    plugin(byte) = ""
    plugin(native) = ""
  )
  package "a_impl" (
    directory = "a_impl"
    description = ""
    requires = "project.a"
    archive(byte) = "a_impl.cma"
    archive(native) = "a_impl.cmxa"
    plugin(byte) = "a_impl.cma"
    plugin(native) = "a_impl.cmxs"
  )
  package "a_of_b" (
    directory = "a_of_b"
    description = ""
    requires = "project.a project.b"
    archive(byte) = "a_of_b.cma"
    archive(native) = "a_of_b.cmxa"
    plugin(byte) = "a_of_b.cma"
    plugin(native) = "a_of_b.cmxs"
  )
  package "b" (
    directory = "b"
    description = ""
    requires = ""
    archive(byte) = ""
    archive(native) = ""
    plugin(byte) = ""
    plugin(native) = ""
  )
  package "b_impl" (
    directory = "b_impl"
    description = ""
    requires = "project.b"
    archive(byte) = "b_impl.cma"
    archive(native) = "b_impl.cmxa"
    plugin(byte) = "b_impl.cma"
    plugin(native) = "b_impl.cmxs"
  )
  package "b_impl2" (
    directory = "b_impl2"
    description = ""
    requires = "project.b"
    archive(byte) = "b_impl2.cma"
    archive(native) = "b_impl2.cmxa"
    plugin(byte) = "b_impl2.cma"
    plugin(native) = "b_impl2.cmxs"
  )
  package "lib_ab" (
    directory = "lib_ab"
    description = ""
    requires = "project.a project.a_of_b project.b"
    archive(byte) = "lib_ab.cma"
    archive(native) = "lib_ab.cmxa"
    plugin(byte) = "lib_ab.cma"
    plugin(native) = "lib_ab.cmxs"
  )
  package "lib_apply" (
    directory = "lib_apply"
    description = ""
    requires =
    "project.a_impl project.a_of_b project.b project.b_impl project.lib_ab"
    archive(byte) = "lib_apply.cma"
    archive(native) = "lib_apply.cmxa"
    plugin(byte) = "lib_apply.cma"
    plugin(native) = "lib_apply.cmxs"
  )

The `dune-package` should list the different instantiations in the dependencies:

  $ cat _build/install/default/lib/project/dune-package | grep -v 'lang dune'
  (name project)
  (sections (lib .) (libexec .) (bin ../../bin))
  (files
   (lib
    (META
     a/a.cmi
     a/a.cmti
     a/a.mli
     a_impl/a_impl.a
     a_impl/a_impl.cma
     a_impl/a_impl.cmi
     a_impl/a_impl.cmt
     a_impl/a_impl.cmx
     a_impl/a_impl.cmxa
     a_impl/a_impl.ml
     a_impl/a_impl__a_impl__.cmi
     a_impl/a_impl__a_impl__.cmt
     a_impl/a_impl__a_impl__.cmx
     a_impl/a_impl__a_impl__.ml
     a_of_b/a_of_b.a
     a_of_b/a_of_b.cma
     a_of_b/a_of_b.cmi
     a_of_b/a_of_b.cmt
     a_of_b/a_of_b.cmx
     a_of_b/a_of_b.cmxa
     a_of_b/a_of_b.ml
     a_of_b/a_of_b__a_of_b__.cmi
     a_of_b/a_of_b__a_of_b__.cmt
     a_of_b/a_of_b__a_of_b__.cmx
     a_of_b/a_of_b__a_of_b__.ml
     b/b.cmi
     b/b.cmti
     b/b.mli
     b_impl/b_dep.ml
     b_impl/b_impl.a
     b_impl/b_impl.cma
     b_impl/b_impl.cmi
     b_impl/b_impl.cmt
     b_impl/b_impl.cmx
     b_impl/b_impl.cmxa
     b_impl/b_impl.ml
     b_impl/b_impl__B_dep.cmi
     b_impl/b_impl__B_dep.cmt
     b_impl/b_impl__B_dep.cmx
     b_impl/b_impl__b_impl__.cmi
     b_impl/b_impl__b_impl__.cmt
     b_impl/b_impl__b_impl__.cmx
     b_impl/b_impl__b_impl__.ml
     b_impl2/b_impl2.a
     b_impl2/b_impl2.cma
     b_impl2/b_impl2.cmi
     b_impl2/b_impl2.cmt
     b_impl2/b_impl2.cmx
     b_impl2/b_impl2.cmxa
     b_impl2/b_impl2.ml
     b_impl2/b_impl2__b_impl2__.cmi
     b_impl2/b_impl2__b_impl2__.cmt
     b_impl2/b_impl2__b_impl2__.cmx
     b_impl2/b_impl2__b_impl2__.ml
     dune-package
     lib_ab/lib_ab.a
     lib_ab/lib_ab.cma
     lib_ab/lib_ab.cmi
     lib_ab/lib_ab.cmt
     lib_ab/lib_ab.cmx
     lib_ab/lib_ab.cmxa
     lib_ab/lib_ab.ml
     lib_apply/f.ml
     lib_apply/lib_apply.a
     lib_apply/lib_apply.cma
     lib_apply/lib_apply.cmi
     lib_apply/lib_apply.cmt
     lib_apply/lib_apply.cmx
     lib_apply/lib_apply.cmxa
     lib_apply/lib_apply.ml
     lib_apply/lib_apply__.cmi
     lib_apply/lib_apply__.cmt
     lib_apply/lib_apply__.cmx
     lib_apply/lib_apply__.ml
     lib_apply/lib_apply__F.cmi
     lib_apply/lib_apply__F.cmt
     lib_apply/lib_apply__F.cmx
     lib_apply/lib_apply__X.cmi
     lib_apply/lib_apply__X.cmt
     lib_apply/lib_apply__X.cmx
     lib_apply/lib_apply__Y.cmi
     lib_apply/lib_apply__Y.cmt
     lib_apply/lib_apply__Y.cmx
     lib_apply/lib_apply__Z.cmi
     lib_apply/lib_apply__Z.cmt
     lib_apply/lib_apply__Z.cmx
     lib_apply/x.ml
     lib_apply/y.ml
     lib_apply/z.ml))
   (libexec
    (a_impl/a_impl.cmxs
     a_of_b/a_of_b.cmxs
     b_impl/b_impl.cmxs
     b_impl2/b_impl2.cmxs
     lib_ab/lib_ab.cmxs
     lib_apply/lib_apply.cmxs))
   (bin (project.bin)))
  (library
   (name project.a)
   (kind parameter)
   (main_module_name A)
   (modes byte)
   (modules
    (singleton
     (obj_name a)
     (visibility public)
     (kind parameter)
     (source (path A) (intf (path a/a.mli))))))
  (library
   (name project.a_impl)
   (kind normal)
   (archives (byte a_impl/a_impl.cma) (native a_impl/a_impl.cmxa))
   (plugins (byte a_impl/a_impl.cma) (native a_impl/a_impl.cmxs))
   (native_archives a_impl/a_impl.a)
   (requires project.a)
   (implements project.a)
   (main_module_name A_impl)
   (modes byte native)
   (modules
    (wrapped
     (group
      (alias
       (obj_name a_impl__a_impl__)
       (visibility public)
       (kind alias)
       (source
        (path A_impl__a_impl__)
        (impl (path a_impl/a_impl__a_impl__.ml-gen))))
      (name A_impl)
      (modules
       (module
        (obj_name a_impl)
        (visibility public)
        (source (path A_impl) (impl (path a_impl/a_impl.ml))))))
     (wrapped true))))
  (library
   (name project.a_of_b)
   (kind normal)
   (archives (byte a_of_b/a_of_b.cma) (native a_of_b/a_of_b.cmxa))
   (plugins (byte a_of_b/a_of_b.cma) (native a_of_b/a_of_b.cmxs))
   (native_archives a_of_b/a_of_b.a)
   (requires project.a project.b)
   (parameters project.b)
   (implements project.a)
   (main_module_name A_of_b)
   (modes byte native)
   (modules
    (wrapped
     (group
      (alias
       (obj_name a_of_b__a_of_b__)
       (visibility public)
       (kind alias)
       (source
        (path A_of_b__a_of_b__)
        (impl (path a_of_b/a_of_b__a_of_b__.ml-gen))))
      (name A_of_b)
      (modules
       (module
        (obj_name a_of_b)
        (visibility public)
        (source (path A_of_b) (impl (path a_of_b/a_of_b.ml))))))
     (wrapped true))))
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
   (name project.b_impl)
   (kind normal)
   (archives (byte b_impl/b_impl.cma) (native b_impl/b_impl.cmxa))
   (plugins (byte b_impl/b_impl.cma) (native b_impl/b_impl.cmxs))
   (native_archives b_impl/b_impl.a)
   (requires project.b)
   (implements project.b)
   (main_module_name B_impl)
   (modes byte native)
   (modules
    (wrapped
     (group
      (alias
       (obj_name b_impl__b_impl__)
       (visibility public)
       (kind alias)
       (source
        (path B_impl__b_impl__)
        (impl (path b_impl/b_impl__b_impl__.ml-gen))))
      (name B_impl)
      (modules
       (module
        (obj_name b_impl__B_dep)
        (visibility public)
        (source (path B_dep) (impl (path b_impl/b_dep.ml))))
       (module
        (obj_name b_impl)
        (visibility public)
        (source (path B_impl) (impl (path b_impl/b_impl.ml))))))
     (wrapped true))))
  (library
   (name project.b_impl2)
   (kind normal)
   (archives (byte b_impl2/b_impl2.cma) (native b_impl2/b_impl2.cmxa))
   (plugins (byte b_impl2/b_impl2.cma) (native b_impl2/b_impl2.cmxs))
   (native_archives b_impl2/b_impl2.a)
   (requires project.b)
   (implements project.b)
   (main_module_name B_impl2)
   (modes byte native)
   (modules
    (wrapped
     (group
      (alias
       (obj_name b_impl2__b_impl2__)
       (visibility public)
       (kind alias)
       (source
        (path B_impl2__b_impl2__)
        (impl (path b_impl2/b_impl2__b_impl2__.ml-gen))))
      (name B_impl2)
      (modules
       (module
        (obj_name b_impl2)
        (visibility public)
        (source (path B_impl2) (impl (path b_impl2/b_impl2.ml))))))
     (wrapped true))))
  (library
   (name project.lib_ab)
   (kind normal)
   (archives (byte lib_ab/lib_ab.cma) (native lib_ab/lib_ab.cmxa))
   (plugins (byte lib_ab/lib_ab.cma) (native lib_ab/lib_ab.cmxs))
   (native_archives lib_ab/lib_ab.a)
   (requires project.a project.b (instantiate project.a_of_b))
   (parameters project.a project.b)
   (main_module_name Lib_ab)
   (modes byte native)
   (modules
    (singleton
     (obj_name lib_ab)
     (visibility public)
     (source (path Lib_ab) (impl (path lib_ab/lib_ab.ml))))))
  (library
   (name project.lib_apply)
   (kind normal)
   (archives (byte lib_apply/lib_apply.cma) (native lib_apply/lib_apply.cmxa))
   (plugins (byte lib_apply/lib_apply.cma) (native lib_apply/lib_apply.cmxs))
   (native_archives lib_apply/lib_apply.a)
   (requires
    project.b
    project.a_impl
    (instantiate project.lib_ab project.a_impl)
    project.b_impl
    (instantiate project.lib_ab project.a_impl project.b_impl)
    (instantiate project.a_of_b)
    (instantiate project.lib_ab project.a_of_b)
    (instantiate project.lib_ab project.a_of_b project.b_impl))
   (parameters project.b)
   (main_module_name Lib_apply)
   (modes byte native)
   (modules
    (wrapped
     (group
      (alias
       (obj_name lib_apply__)
       (visibility public)
       (kind alias)
       (source (path Lib_apply__) (impl (path lib_apply/lib_apply__.ml-gen))))
      (name Lib_apply)
      (modules
       (module
        (obj_name lib_apply__F)
        (visibility public)
        (source (path F) (impl (path lib_apply/f.ml))))
       (module
        (obj_name lib_apply)
        (visibility public)
        (source (path Lib_apply) (impl (path lib_apply/lib_apply.ml))))
       (module
        (obj_name lib_apply__X)
        (visibility public)
        (source (path X) (impl (path lib_apply/x.ml))))
       (module
        (obj_name lib_apply__Y)
        (visibility public)
        (source (path Y) (impl (path lib_apply/y.ml))))
       (module
        (obj_name lib_apply__Z)
        (visibility public)
        (source (path Z) (impl (path lib_apply/z.ml))))))
     (wrapped true))))

And all the required files should be installed:

  $ cat _build/default/project.install
  lib: [
    "_build/install/default/lib/project/META"
    "_build/install/default/lib/project/a/a.cmi" {"a/a.cmi"}
    "_build/install/default/lib/project/a/a.cmti" {"a/a.cmti"}
    "_build/install/default/lib/project/a/a.mli" {"a/a.mli"}
    "_build/install/default/lib/project/a_impl/a_impl.a" {"a_impl/a_impl.a"}
    "_build/install/default/lib/project/a_impl/a_impl.cma" {"a_impl/a_impl.cma"}
    "_build/install/default/lib/project/a_impl/a_impl.cmi" {"a_impl/a_impl.cmi"}
    "_build/install/default/lib/project/a_impl/a_impl.cmt" {"a_impl/a_impl.cmt"}
    "_build/install/default/lib/project/a_impl/a_impl.cmx" {"a_impl/a_impl.cmx"}
    "_build/install/default/lib/project/a_impl/a_impl.cmxa" {"a_impl/a_impl.cmxa"}
    "_build/install/default/lib/project/a_impl/a_impl.ml" {"a_impl/a_impl.ml"}
    "_build/install/default/lib/project/a_impl/a_impl__a_impl__.cmi" {"a_impl/a_impl__a_impl__.cmi"}
    "_build/install/default/lib/project/a_impl/a_impl__a_impl__.cmt" {"a_impl/a_impl__a_impl__.cmt"}
    "_build/install/default/lib/project/a_impl/a_impl__a_impl__.cmx" {"a_impl/a_impl__a_impl__.cmx"}
    "_build/install/default/lib/project/a_impl/a_impl__a_impl__.ml" {"a_impl/a_impl__a_impl__.ml"}
    "_build/install/default/lib/project/a_of_b/a_of_b.a" {"a_of_b/a_of_b.a"}
    "_build/install/default/lib/project/a_of_b/a_of_b.cma" {"a_of_b/a_of_b.cma"}
    "_build/install/default/lib/project/a_of_b/a_of_b.cmi" {"a_of_b/a_of_b.cmi"}
    "_build/install/default/lib/project/a_of_b/a_of_b.cmt" {"a_of_b/a_of_b.cmt"}
    "_build/install/default/lib/project/a_of_b/a_of_b.cmx" {"a_of_b/a_of_b.cmx"}
    "_build/install/default/lib/project/a_of_b/a_of_b.cmxa" {"a_of_b/a_of_b.cmxa"}
    "_build/install/default/lib/project/a_of_b/a_of_b.ml" {"a_of_b/a_of_b.ml"}
    "_build/install/default/lib/project/a_of_b/a_of_b__a_of_b__.cmi" {"a_of_b/a_of_b__a_of_b__.cmi"}
    "_build/install/default/lib/project/a_of_b/a_of_b__a_of_b__.cmt" {"a_of_b/a_of_b__a_of_b__.cmt"}
    "_build/install/default/lib/project/a_of_b/a_of_b__a_of_b__.cmx" {"a_of_b/a_of_b__a_of_b__.cmx"}
    "_build/install/default/lib/project/a_of_b/a_of_b__a_of_b__.ml" {"a_of_b/a_of_b__a_of_b__.ml"}
    "_build/install/default/lib/project/b/b.cmi" {"b/b.cmi"}
    "_build/install/default/lib/project/b/b.cmti" {"b/b.cmti"}
    "_build/install/default/lib/project/b/b.mli" {"b/b.mli"}
    "_build/install/default/lib/project/b_impl/b_dep.ml" {"b_impl/b_dep.ml"}
    "_build/install/default/lib/project/b_impl/b_impl.a" {"b_impl/b_impl.a"}
    "_build/install/default/lib/project/b_impl/b_impl.cma" {"b_impl/b_impl.cma"}
    "_build/install/default/lib/project/b_impl/b_impl.cmi" {"b_impl/b_impl.cmi"}
    "_build/install/default/lib/project/b_impl/b_impl.cmt" {"b_impl/b_impl.cmt"}
    "_build/install/default/lib/project/b_impl/b_impl.cmx" {"b_impl/b_impl.cmx"}
    "_build/install/default/lib/project/b_impl/b_impl.cmxa" {"b_impl/b_impl.cmxa"}
    "_build/install/default/lib/project/b_impl/b_impl.ml" {"b_impl/b_impl.ml"}
    "_build/install/default/lib/project/b_impl/b_impl__B_dep.cmi" {"b_impl/b_impl__B_dep.cmi"}
    "_build/install/default/lib/project/b_impl/b_impl__B_dep.cmt" {"b_impl/b_impl__B_dep.cmt"}
    "_build/install/default/lib/project/b_impl/b_impl__B_dep.cmx" {"b_impl/b_impl__B_dep.cmx"}
    "_build/install/default/lib/project/b_impl/b_impl__b_impl__.cmi" {"b_impl/b_impl__b_impl__.cmi"}
    "_build/install/default/lib/project/b_impl/b_impl__b_impl__.cmt" {"b_impl/b_impl__b_impl__.cmt"}
    "_build/install/default/lib/project/b_impl/b_impl__b_impl__.cmx" {"b_impl/b_impl__b_impl__.cmx"}
    "_build/install/default/lib/project/b_impl/b_impl__b_impl__.ml" {"b_impl/b_impl__b_impl__.ml"}
    "_build/install/default/lib/project/b_impl2/b_impl2.a" {"b_impl2/b_impl2.a"}
    "_build/install/default/lib/project/b_impl2/b_impl2.cma" {"b_impl2/b_impl2.cma"}
    "_build/install/default/lib/project/b_impl2/b_impl2.cmi" {"b_impl2/b_impl2.cmi"}
    "_build/install/default/lib/project/b_impl2/b_impl2.cmt" {"b_impl2/b_impl2.cmt"}
    "_build/install/default/lib/project/b_impl2/b_impl2.cmx" {"b_impl2/b_impl2.cmx"}
    "_build/install/default/lib/project/b_impl2/b_impl2.cmxa" {"b_impl2/b_impl2.cmxa"}
    "_build/install/default/lib/project/b_impl2/b_impl2.ml" {"b_impl2/b_impl2.ml"}
    "_build/install/default/lib/project/b_impl2/b_impl2__b_impl2__.cmi" {"b_impl2/b_impl2__b_impl2__.cmi"}
    "_build/install/default/lib/project/b_impl2/b_impl2__b_impl2__.cmt" {"b_impl2/b_impl2__b_impl2__.cmt"}
    "_build/install/default/lib/project/b_impl2/b_impl2__b_impl2__.cmx" {"b_impl2/b_impl2__b_impl2__.cmx"}
    "_build/install/default/lib/project/b_impl2/b_impl2__b_impl2__.ml" {"b_impl2/b_impl2__b_impl2__.ml"}
    "_build/install/default/lib/project/dune-package"
    "_build/install/default/lib/project/lib_ab/lib_ab.a" {"lib_ab/lib_ab.a"}
    "_build/install/default/lib/project/lib_ab/lib_ab.cma" {"lib_ab/lib_ab.cma"}
    "_build/install/default/lib/project/lib_ab/lib_ab.cmi" {"lib_ab/lib_ab.cmi"}
    "_build/install/default/lib/project/lib_ab/lib_ab.cmt" {"lib_ab/lib_ab.cmt"}
    "_build/install/default/lib/project/lib_ab/lib_ab.cmx" {"lib_ab/lib_ab.cmx"}
    "_build/install/default/lib/project/lib_ab/lib_ab.cmxa" {"lib_ab/lib_ab.cmxa"}
    "_build/install/default/lib/project/lib_ab/lib_ab.ml" {"lib_ab/lib_ab.ml"}
    "_build/install/default/lib/project/lib_apply/f.ml" {"lib_apply/f.ml"}
    "_build/install/default/lib/project/lib_apply/lib_apply.a" {"lib_apply/lib_apply.a"}
    "_build/install/default/lib/project/lib_apply/lib_apply.cma" {"lib_apply/lib_apply.cma"}
    "_build/install/default/lib/project/lib_apply/lib_apply.cmi" {"lib_apply/lib_apply.cmi"}
    "_build/install/default/lib/project/lib_apply/lib_apply.cmt" {"lib_apply/lib_apply.cmt"}
    "_build/install/default/lib/project/lib_apply/lib_apply.cmx" {"lib_apply/lib_apply.cmx"}
    "_build/install/default/lib/project/lib_apply/lib_apply.cmxa" {"lib_apply/lib_apply.cmxa"}
    "_build/install/default/lib/project/lib_apply/lib_apply.ml" {"lib_apply/lib_apply.ml"}
    "_build/install/default/lib/project/lib_apply/lib_apply__.cmi" {"lib_apply/lib_apply__.cmi"}
    "_build/install/default/lib/project/lib_apply/lib_apply__.cmt" {"lib_apply/lib_apply__.cmt"}
    "_build/install/default/lib/project/lib_apply/lib_apply__.cmx" {"lib_apply/lib_apply__.cmx"}
    "_build/install/default/lib/project/lib_apply/lib_apply__.ml" {"lib_apply/lib_apply__.ml"}
    "_build/install/default/lib/project/lib_apply/lib_apply__F.cmi" {"lib_apply/lib_apply__F.cmi"}
    "_build/install/default/lib/project/lib_apply/lib_apply__F.cmt" {"lib_apply/lib_apply__F.cmt"}
    "_build/install/default/lib/project/lib_apply/lib_apply__F.cmx" {"lib_apply/lib_apply__F.cmx"}
    "_build/install/default/lib/project/lib_apply/lib_apply__X.cmi" {"lib_apply/lib_apply__X.cmi"}
    "_build/install/default/lib/project/lib_apply/lib_apply__X.cmt" {"lib_apply/lib_apply__X.cmt"}
    "_build/install/default/lib/project/lib_apply/lib_apply__X.cmx" {"lib_apply/lib_apply__X.cmx"}
    "_build/install/default/lib/project/lib_apply/lib_apply__Y.cmi" {"lib_apply/lib_apply__Y.cmi"}
    "_build/install/default/lib/project/lib_apply/lib_apply__Y.cmt" {"lib_apply/lib_apply__Y.cmt"}
    "_build/install/default/lib/project/lib_apply/lib_apply__Y.cmx" {"lib_apply/lib_apply__Y.cmx"}
    "_build/install/default/lib/project/lib_apply/lib_apply__Z.cmi" {"lib_apply/lib_apply__Z.cmi"}
    "_build/install/default/lib/project/lib_apply/lib_apply__Z.cmt" {"lib_apply/lib_apply__Z.cmt"}
    "_build/install/default/lib/project/lib_apply/lib_apply__Z.cmx" {"lib_apply/lib_apply__Z.cmx"}
    "_build/install/default/lib/project/lib_apply/x.ml" {"lib_apply/x.ml"}
    "_build/install/default/lib/project/lib_apply/y.ml" {"lib_apply/y.ml"}
    "_build/install/default/lib/project/lib_apply/z.ml" {"lib_apply/z.ml"}
  ]
  libexec: [
    "_build/install/default/lib/project/a_impl/a_impl.cmxs" {"a_impl/a_impl.cmxs"}
    "_build/install/default/lib/project/a_of_b/a_of_b.cmxs" {"a_of_b/a_of_b.cmxs"}
    "_build/install/default/lib/project/b_impl/b_impl.cmxs" {"b_impl/b_impl.cmxs"}
    "_build/install/default/lib/project/b_impl2/b_impl2.cmxs" {"b_impl2/b_impl2.cmxs"}
    "_build/install/default/lib/project/lib_ab/lib_ab.cmxs" {"lib_ab/lib_ab.cmxs"}
    "_build/install/default/lib/project/lib_apply/lib_apply.cmxs" {"lib_apply/lib_apply.cmxs"}
  ]
  bin: [
    "_build/install/default/bin/project.bin"
  ]
