Testing composition of theories across a dune workspace with a boot library. A
boot library must have the following:

- Under the module prefix "Coq"
- Have a file Init/Prelude.v

When composing with a (boot) library, every library must have -boot passed to
coqdep and coqc.

  $ dune build A
  Module
  Prelude
  := Struct Inductive BootType : Set :=  boot : BootType | type : BootType. End
  Hello
       : Set

We can even have a secondary private boot library in another scope and only the
private boot library will be loaded implicitly.

  $ cat > B/Coq/dune << EOF
  > (include_subdirs qualified)
  > (rocq.theory
  >  (name Coq)
  >  (boot))
  > EOF

  $ dune build B
  private_boot
       : PrivateBootType

However if this boot library is public Dune will complain

  $ cat > B/Coq/dune << EOF
  > (include_subdirs qualified)
  > (rocq.theory
  >  (name Coq)
  >  (package Foo)
  >  (boot))
  > EOF

Due to lazy loading of installed theories, the error messages for having more
than one boot theory is a bit delayed and comes with the multiple rules error
message together. This is some extra noise for the user, but we are not sure how
to fix this currently.

  $ dune build B
  Error: Cannot have more than one boot theory in scope:
  - Coq at B/Coq/dune:2
  - Coq at Coq/dune:1
  Error: Multiple rules generated for
  _build/install/default/lib/coq/theories/rocq-package:
  - Coq/dune:1
  - B/Coq/dune:2
  -> required by _build/default/B/Foo.install
  -> required by alias B/all
  -> required by alias B/default
  [1]
