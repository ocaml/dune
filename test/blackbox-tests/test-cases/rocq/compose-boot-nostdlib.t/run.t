Testing composition of theories across a Dune workspace with a boot library and
importing ``corelib`` enabled or disabled.

Composing library A depending on Coq but having `(no_corelib)`:

  $ dune build A
  Module
  Prelude
  := Struct Inductive BootType : Set :=  boot : BootType | type : BootType. End
  Hello
       : Set

Composing library B depending on Coq with the default Corelib binding:

  $ dune build B
  Module
  Prelude
  := Struct Inductive BootType : Set :=  boot : BootType | type : BootType. End
  Hello
       : Set
