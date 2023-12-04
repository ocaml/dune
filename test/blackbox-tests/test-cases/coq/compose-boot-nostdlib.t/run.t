Testing composition of theories across a Dune workspace with a boot library and
importing ``stdlib`` enabled or disabled.

Composing library A depending on Coq but having `(stdlib no)`:

  $ dune build A
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Module
  Prelude
  := Struct Inductive BootType : Set :=  boot : BootType | type : BootType. End
  
  Hello
       : Set

Composing library B depending on Coq but having `(stdlib yes)`:

  $ dune build B
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Module
  Prelude
  := Struct Inductive BootType : Set :=  boot : BootType | type : BootType. End
  
  Hello
       : Set
