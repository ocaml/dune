Testing composition of theories across a Dune workspace with a boot library and
importing ``stdlib`` enabled or disabled.

Composing library A depending on Coq but having `(stdlib no)`:

  $ dune build A
  Module
  Prelude
  := Struct Inductive BootType : Set :=  boot : BootType | type : BootType. End
  
  Hello
       : Set

Composing library B depending on Coq but having `(stdlib yes)`:

  $ dune build B
  Module
  Prelude
  := Struct Inductive BootType : Set :=  boot : BootType | type : BootType. End
  
  Hello
       : Set
