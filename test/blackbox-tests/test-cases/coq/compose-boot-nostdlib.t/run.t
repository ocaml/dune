Testing composition of theories across a Dune workspace with a boot library and
importing ``stdlib`` enabled or disabled.

Composing library A depending on Coq but having `(stdlib no)`:

  $ dune build A
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  Module
  Prelude
  := Struct Inductive BootType : Set :=  boot : BootType | type : BootType. End
  
  Hello
       : Set

Composing library B depending on Coq but having `(stdlib yes)`:

  $ dune build B
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  Module
  Prelude
  := Struct Inductive BootType : Set :=  boot : BootType | type : BootType. End
  
  Hello
       : Set
