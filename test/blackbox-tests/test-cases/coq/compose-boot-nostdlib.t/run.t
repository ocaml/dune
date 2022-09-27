Testing composition of theories across a Dune workspace with a boot library and
importing ``stdlib`` enabled or disabled.

Composing library A depending on Coq but having `(stdlib no)`:

  $ dune build A
  Logical Path / Physical path:
  A
    $TESTCASE_ROOT/_build/default/A
  Coq
    $TESTCASE_ROOT/_build/default/Coq
  Coq.Init
    $TESTCASE_ROOT/_build/default/Coq/Init
  Module
  Prelude
  := Struct Inductive BootType : Set :=  boot : BootType | type : BootType. End
  
  Hello
       : Set

Composing library B depending on Coq but having `(stdlib yes)`:

  $ dune build B
  Logical Path / Physical path:
  B
    $TESTCASE_ROOT/_build/default/B
  Coq
    $TESTCASE_ROOT/_build/default/Coq
  Coq.Init
    $TESTCASE_ROOT/_build/default/Coq/Init
  Module
  Prelude
  := Struct Inductive BootType : Set :=  boot : BootType | type : BootType. End
  
  Hello
       : Set
