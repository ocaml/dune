Test the (dialect ...) stanza inside the dune-project file.

  $ dune build --root good --display short @install
  Entering directory 'good'
      ocamldep .fmt.eobjs/fmt.ml.d
        ocamlc .fmt.eobjs/byte/fmt.{cmi,cmo,cmt}
      ocamlopt .fmt.eobjs/native/fmt.{cmx,o}
      ocamlopt fmt.exe
      ocamldep .main.eobjs/main.mf.d
      ocamldep .main.eobjs/main.mfi.d
        ocamlc .main.eobjs/byte/main.{cmi,cmti}
      ocamlopt .main.eobjs/native/main.{cmx,o}
      ocamlopt main.exe

  $ dune build --root good --display short @fmt
  Entering directory 'good'
           fmt .formatted/main.mf
           fmt .formatted/main.mfi

  $ dune build --root bad1
  Entering directory 'bad1'
  File "dune-project", line 9, characters 1-74:
   9 |  (name d)
  10 |  (implementation (extension foo2))
  11 |  (interface (extension bar2)))
  Error: dialect "d" is already defined
  [1]

  $ dune build --root bad2
  Entering directory 'bad2'
  File "dune-project", line 9, characters 1-74:
   9 |  (name d2)
  10 |  (implementation (extension foo))
  11 |  (interface (extension bar2)))
  Error: extension "foo" is already registered by dialect "d"
  [1]

  $ dune build --root bad3
  Entering directory 'bad3'
  File "dune-project", line 5, characters 28-32:
  5 |  (implementation (extension .foo))
                                  ^^^^
  Error: extension must not contain '.'
  [1]
