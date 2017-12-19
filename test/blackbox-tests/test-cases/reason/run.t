  $ $JBUILDER runtest -j1 --root .
         refmt bar.re.ml
      ocamldep pp/reasononlypp.depends.ocamldep-output
      ocamldep ppx/reasonppx.depends.ocamldep-output
         refmt hello.re.ml
         refmt pped.re.ml
        ocamlc rlib.{cmi,cmo,cmt}
         refmt foo.re.mli
         refmt hello.re.mli
         refmt pped.re.mli
        ocamlc pp/reasononlypp.{cmi,cmo,cmt}
        ocamlc ppx/reasonppx.{cmi,cmo,cmt}
      ocamlopt rlib.{cmx,o}
      ocamlopt pp/reasononlypp.{cmx,o}
      ocamlopt ppx/reasonppx.{cmx,o}
      ocamlopt pp/reasononlypp.exe
      ocamlopt ppx/reasonppx.{a,cmxa}
  reasononlypp cppome.pp.re
  reasononlypp cppome.pp.rei
      ocamlopt .ppx/reasonppx/ppx.exe
         refmt cppome.pp.re.ml
         refmt cppome.pp.re.mli
           ppx foo.pp.ml
           ppx hello.re.pp.ml (exit 2)
  (cd _build/default && ./.ppx/reasonppx/ppx.exe -lint false --dump-ast --cookie 'library-name="rlib"' -o hello.re.pp.ml --impl hello.re.ml)
  ./.ppx/reasonppx/ppx.exe: unknown option '-lint'.
  ppx.exe [options] [<files>]
    --as-ppx           Act as a -ppx rewriter
    --as-pp            Shorthand for: --dump-ast --embed-errors
    --dump-ast         Output a binary AST instead of source code
    -o FILE            Output to this file instead of the standard output
    --intf FILE        Treat FILE as a .mli file
    --impl FILE        Treat FILE as a .ml file
    --embed-errors     Embed error reported by rewriters into the AST
    --cookie NAME=EXPR Set the cookie NAME to EXPR
    -help              Display this list of options
    --help             Display this list of options
  [1]
