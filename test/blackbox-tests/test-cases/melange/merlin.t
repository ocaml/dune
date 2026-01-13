 Temporary special merlin support for melange only libs

  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"
  $ melc_compiler="$(which melc)"
  $ export BUILD_PATH_PREFIX_MAP="$(melc_stdlib_prefix)":$BUILD_PATH_PREFIX_MAP
  $ export BUILD_PATH_PREFIX_MAP="/MELC_COMPILER=$melc_compiler:$BUILD_PATH_PREFIX_MAP"
  $ export BUILD_PATH_PREFIX_MAP="/MELC_STDLIB=$(ocamlfind query melange):$BUILD_PATH_PREFIX_MAP"

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ lib=foo
  $ cat >dune <<EOF
  > (library
  >  (name $lib)
  >  (private_modules bar)
  >  (modes melange))
  > EOF

  $ touch bar.ml $lib.ml
  $ dune build @check
  $ dune ocaml merlin dump-config --format=json "$PWD" | jq '
  > include "dune";
  > [
  >   .[]
  >   | merlinConfigSummary(["FLG", "UNIT_NAME"])
  > ]
  > | .[]'
  {
    "module_name": "Foo__",
    "source_path": "_build/default/.melange_src/foo__",
    "config": [
      [
        "FLG",
        [
          "-w",
          "@1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40",
          "-strict-sequence",
          "-strict-formats",
          "-short-paths",
          "-keep-locs",
          "-g"
        ]
      ],
      [
        "UNIT_NAME",
        "foo__"
      ]
    ]
  }
  {
    "module_name": "Foo__",
    "source_path": "_build/default/.melange_src/foo__.ml-gen",
    "config": [
      [
        "FLG",
        [
          "-w",
          "@1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40",
          "-strict-sequence",
          "-strict-formats",
          "-short-paths",
          "-keep-locs",
          "-g"
        ]
      ],
      [
        "UNIT_NAME",
        "foo__"
      ]
    ]
  }
  {
    "module_name": "Bar",
    "source_path": "_build/default/bar",
    "config": [
      [
        "FLG",
        [
          "-w",
          "@1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40",
          "-strict-sequence",
          "-strict-formats",
          "-short-paths",
          "-keep-locs",
          "-g"
        ]
      ],
      [
        "FLG",
        [
          "-open",
          "Foo__"
        ]
      ],
      [
        "UNIT_NAME",
        "foo__Bar"
      ]
    ]
  }
  {
    "module_name": "Bar",
    "source_path": "_build/default/bar.ml",
    "config": [
      [
        "FLG",
        [
          "-w",
          "@1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40",
          "-strict-sequence",
          "-strict-formats",
          "-short-paths",
          "-keep-locs",
          "-g"
        ]
      ],
      [
        "FLG",
        [
          "-open",
          "Foo__"
        ]
      ],
      [
        "UNIT_NAME",
        "foo__Bar"
      ]
    ]
  }
  {
    "module_name": "Foo",
    "source_path": "_build/default/foo",
    "config": [
      [
        "FLG",
        [
          "-w",
          "@1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40",
          "-strict-sequence",
          "-strict-formats",
          "-short-paths",
          "-keep-locs",
          "-g"
        ]
      ],
      [
        "FLG",
        [
          "-open",
          "Foo__"
        ]
      ],
      [
        "UNIT_NAME",
        "foo"
      ]
    ]
  }
  {
    "module_name": "Foo",
    "source_path": "_build/default/foo.ml",
    "config": [
      [
        "FLG",
        [
          "-w",
          "@1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40",
          "-strict-sequence",
          "-strict-formats",
          "-short-paths",
          "-keep-locs",
          "-g"
        ]
      ],
      [
        "FLG",
        [
          "-open",
          "Foo__"
        ]
      ],
      [
        "UNIT_NAME",
        "foo"
      ]
    ]
  }

Paths to Melange stdlib appear in B and S entries without melange.emit stanza

  $ dune ocaml dump-dot-merlin $PWD | grep -e "^B " -e "^S "
  B /MELC_STDLIB/__private__/melange_mini_stdlib/melange/.public_cmi_melange
  B /MELC_STDLIB/melange
  B /MELC_STDLIB/melange
  B $TESTCASE_ROOT/_build/default/.foo.objs/melange
  S /MELC_STDLIB
  S /MELC_STDLIB/__private__/melange_mini_stdlib
  S /MELC_STDLIB
  S $TESTCASE_ROOT

  $ target=output
  $ cat >dune <<EOF
  > (melange.emit
  >  (target "$target")
  >  (compile_flags :standard --mel-noassertfalse)
  >  (emit_stdlib false)
  >  (modules main))
  > EOF

  $ touch main.ml
  $ dune build @check
  $ dune ocaml merlin dump-config --format=json $PWD | jq '
  > include "dune";
  > [
  >   .[]
  >   | merlinConfigSummary(["UNIT_NAME"])
  > ]
  > | .[]'
  {
    "module_name": "Main",
    "source_path": "_build/default/main",
    "config": [
      [
        "UNIT_NAME",
        "melange__Main"
      ]
    ]
  }
  {
    "module_name": "Main",
    "source_path": "_build/default/main.ml",
    "config": [
      [
        "UNIT_NAME",
        "melange__Main"
      ]
    ]
  }

Dump-dot-merlin includes the melange flags

  $ dune ocaml dump-dot-merlin $PWD
  EXCLUDE_QUERY_DIR
  STDLIB /MELC_STDLIB/melange
  SOURCE_ROOT $TESTCASE_ROOT
  B /MELC_STDLIB/__private__/melange_mini_stdlib/melange/.public_cmi_melange
  B /MELC_STDLIB/melange
  B /MELC_STDLIB/melange
  B $TESTCASE_ROOT/_build/default/.output.mobjs/melange
  S /MELC_STDLIB
  S /MELC_STDLIB/__private__/melange_mini_stdlib
  S /MELC_STDLIB
  S $TESTCASE_ROOT
  INDEX $TESTCASE_ROOT/_build/default/.output.mobjs/cctx.ocaml-index
  # FLG -w @1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40 -strict-sequence -strict-formats -short-paths -keep-locs -g --mel-noassertfalse
  
Check for flag directives ordering when another preprocessor is defined

  $ cat >fooppx.ml <<EOF
  > open Ppxlib
  > 
  > let rules =
  >   let extension =
  >     Extension.declare "test" Expression Ast_pattern.__ (fun ~loc ~path:_ _ ->
  >       Ast_builder.Default.eint ~loc 42)
  >   in
  >   [ Context_free.Rule.extension extension ]
  > 
  > let () = Ppxlib.Driver.register_transformation "rules" ~rules
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name $lib)
  >  (private_modules bar)
  >  (modules bar)
  >  (preprocess (pps fooppx))
  >  (modes melange))
  > (library
  >  (name fooppx)
  >  (modules fooppx)
  >  (libraries ppxlib)
  >  (kind ppx_rewriter))
  > EOF

  $ dune build @check

User ppx flags should appear in merlin config

  $ dune ocaml merlin dump-config --format=json $PWD | jq '
  > include "dune";
  > [
  >   .[]
  >   | merlinConfigSummary(["STDLIB", "FLG", "UNIT_NAME"])
  > ]
  > | .[]' | censor_ppx
  {
    "module_name": "Foo",
    "source_path": "_build/default/.melange_src/foo",
    "config": [
      [
        "STDLIB",
        "/MELC_STDLIB/melange"
      ],
      [
        "FLG",
        [
          "-w",
          "@1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40",
          "-strict-sequence",
          "-strict-formats",
          "-short-paths",
          "-keep-locs",
          "-g"
        ]
      ],
      [
        "FLG",
        [
          "-ppx",
          "$TESTCASE_ROOT/_build/default/.ppx/$DIGEST/ppx.exe --as-ppx --cookie 'library-name=\"foo\"'"
        ]
      ],
      [
        "UNIT_NAME",
        "foo"
      ]
    ]
  }
  {
    "module_name": "Foo",
    "source_path": "_build/default/.melange_src/foo.ml-gen",
    "config": [
      [
        "STDLIB",
        "/MELC_STDLIB/melange"
      ],
      [
        "FLG",
        [
          "-w",
          "@1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40",
          "-strict-sequence",
          "-strict-formats",
          "-short-paths",
          "-keep-locs",
          "-g"
        ]
      ],
      [
        "FLG",
        [
          "-ppx",
          "$TESTCASE_ROOT/_build/default/.ppx/$DIGEST/ppx.exe --as-ppx --cookie 'library-name=\"foo\"'"
        ]
      ],
      [
        "UNIT_NAME",
        "foo"
      ]
    ]
  }
  {
    "module_name": "Bar",
    "source_path": "_build/default/bar",
    "config": [
      [
        "STDLIB",
        "/MELC_STDLIB/melange"
      ],
      [
        "FLG",
        [
          "-w",
          "@1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40",
          "-strict-sequence",
          "-strict-formats",
          "-short-paths",
          "-keep-locs",
          "-g"
        ]
      ],
      [
        "FLG",
        [
          "-ppx",
          "$TESTCASE_ROOT/_build/default/.ppx/$DIGEST/ppx.exe --as-ppx --cookie 'library-name=\"foo\"'"
        ]
      ],
      [
        "FLG",
        [
          "-open",
          "Foo"
        ]
      ],
      [
        "UNIT_NAME",
        "foo__Bar"
      ]
    ]
  }
  {
    "module_name": "Bar",
    "source_path": "_build/default/bar.ml",
    "config": [
      [
        "STDLIB",
        "/MELC_STDLIB/melange"
      ],
      [
        "FLG",
        [
          "-w",
          "@1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40",
          "-strict-sequence",
          "-strict-formats",
          "-short-paths",
          "-keep-locs",
          "-g"
        ]
      ],
      [
        "FLG",
        [
          "-ppx",
          "$TESTCASE_ROOT/_build/default/.ppx/$DIGEST/ppx.exe --as-ppx --cookie 'library-name=\"foo\"'"
        ]
      ],
      [
        "FLG",
        [
          "-open",
          "Foo"
        ]
      ],
      [
        "UNIT_NAME",
        "foo__Bar"
      ]
    ]
  }
  {
    "module_name": "Fooppx",
    "source_path": "_build/default/fooppx",
    "config": [
      [
        "STDLIB",
        "/OCAMLC_WHERE"
      ],
      [
        "FLG",
        [
          "-w",
          "@1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40",
          "-strict-sequence",
          "-strict-formats",
          "-short-paths",
          "-keep-locs",
          "-g"
        ]
      ],
      [
        "UNIT_NAME",
        "fooppx"
      ]
    ]
  }
  {
    "module_name": "Fooppx",
    "source_path": "_build/default/fooppx.ml",
    "config": [
      [
        "STDLIB",
        "/OCAMLC_WHERE"
      ],
      [
        "FLG",
        [
          "-w",
          "@1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40",
          "-strict-sequence",
          "-strict-formats",
          "-short-paths",
          "-keep-locs",
          "-g"
        ]
      ],
      [
        "UNIT_NAME",
        "fooppx"
      ]
    ]
  }
