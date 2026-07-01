 Temporary special merlin support for melange only libs

  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"
  $ melc_compiler="$(which melc)"
  $ export BUILD_PATH_PREFIX_MAP="$(melc_stdlib_prefix)":$BUILD_PATH_PREFIX_MAP
  $ export BUILD_PATH_PREFIX_MAP="/MELC_COMPILER=$melc_compiler:$BUILD_PATH_PREFIX_MAP"
  $ export BUILD_PATH_PREFIX_MAP="/MELC_STDLIB=$(ocamlfind query melange):$BUILD_PATH_PREFIX_MAP"

  $ make_melange_project 3.8 0.1

  $ lib=foo
  $ cat >dune <<EOF
  > (library
  >  (name $lib)
  >  (private_modules bar)
  >  (modes melange))
  > EOF

  $ touch bar.ml $lib.ml
  $ dune build @check
  $ dune ocaml merlin dump-config --format=json "$PWD" | jq_dune '
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
  $ dune ocaml merlin dump-config --format=json $PWD | jq_dune '
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
  INDEX $TESTCASE_ROOT/_build/default/.output.mobjs/melange/cctx.ocaml-index
  SUFFIX .melange.ml .melange.mli
  SUFFIX .melange.re .melange.rei
  SUFFIX .melange.res .melange.resi
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

  $ dune ocaml merlin dump-config --format=json $PWD | jq_dune '
  > [
  >   .[]
  >   | merlinConfigSummary(["STDLIB", "FLG", "UNIT_NAME"])
  > ]
  > | .[]' | censor
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
          "$PWD/_build/default/.ppx/$DIGEST/ppx.exe --as-ppx --cookie 'library-name=\"foo\"'"
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
          "$PWD/_build/default/.ppx/$DIGEST/ppx.exe --as-ppx --cookie 'library-name=\"foo\"'"
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
          "$PWD/_build/default/.ppx/$DIGEST/ppx.exe --as-ppx --cookie 'library-name=\"foo\"'"
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
          "$PWD/_build/default/.ppx/$DIGEST/ppx.exe --as-ppx --cookie 'library-name=\"foo\"'"
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

Mixed OCaml/Melange libraries generate separate Merlin configuration files.

  $ mkdir mixed
  $ cat > mixed/dune-project <<EOF
  > (lang dune 3.24)
  > (using melange 0.1)
  > EOF
  $ cat > mixed/pp_ocaml.sh <<'EOF'
  > #!/bin/sh
  > cat "$1"
  > EOF
  $ cat > mixed/pp_melange.sh <<'EOF'
  > #!/bin/sh
  > cat "$1"
  > EOF
  $ chmod +x mixed/pp_ocaml.sh mixed/pp_melange.sh
  $ cat > mixed/dune <<EOF
  > (library
  >  (name mixed)
  >  (modules foo)
  >  (modes :standard melange)
  >  (preprocess
  >   (action
  >    (run sh %{dep:pp_ocaml.sh} %{input-file})))
  >  (melange.preprocess
  >   (action
  >    (run sh %{dep:pp_melange.sh} %{input-file}))))
  > EOF
  $ cat > mixed/foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ dune build --root mixed @check
  $ find mixed/_build/default/.merlin-conf -type f | sort
  mixed/_build/default/.merlin-conf/lib-mixed

The old `File` query still returns the default OCaml Merlin configuration.

  $ query_ocaml_merlin_pp "$PWD/mixed/foo.ml" --root mixed | grep -E 'STDLIB|\(B .*\.mixed\.objs'
   (STDLIB /OCAMLC_WHERE)
   (B $TESTCASE_ROOT/mixed/_build/default/.mixed.objs/byte)
  $ query_ocaml_merlin_pp "$PWD/mixed/foo.ml" --root mixed | grep -E 'MELC_STDLIB|\.objs/melange|pp_melange'
  [1]

Both generated configurations remain available to debug tooling.

  $ dune ocaml merlin dump-config --root mixed --format=json "$PWD/mixed" | jq_dune '
  > def config($name): .config[] | select(.[0] == $name) | .[1];
  > def local_path: sub("^.*_build/default/"; "_build/default/");
  > [
  >   merlinEntry("Foo")
  >   | (first(config("B") | select(contains(".mixed.objs"))) | local_path) as $obj_dir
  >   | first(config("FLG") | select(.[0] == "-pp") | .[1]) as $pp
  >   | {
  >       mode: (if $obj_dir | contains("/melange") then "melange" else "ocaml" end),
  >       obj_dir: $obj_dir,
  >       preprocess: (if $pp | contains("pp_melange") then "melange" else "ocaml" end)
  >     }
  > ]
  > | unique
  > | sort_by(.mode == "melange")' | censor
  [
    {
      "mode": "ocaml",
      "obj_dir": "_build/default/.mixed.objs/byte",
      "preprocess": "ocaml"
    }
  ]
