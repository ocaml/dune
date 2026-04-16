  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"
  $ ocamlfind_libs="$(ocamlfind printconf path | while read line; do printf lib=${line}:; done)"
  $ export BUILD_PATH_PREFIX_MAP="$ocamlfind_libs:$BUILD_PATH_PREFIX_MAP"

We're going create a fake findlib library for use:

  $ mkdir -p _findlib/publicfoo
  $ cat >_findlib/publicfoo/META <<EOF
  > EOF
  $ export OCAMLPATH="$PWD/_findlib:$OCAMLPATH"

CRAM sanitization
  $ dune build ./exe/.merlin-conf/exe-x --profile release
  $ dune ocaml merlin dump-config --format=json $PWD/exe | jq '
  >   include "dune";
  >   [
  >     .[]
  >     | select(.module_name == "X")
  >     | merlinConfigSummary(["B", "S", "FLG", "UNIT_NAME"])
  >   ]
  >   | sort_by(.source_path)
  >   | .[]' | censor_ppx
  {
    "module_name": "X",
    "source_path": "_build/default/exe/x",
    "config": [
      [
        "B",
        "$TESTCASE_ROOT/_findlib/publicfoo"
      ],
      [
        "B",
        "$TESTCASE_ROOT/_build/default/exe/.x.eobjs/byte"
      ],
      [
        "B",
        "$TESTCASE_ROOT/_build/default/lib/.foo.objs/public_cmi"
      ],
      [
        "S",
        "$TESTCASE_ROOT/_findlib/publicfoo"
      ],
      [
        "S",
        "$TESTCASE_ROOT/exe"
      ],
      [
        "S",
        "$TESTCASE_ROOT/lib"
      ],
      [
        "FLG",
        [
          "-w",
          "-40",
          "-g"
        ]
      ],
      [
        "FLG",
        [
          "-pp",
          "$TESTCASE_ROOT/_build/default/pp/pp.exe"
        ]
      ],
      [
        "UNIT_NAME",
        "x"
      ]
    ]
  }
  {
    "module_name": "X",
    "source_path": "_build/default/exe/x.ml",
    "config": [
      [
        "B",
        "$TESTCASE_ROOT/_findlib/publicfoo"
      ],
      [
        "B",
        "$TESTCASE_ROOT/_build/default/exe/.x.eobjs/byte"
      ],
      [
        "B",
        "$TESTCASE_ROOT/_build/default/lib/.foo.objs/public_cmi"
      ],
      [
        "S",
        "$TESTCASE_ROOT/_findlib/publicfoo"
      ],
      [
        "S",
        "$TESTCASE_ROOT/exe"
      ],
      [
        "S",
        "$TESTCASE_ROOT/lib"
      ],
      [
        "FLG",
        [
          "-w",
          "-40",
          "-g"
        ]
      ],
      [
        "FLG",
        [
          "-pp",
          "$TESTCASE_ROOT/_build/default/pp/pp.exe"
        ]
      ],
      [
        "UNIT_NAME",
        "x"
      ]
    ]
  }

  $ dune build ./lib/.merlin-conf/lib-foo ./lib/.merlin-conf/lib-bar --profile release
  $ dune ocaml merlin dump-config --format=json $PWD/lib | jq '
  >   include "dune";
  >   [
  >     .[]
  >     | select(.module_name == "Bar" or .module_name == "File" or .module_name == "Foo" or .module_name == "Privmod")
  >     | merlinConfigSummary(["B", "S", "FLG", "UNIT_NAME"])
  >   ]
  >   | sort_by(.module_name, .source_path)
  >   | .[]' | censor_ppx
  {
    "module_name": "Bar",
    "source_path": "_build/default/lib/bar",
    "config": [
      [
        "B",
        "$TESTCASE_ROOT/_build/default/lib/.bar.objs/byte"
      ],
      [
        "S",
        "$TESTCASE_ROOT/lib"
      ],
      [
        "S",
        "$TESTCASE_ROOT/lib/subdir"
      ],
      [
        "FLG",
        [
          "-w",
          "-40",
          "-g"
        ]
      ],
      [
        "FLG",
        [
          "-ppx",
          "$TESTCASE_ROOT/_build/default/.ppx/$DIGEST/ppx.exe --as-ppx --cookie 'library-name=\"bar\"'"
        ]
      ],
      [
        "UNIT_NAME",
        "bar"
      ]
    ]
  }
  {
    "module_name": "Bar",
    "source_path": "_build/default/lib/bar.ml-gen",
    "config": [
      [
        "B",
        "$TESTCASE_ROOT/_build/default/lib/.bar.objs/byte"
      ],
      [
        "S",
        "$TESTCASE_ROOT/lib"
      ],
      [
        "S",
        "$TESTCASE_ROOT/lib/subdir"
      ],
      [
        "FLG",
        [
          "-w",
          "-40",
          "-g"
        ]
      ],
      [
        "FLG",
        [
          "-ppx",
          "$TESTCASE_ROOT/_build/default/.ppx/$DIGEST/ppx.exe --as-ppx --cookie 'library-name=\"bar\"'"
        ]
      ],
      [
        "UNIT_NAME",
        "bar"
      ]
    ]
  }
  {
    "module_name": "File",
    "source_path": "_build/default/lib/subdir/file",
    "config": [
      [
        "B",
        "$TESTCASE_ROOT/_build/default/lib/.bar.objs/byte"
      ],
      [
        "S",
        "$TESTCASE_ROOT/lib"
      ],
      [
        "S",
        "$TESTCASE_ROOT/lib/subdir"
      ],
      [
        "FLG",
        [
          "-w",
          "-40",
          "-g"
        ]
      ],
      [
        "FLG",
        [
          "-ppx",
          "$TESTCASE_ROOT/_build/default/.ppx/$DIGEST/ppx.exe --as-ppx --cookie 'library-name=\"bar\"'"
        ]
      ],
      [
        "FLG",
        [
          "-open",
          "Bar"
        ]
      ],
      [
        "UNIT_NAME",
        "bar__File"
      ]
    ]
  }
  {
    "module_name": "File",
    "source_path": "_build/default/lib/subdir/file.ml",
    "config": [
      [
        "B",
        "$TESTCASE_ROOT/_build/default/lib/.bar.objs/byte"
      ],
      [
        "S",
        "$TESTCASE_ROOT/lib"
      ],
      [
        "S",
        "$TESTCASE_ROOT/lib/subdir"
      ],
      [
        "FLG",
        [
          "-w",
          "-40",
          "-g"
        ]
      ],
      [
        "FLG",
        [
          "-ppx",
          "$TESTCASE_ROOT/_build/default/.ppx/$DIGEST/ppx.exe --as-ppx --cookie 'library-name=\"bar\"'"
        ]
      ],
      [
        "FLG",
        [
          "-open",
          "Bar"
        ]
      ],
      [
        "UNIT_NAME",
        "bar__File"
      ]
    ]
  }
  {
    "module_name": "Foo",
    "source_path": "_build/default/lib/foo",
    "config": [
      [
        "B",
        "$TESTCASE_ROOT/_findlib/publicfoo"
      ],
      [
        "B",
        "$TESTCASE_ROOT/_build/default/lib/.foo.objs/byte"
      ],
      [
        "S",
        "$TESTCASE_ROOT/_findlib/publicfoo"
      ],
      [
        "S",
        "$TESTCASE_ROOT/lib"
      ],
      [
        "S",
        "$TESTCASE_ROOT/lib/subdir"
      ],
      [
        "FLG",
        [
          "-w",
          "-40",
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
    "source_path": "_build/default/lib/foo.ml-gen",
    "config": [
      [
        "B",
        "$TESTCASE_ROOT/_findlib/publicfoo"
      ],
      [
        "B",
        "$TESTCASE_ROOT/_build/default/lib/.foo.objs/byte"
      ],
      [
        "S",
        "$TESTCASE_ROOT/_findlib/publicfoo"
      ],
      [
        "S",
        "$TESTCASE_ROOT/lib"
      ],
      [
        "S",
        "$TESTCASE_ROOT/lib/subdir"
      ],
      [
        "FLG",
        [
          "-w",
          "-40",
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
    "module_name": "Privmod",
    "source_path": "_build/default/lib/privmod",
    "config": [
      [
        "B",
        "$TESTCASE_ROOT/_findlib/publicfoo"
      ],
      [
        "B",
        "$TESTCASE_ROOT/_build/default/lib/.foo.objs/byte"
      ],
      [
        "S",
        "$TESTCASE_ROOT/_findlib/publicfoo"
      ],
      [
        "S",
        "$TESTCASE_ROOT/lib"
      ],
      [
        "S",
        "$TESTCASE_ROOT/lib/subdir"
      ],
      [
        "FLG",
        [
          "-w",
          "-40",
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
        "foo__Privmod"
      ]
    ]
  }
  {
    "module_name": "Privmod",
    "source_path": "_build/default/lib/privmod.ml",
    "config": [
      [
        "B",
        "$TESTCASE_ROOT/_findlib/publicfoo"
      ],
      [
        "B",
        "$TESTCASE_ROOT/_build/default/lib/.foo.objs/byte"
      ],
      [
        "S",
        "$TESTCASE_ROOT/_findlib/publicfoo"
      ],
      [
        "S",
        "$TESTCASE_ROOT/lib"
      ],
      [
        "S",
        "$TESTCASE_ROOT/lib/subdir"
      ],
      [
        "FLG",
        [
          "-w",
          "-40",
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
        "foo__Privmod"
      ]
    ]
  }

Make sure a ppx directive is generated (if not, the [grep ppx] step fails)
  $ dune ocaml merlin dump-config --format=json $PWD/lib | jq '
  >   include "dune";
  >   [
  >     .[]
  >     | select(.module_name == "Bar" or .module_name == "Foo" or .module_name == "Privmod")
  >     | {
  >         module_name,
  >         source_path: merlinBuildPath,
  >         ppx_flags: [
  >           .config[]
  >           | select(.[0] == "FLG" and (.[1] | type == "array" and index("-ppx")))
  >         ]
  >       }
  >     | select(.ppx_flags != [])
  >   ]
  >   | sort_by(.module_name, .source_path)
  >   | .[]' | censor_ppx
  {
    "module_name": "Bar",
    "source_path": "_build/default/lib/bar",
    "ppx_flags": [
      [
        "FLG",
        [
          "-ppx",
          "$TESTCASE_ROOT/_build/default/.ppx/$DIGEST/ppx.exe --as-ppx --cookie 'library-name=\"bar\"'"
        ]
      ]
    ]
  }
  {
    "module_name": "Bar",
    "source_path": "_build/default/lib/bar.ml-gen",
    "ppx_flags": [
      [
        "FLG",
        [
          "-ppx",
          "$TESTCASE_ROOT/_build/default/.ppx/$DIGEST/ppx.exe --as-ppx --cookie 'library-name=\"bar\"'"
        ]
      ]
    ]
  }
  {
    "module_name": "Foo",
    "source_path": "_build/default/lib/foo",
    "ppx_flags": [
      [
        "FLG",
        [
          "-ppx",
          "$TESTCASE_ROOT/_build/default/.ppx/$DIGEST/ppx.exe --as-ppx --cookie 'library-name=\"foo\"'"
        ]
      ]
    ]
  }
  {
    "module_name": "Foo",
    "source_path": "_build/default/lib/foo.ml-gen",
    "ppx_flags": [
      [
        "FLG",
        [
          "-ppx",
          "$TESTCASE_ROOT/_build/default/.ppx/$DIGEST/ppx.exe --as-ppx --cookie 'library-name=\"foo\"'"
        ]
      ]
    ]
  }
  {
    "module_name": "Privmod",
    "source_path": "_build/default/lib/privmod",
    "ppx_flags": [
      [
        "FLG",
        [
          "-ppx",
          "$TESTCASE_ROOT/_build/default/.ppx/$DIGEST/ppx.exe --as-ppx --cookie 'library-name=\"foo\"'"
        ]
      ]
    ]
  }
  {
    "module_name": "Privmod",
    "source_path": "_build/default/lib/privmod.ml",
    "ppx_flags": [
      [
        "FLG",
        [
          "-ppx",
          "$TESTCASE_ROOT/_build/default/.ppx/$DIGEST/ppx.exe --as-ppx --cookie 'library-name=\"foo\"'"
        ]
      ]
    ]
  }

Make sure pp flag is correct and variables are expanded

  $ dune build ./pp-with-expand/.merlin-conf/exe-foobar --profile release
  $ dune ocaml merlin dump-config --format=json $PWD/pp-with-expand | jq '
  >   include "dune";
  >   [
  >     .[]
  >     | select(.module_name == "Foobar")
  >     | merlinConfigSummary(["FLG", "UNIT_NAME"])
  >   ]
  >   | sort_by(.source_path)
  >   | .[]'
  {
    "module_name": "Foobar",
    "source_path": "_build/default/pp-with-expand/foobar",
    "config": [
      [
        "FLG",
        [
          "-w",
          "-40",
          "-g"
        ]
      ],
      [
        "FLG",
        [
          "-pp",
          "$TESTCASE_ROOT/_build/default/pp/pp.exe -nothing"
        ]
      ],
      [
        "UNIT_NAME",
        "foobar"
      ]
    ]
  }
  {
    "module_name": "Foobar",
    "source_path": "_build/default/pp-with-expand/foobar.ml",
    "config": [
      [
        "FLG",
        [
          "-w",
          "-40",
          "-g"
        ]
      ],
      [
        "FLG",
        [
          "-pp",
          "$TESTCASE_ROOT/_build/default/pp/pp.exe -nothing"
        ]
      ],
      [
        "UNIT_NAME",
        "foobar"
      ]
    ]
  }

Check hash of executables names if more than one
  $ dune build @exes/check
  $ dune ocaml merlin dump-config --format=json $PWD/exes | jq '
  >   include "dune";
  >   [
  >     .[]
  >     | select(.module_name == "X" or .module_name == "Y")
  >     | merlinConfigSummary(["B", "S", "FLG", "UNIT_NAME"])
  >   ]
  >   | sort_by(.module_name, .source_path)
  >   | .[]'
  {
    "module_name": "X",
    "source_path": "_build/default/exes/x",
    "config": [
      [
        "B",
        "$TESTCASE_ROOT/_build/default/exes/.x.eobjs/byte"
      ],
      [
        "S",
        "$TESTCASE_ROOT/exes"
      ],
      [
        "FLG",
        [
          "-w",
          "@1..3@5..28@30..39@43@46..47@49..57@61..62-40",
          "-strict-sequence",
          "-strict-formats",
          "-short-paths",
          "-keep-locs",
          "-g"
        ]
      ],
      [
        "UNIT_NAME",
        "x"
      ]
    ]
  }
  {
    "module_name": "X",
    "source_path": "_build/default/exes/x.ml",
    "config": [
      [
        "B",
        "$TESTCASE_ROOT/_build/default/exes/.x.eobjs/byte"
      ],
      [
        "S",
        "$TESTCASE_ROOT/exes"
      ],
      [
        "FLG",
        [
          "-w",
          "@1..3@5..28@30..39@43@46..47@49..57@61..62-40",
          "-strict-sequence",
          "-strict-formats",
          "-short-paths",
          "-keep-locs",
          "-g"
        ]
      ],
      [
        "UNIT_NAME",
        "x"
      ]
    ]
  }
  {
    "module_name": "Y",
    "source_path": "_build/default/exes/y",
    "config": [
      [
        "B",
        "$TESTCASE_ROOT/_build/default/exes/.x.eobjs/byte"
      ],
      [
        "S",
        "$TESTCASE_ROOT/exes"
      ],
      [
        "FLG",
        [
          "-w",
          "@1..3@5..28@30..39@43@46..47@49..57@61..62-40",
          "-strict-sequence",
          "-strict-formats",
          "-short-paths",
          "-keep-locs",
          "-g"
        ]
      ],
      [
        "UNIT_NAME",
        "y"
      ]
    ]
  }
  {
    "module_name": "Y",
    "source_path": "_build/default/exes/y.ml",
    "config": [
      [
        "B",
        "$TESTCASE_ROOT/_build/default/exes/.x.eobjs/byte"
      ],
      [
        "S",
        "$TESTCASE_ROOT/exes"
      ],
      [
        "FLG",
        [
          "-w",
          "@1..3@5..28@30..39@43@46..47@49..57@61..62-40",
          "-strict-sequence",
          "-strict-formats",
          "-short-paths",
          "-keep-locs",
          "-g"
        ]
      ],
      [
        "UNIT_NAME",
        "y"
      ]
    ]
  }
