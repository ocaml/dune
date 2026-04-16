  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"

  $ dune exec ./foo.exe
  42

  $ dune ocaml merlin dump-config --format=json $PWD | jq '
  >   include "dune";
  >   merlinEntry("Foo")
  >   | merlinUnitNameSummary'
  {
    "module_name": "Foo",
    "source_path": "_build/default/foo",
    "unit_name": "dune__exe__Foo"
  }
  {
    "module_name": "Foo",
    "source_path": "_build/default/foo.ml",
    "unit_name": "dune__exe__Foo"
  }

  $ dune ocaml merlin dump-config --format=json $PWD/foo | jq '
  >   include "dune";
  >   [
  >     (merlinEntry("Bar")
  >      | merlinConfigSummary(["UNIT_NAME", "FLG"])),
  >     (merlinEntry("Foo")
  >      | merlinConfigSummary(["UNIT_NAME", "FLG"]))
  >   ]
  >   | sort_by(.module_name, .source_path)
  >   | .[]'
  {
    "module_name": "Bar",
    "source_path": "_build/default/foo/bar",
    "config": [
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
    "source_path": "_build/default/foo/bar.ml",
    "config": [
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
    "module_name": "Foo",
    "source_path": "_build/default/foo/foo",
    "config": [
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
        "foo"
      ]
    ]
  }
  {
    "module_name": "Foo",
    "source_path": "_build/default/foo/foo.ml-gen",
    "config": [
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
        "foo"
      ]
    ]
  }

FIXME : module Foo is not unbound
This test is disabled because it depends on root detection and is not reproducible.
$ ocamlmerlin single errors -filename foo.ml < foo.ml | jq ".value.message"
"Unbound module Foo"
