Generates Merlin config for `(include_subdirs qualified)` libraries.

  $ make_dune_project 3.12

  $ cat >dune <<EOF
  > (include_subdirs qualified)
  > (library
  >  (name foo))
  > EOF

  $ touch main.ml
  $ mkdir utils
  $ touch utils/calc.ml

  $ mkdir groupintf
  $ touch groupintf/groupintf.ml
  $ touch groupintf/calc.ml

  $ opam_prefix="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OPAM_PREFIX=$opam_prefix:$BUILD_PATH_PREFIX_MAP"

  $ dune build .merlin-conf/lib-foo
  $ dune ocaml merlin dump-config --format=json . | jq '
  >   include "dune";
  >   [
  >     .[]
  >     | merlinConfigSummary(["FLG", "UNIT_NAME"])
  >   ]
  >   | .[]'
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
        "UNIT_NAME",
        "foo"
      ]
    ]
  }
  {
    "module_name": "Foo",
    "source_path": "_build/default/foo.ml-gen",
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
        "foo"
      ]
    ]
  }
  {
    "module_name": "Foo__Groupintf__",
    "source_path": "_build/default/foo__Groupintf__",
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
        "foo__Groupintf__"
      ]
    ]
  }
  {
    "module_name": "Foo__Groupintf__",
    "source_path": "_build/default/foo__Groupintf__.ml-gen",
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
        "foo__Groupintf__"
      ]
    ]
  }
  {
    "module_name": "Utils",
    "source_path": "_build/default/foo__Utils",
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
        "foo__Utils"
      ]
    ]
  }
  {
    "module_name": "Utils",
    "source_path": "_build/default/foo__Utils.ml-gen",
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
        "foo__Utils"
      ]
    ]
  }
  {
    "module_name": "Calc",
    "source_path": "_build/default/groupintf/calc",
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
          "Foo",
          "-open",
          "Foo__Groupintf__"
        ]
      ],
      [
        "UNIT_NAME",
        "foo__Groupintf__Calc"
      ]
    ]
  }
  {
    "module_name": "Calc",
    "source_path": "_build/default/groupintf/calc.ml",
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
          "Foo",
          "-open",
          "Foo__Groupintf__"
        ]
      ],
      [
        "UNIT_NAME",
        "foo__Groupintf__Calc"
      ]
    ]
  }
  {
    "module_name": "Groupintf",
    "source_path": "_build/default/groupintf/groupintf",
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
          "Foo",
          "-open",
          "Foo__Groupintf__"
        ]
      ],
      [
        "UNIT_NAME",
        "foo__Groupintf"
      ]
    ]
  }
  {
    "module_name": "Groupintf",
    "source_path": "_build/default/groupintf/groupintf.ml",
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
          "Foo",
          "-open",
          "Foo__Groupintf__"
        ]
      ],
      [
        "UNIT_NAME",
        "foo__Groupintf"
      ]
    ]
  }
  {
    "module_name": "Main",
    "source_path": "_build/default/main",
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
          "Foo"
        ]
      ],
      [
        "UNIT_NAME",
        "foo__Main"
      ]
    ]
  }
  {
    "module_name": "Main",
    "source_path": "_build/default/main.ml",
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
          "Foo"
        ]
      ],
      [
        "UNIT_NAME",
        "foo__Main"
      ]
    ]
  }
  {
    "module_name": "Calc",
    "source_path": "_build/default/utils/calc",
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
          "Foo",
          "-open",
          "Foo__Utils"
        ]
      ],
      [
        "UNIT_NAME",
        "foo__Utils__Calc"
      ]
    ]
  }
  {
    "module_name": "Calc",
    "source_path": "_build/default/utils/calc.ml",
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
          "Foo",
          "-open",
          "Foo__Utils"
        ]
      ],
      [
        "UNIT_NAME",
        "foo__Utils__Calc"
      ]
    ]
  }
  $ dune ocaml merlin dump-config --format=json utils | jq '
  >   include "dune";
  >   [
  >     .[]
  >     | merlinConfigSummary(["FLG", "UNIT_NAME"])
  >   ]
  >   | .[]'
