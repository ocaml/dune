Test that the rule trace category emits info events with both deps (inputs) and
targets (outputs) with file hashes, enabling build graph analysis.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF

Create a chain of libraries: base -> mid -> top -> main.exe

  $ mkdir -p base
  $ cat > base/dune <<EOF
  > (library (name base))
  > EOF

  $ cat > base/core.ml <<EOF
  > let value = 42
  > EOF

  $ mkdir -p mid
  $ cat > mid/dune <<EOF
  > (library (name mid) (libraries base))
  > EOF

  $ cat > mid/helper.ml <<EOF
  > let doubled = Base.Core.value * 2
  > EOF

  $ mkdir -p top
  $ cat > top/dune <<EOF
  > (library (name top) (libraries mid))
  > EOF

  $ cat > top/api.ml <<EOF
  > let result = Mid.Helper.doubled + 1
  > EOF

  $ cat > dune <<EOF
  > (executable (name main) (libraries top))
  > EOF

  $ cat > main.ml <<EOF
  > let () = Printf.printf "Result: %d\n" Top.Api.result
  > EOF

Build with rule tracing enabled:

  $ export DUNE_TRACE=rule
  $ dune build main.exe

Dump all rule info events (scrubbing hashes):

  $ dune trace cat | jq 'select(.cat == "rule") | del(.ts, .dur)' | sed 's/[0-9a-f]\{32\}/<hash>/g'
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": [],
      "targets": {
        "_build/default/.dune/configurator": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": [],
      "targets": {
        "_build/default/.dune/configurator.v2": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": [],
      "targets": {
        "_build/default/.merlin-conf/exe-main": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": [],
      "targets": {
        "_build/default/base/.merlin-conf/lib-base": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": [],
      "targets": {
        "_build/default/base/base.ml-gen": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": {
        "base/core.ml": "<hash>"
      },
      "targets": {
        "_build/default/base/core.ml": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": [],
      "targets": {
        "_build/default/mid/.merlin-conf/lib-mid": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": [],
      "targets": {
        "_build/default/mid/mid.ml-gen": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": {
        "mid/helper.ml": "<hash>"
      },
      "targets": {
        "_build/default/mid/helper.ml": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": [],
      "targets": {
        "_build/default/top/.merlin-conf/lib-top": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": [],
      "targets": {
        "_build/default/top/top.ml-gen": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": {
        "top/api.ml": "<hash>"
      },
      "targets": {
        "_build/default/top/api.ml": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": [],
      "targets": {
        "_build/default/main.mli": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": {
        "main.ml": "<hash>"
      },
      "targets": {
        "_build/default/main.ml": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": {
        "_build/default/base/base.ml-gen": "<hash>",
        "/nix/store/0gxysxzzvzqm2m2aznix02pm05bmqm1q-ocaml+fp-5.4.0/bin/ocamlc.opt": "<hash>"
      },
      "targets": {
        "_build/default/base/.base.objs/byte/base.cmi": "<hash>",
        "_build/default/base/.base.objs/byte/base.cmo": "<hash>",
        "_build/default/base/.base.objs/byte/base.cmt": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": {
        "_build/default/base/core.ml": "<hash>",
        "/nix/store/0gxysxzzvzqm2m2aznix02pm05bmqm1q-ocaml+fp-5.4.0/bin/ocamldep.opt": "<hash>"
      },
      "targets": {
        "_build/default/base/.base.objs/base__Core.impl.d": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": [],
      "targets": {
        "_build/default/base/.base.objs/base__Core.impl.all-deps": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": {
        "_build/default/mid/mid.ml-gen": "<hash>",
        "/nix/store/0gxysxzzvzqm2m2aznix02pm05bmqm1q-ocaml+fp-5.4.0/bin/ocamlc.opt": "<hash>"
      },
      "targets": {
        "_build/default/mid/.mid.objs/byte/mid.cmi": "<hash>",
        "_build/default/mid/.mid.objs/byte/mid.cmo": "<hash>",
        "_build/default/mid/.mid.objs/byte/mid.cmt": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": {
        "_build/default/mid/helper.ml": "<hash>",
        "/nix/store/0gxysxzzvzqm2m2aznix02pm05bmqm1q-ocaml+fp-5.4.0/bin/ocamldep.opt": "<hash>"
      },
      "targets": {
        "_build/default/mid/.mid.objs/mid__Helper.impl.d": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": [],
      "targets": {
        "_build/default/mid/.mid.objs/mid__Helper.impl.all-deps": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": {
        "_build/default/top/top.ml-gen": "<hash>",
        "/nix/store/0gxysxzzvzqm2m2aznix02pm05bmqm1q-ocaml+fp-5.4.0/bin/ocamlc.opt": "<hash>"
      },
      "targets": {
        "_build/default/top/.top.objs/byte/top.cmi": "<hash>",
        "_build/default/top/.top.objs/byte/top.cmo": "<hash>",
        "_build/default/top/.top.objs/byte/top.cmt": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": {
        "_build/default/top/api.ml": "<hash>",
        "/nix/store/0gxysxzzvzqm2m2aznix02pm05bmqm1q-ocaml+fp-5.4.0/bin/ocamldep.opt": "<hash>"
      },
      "targets": {
        "_build/default/top/.top.objs/top__Api.impl.d": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": [],
      "targets": {
        "_build/default/top/.top.objs/top__Api.impl.all-deps": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": {
        "_build/default/base/base.ml-gen": "<hash>",
        "_build/default/base/.base.objs/byte/base.cmi": "<hash>",
        "/nix/store/0gxysxzzvzqm2m2aznix02pm05bmqm1q-ocaml+fp-5.4.0/bin/ocamlopt.opt": "<hash>"
      },
      "targets": {
        "_build/default/base/.base.objs/native/base.cmx": "<hash>",
        "_build/default/base/.base.objs/native/base.o": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": {
        "_build/default/base/core.ml": "<hash>",
        "_build/default/base/.base.objs/byte/base.cmi": "<hash>",
        "/nix/store/0gxysxzzvzqm2m2aznix02pm05bmqm1q-ocaml+fp-5.4.0/bin/ocamlc.opt": "<hash>"
      },
      "targets": {
        "_build/default/base/.base.objs/byte/base__Core.cmi": "<hash>",
        "_build/default/base/.base.objs/byte/base__Core.cmo": "<hash>",
        "_build/default/base/.base.objs/byte/base__Core.cmt": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": {
        "_build/default/mid/mid.ml-gen": "<hash>",
        "_build/default/mid/.mid.objs/byte/mid.cmi": "<hash>",
        "/nix/store/0gxysxzzvzqm2m2aznix02pm05bmqm1q-ocaml+fp-5.4.0/bin/ocamlopt.opt": "<hash>"
      },
      "targets": {
        "_build/default/mid/.mid.objs/native/mid.cmx": "<hash>",
        "_build/default/mid/.mid.objs/native/mid.o": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": {
        "_build/default/top/top.ml-gen": "<hash>",
        "_build/default/top/.top.objs/byte/top.cmi": "<hash>",
        "/nix/store/0gxysxzzvzqm2m2aznix02pm05bmqm1q-ocaml+fp-5.4.0/bin/ocamlopt.opt": "<hash>"
      },
      "targets": {
        "_build/default/top/.top.objs/native/top.cmx": "<hash>",
        "_build/default/top/.top.objs/native/top.o": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": {
        "_build/default/base/core.ml": "<hash>",
        "_build/default/base/.base.objs/byte/base__Core.cmi": "<hash>",
        "_build/default/base/.base.objs/byte/base.cmi": "<hash>",
        "/nix/store/0gxysxzzvzqm2m2aznix02pm05bmqm1q-ocaml+fp-5.4.0/bin/ocamlopt.opt": "<hash>"
      },
      "targets": {
        "_build/default/base/.base.objs/native/base__Core.cmx": "<hash>",
        "_build/default/base/.base.objs/native/base__Core.o": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": {
        "_build/default/base/.base.objs/byte": "<hash>",
        "_build/default/mid/helper.ml": "<hash>",
        "_build/default/mid/.mid.objs/byte/mid.cmi": "<hash>",
        "/nix/store/0gxysxzzvzqm2m2aznix02pm05bmqm1q-ocaml+fp-5.4.0/bin/ocamlc.opt": "<hash>"
      },
      "targets": {
        "_build/default/mid/.mid.objs/byte/mid__Helper.cmi": "<hash>",
        "_build/default/mid/.mid.objs/byte/mid__Helper.cmo": "<hash>",
        "_build/default/mid/.mid.objs/byte/mid__Helper.cmt": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": {
        "_build/default/base/.base.objs/native/base__Core.o": "<hash>",
        "_build/default/base/.base.objs/native/base__Core.cmx": "<hash>",
        "_build/default/base/.base.objs/native/base.o": "<hash>",
        "_build/default/base/.base.objs/native/base.cmx": "<hash>",
        "/nix/store/0gxysxzzvzqm2m2aznix02pm05bmqm1q-ocaml+fp-5.4.0/bin/ocamlopt.opt": "<hash>"
      },
      "targets": {
        "_build/default/base/base.a": "<hash>",
        "_build/default/base/base.cmxa": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": {
        "_build/default/base/.base.objs/byte": "<hash>",
        "_build/default/mid/helper.ml": "<hash>",
        "_build/default/mid/.mid.objs/byte/mid__Helper.cmi": "<hash>",
        "_build/default/mid/.mid.objs/byte/mid.cmi": "<hash>",
        "/nix/store/0gxysxzzvzqm2m2aznix02pm05bmqm1q-ocaml+fp-5.4.0/bin/ocamlopt.opt": "<hash>"
      },
      "targets": {
        "_build/default/mid/.mid.objs/native/mid__Helper.cmx": "<hash>",
        "_build/default/mid/.mid.objs/native/mid__Helper.o": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": {
        "_build/default/mid/.mid.objs/byte": "<hash>",
        "_build/default/base/.base.objs/byte": "<hash>",
        "_build/default/top/api.ml": "<hash>",
        "_build/default/top/.top.objs/byte/top.cmi": "<hash>",
        "/nix/store/0gxysxzzvzqm2m2aznix02pm05bmqm1q-ocaml+fp-5.4.0/bin/ocamlc.opt": "<hash>"
      },
      "targets": {
        "_build/default/top/.top.objs/byte/top__Api.cmi": "<hash>",
        "_build/default/top/.top.objs/byte/top__Api.cmo": "<hash>",
        "_build/default/top/.top.objs/byte/top__Api.cmt": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": {
        "_build/default/mid/.mid.objs/native/mid__Helper.o": "<hash>",
        "_build/default/mid/.mid.objs/native/mid__Helper.cmx": "<hash>",
        "_build/default/mid/.mid.objs/native/mid.o": "<hash>",
        "_build/default/mid/.mid.objs/native/mid.cmx": "<hash>",
        "/nix/store/0gxysxzzvzqm2m2aznix02pm05bmqm1q-ocaml+fp-5.4.0/bin/ocamlopt.opt": "<hash>"
      },
      "targets": {
        "_build/default/mid/mid.a": "<hash>",
        "_build/default/mid/mid.cmxa": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": {
        "_build/default/mid/.mid.objs/byte": "<hash>",
        "_build/default/base/.base.objs/byte": "<hash>",
        "_build/default/top/api.ml": "<hash>",
        "_build/default/top/.top.objs/byte/top__Api.cmi": "<hash>",
        "_build/default/top/.top.objs/byte/top.cmi": "<hash>",
        "/nix/store/0gxysxzzvzqm2m2aznix02pm05bmqm1q-ocaml+fp-5.4.0/bin/ocamlopt.opt": "<hash>"
      },
      "targets": {
        "_build/default/top/.top.objs/native/top__Api.cmx": "<hash>",
        "_build/default/top/.top.objs/native/top__Api.o": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": {
        "_build/default/top/.top.objs/byte": "<hash>",
        "_build/default/mid/.mid.objs/byte": "<hash>",
        "_build/default/base/.base.objs/byte": "<hash>",
        "_build/default/main.mli": "<hash>",
        "/nix/store/0gxysxzzvzqm2m2aznix02pm05bmqm1q-ocaml+fp-5.4.0/bin/ocamlc.opt": "<hash>"
      },
      "targets": {
        "_build/default/.main.eobjs/byte/dune__exe__Main.cmi": "<hash>",
        "_build/default/.main.eobjs/byte/dune__exe__Main.cmti": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": {
        "_build/default/top/.top.objs/native/top__Api.o": "<hash>",
        "_build/default/top/.top.objs/native/top__Api.cmx": "<hash>",
        "_build/default/top/.top.objs/native/top.o": "<hash>",
        "_build/default/top/.top.objs/native/top.cmx": "<hash>",
        "/nix/store/0gxysxzzvzqm2m2aznix02pm05bmqm1q-ocaml+fp-5.4.0/bin/ocamlopt.opt": "<hash>"
      },
      "targets": {
        "_build/default/top/top.a": "<hash>",
        "_build/default/top/top.cmxa": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": {
        "_build/default/top/.top.objs/byte": "<hash>",
        "_build/default/mid/.mid.objs/byte": "<hash>",
        "_build/default/base/.base.objs/byte": "<hash>",
        "_build/default/main.ml": "<hash>",
        "_build/default/.main.eobjs/byte/dune__exe__Main.cmi": "<hash>",
        "/nix/store/0gxysxzzvzqm2m2aznix02pm05bmqm1q-ocaml+fp-5.4.0/bin/ocamlopt.opt": "<hash>"
      },
      "targets": {
        "_build/default/.main.eobjs/native/dune__exe__Main.cmx": "<hash>",
        "_build/default/.main.eobjs/native/dune__exe__Main.o": "<hash>"
      }
    }
  }
  {
    "cat": "rule",
    "name": "info",
    "args": {
      "rule_digest": "<hash>",
      "deps": {
        "_build/default/top/top.cmxa": "<hash>",
        "_build/default/top/top.a": "<hash>",
        "_build/default/mid/mid.cmxa": "<hash>",
        "_build/default/mid/mid.a": "<hash>",
        "_build/default/base/base.cmxa": "<hash>",
        "_build/default/base/base.a": "<hash>",
        "_build/default/.main.eobjs/native/dune__exe__Main.o": "<hash>",
        "_build/default/.main.eobjs/native/dune__exe__Main.cmx": "<hash>",
        "/nix/store/0gxysxzzvzqm2m2aznix02pm05bmqm1q-ocaml+fp-5.4.0/bin/ocamlopt.opt": "<hash>"
      },
      "targets": {
        "_build/default/main.exe": "<hash>"
      }
    }
  }
