
(rule
 (with-stdout-to dune-promote.1
  (run dune promote --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-promote.1))

(rule
 (with-stdout-to dune-test.1
  (run dune test --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-test.1))

(rule
 (with-stdout-to dune-build.1
  (run dune build --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-build.1))

(rule
 (with-stdout-to dune-cache.1
  (run dune cache --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-cache.1))

(rule
 (with-stdout-to dune-clean.1
  (run dune clean --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-clean.1))

(rule
 (with-stdout-to dune-coq.1
  (run dune coq --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-coq.1))

(rule
 (with-stdout-to dune-describe.1
  (run dune describe --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-describe.1))

(rule
 (with-stdout-to dune-diagnostics.1
  (run dune diagnostics --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-diagnostics.1))

(rule
 (with-stdout-to dune-exec.1
  (run dune exec --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-exec.1))

(rule
 (with-stdout-to dune-external-lib-deps.1
  (run dune external-lib-deps --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-external-lib-deps.1))

(rule
 (with-stdout-to dune-fmt.1
  (run dune fmt --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-fmt.1))

(rule
 (with-stdout-to dune-format-dune-file.1
  (run dune format-dune-file --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-format-dune-file.1))

(rule
 (with-stdout-to dune-help.1
  (run dune help --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-help.1))

(rule
 (with-stdout-to dune-init.1
  (run dune init --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-init.1))

(rule
 (with-stdout-to dune-install.1
  (run dune install --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-install.1))

(rule
 (with-stdout-to dune-installed-libraries.1
  (run dune installed-libraries --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-installed-libraries.1))

(rule
 (with-stdout-to dune-internal.1
  (run dune internal --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-internal.1))

(rule
 (with-stdout-to dune-ocaml.1
  (run dune ocaml --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-ocaml.1))

(rule
 (with-stdout-to dune-ocaml-merlin.1
  (run dune ocaml-merlin --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-ocaml-merlin.1))

(rule
 (with-stdout-to dune-printenv.1
  (run dune printenv --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-printenv.1))

(rule
 (with-stdout-to dune-promotion.1
  (run dune promotion --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-promotion.1))

(rule
 (with-stdout-to dune-rpc.1
  (run dune rpc --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-rpc.1))

(rule
 (with-stdout-to dune-rules.1
  (run dune rules --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-rules.1))

(rule
 (with-stdout-to dune-runtest.1
  (run dune runtest --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-runtest.1))

(rule
 (with-stdout-to dune-shutdown.1
  (run dune shutdown --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-shutdown.1))

(rule
 (with-stdout-to dune-subst.1
  (run dune subst --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-subst.1))

(rule
 (with-stdout-to dune-top.1
  (run dune top --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-top.1))

(rule
 (with-stdout-to dune-uninstall.1
  (run dune uninstall --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-uninstall.1))

(rule
 (with-stdout-to dune-upgrade.1
  (run dune upgrade --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-upgrade.1))

(rule
 (with-stdout-to dune-utop.1
  (run dune utop --help=groff)))

(install
 (section man)
 (package dune)
 (files   dune-utop.1))

