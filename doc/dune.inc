
(rule
 ((targets (dune-build.1))
  (action  (with-stdout-to ${@}
            (run dune build --help=groff)))))

(install
 ((section man)
  (files (dune-build.1))))

(rule
 ((targets (dune-clean.1))
  (action  (with-stdout-to ${@}
            (run dune clean --help=groff)))))

(install
 ((section man)
  (files (dune-clean.1))))

(rule
 ((targets (dune-exec.1))
  (action  (with-stdout-to ${@}
            (run dune exec --help=groff)))))

(install
 ((section man)
  (files (dune-exec.1))))

(rule
 ((targets (dune-external-lib-deps.1))
  (action  (with-stdout-to ${@}
            (run dune external-lib-deps --help=groff)))))

(install
 ((section man)
  (files (dune-external-lib-deps.1))))

(rule
 ((targets (dune-help.1))
  (action  (with-stdout-to ${@}
            (run dune help --help=groff)))))

(install
 ((section man)
  (files (dune-help.1))))

(rule
 ((targets (dune-install.1))
  (action  (with-stdout-to ${@}
            (run dune install --help=groff)))))

(install
 ((section man)
  (files (dune-install.1))))

(rule
 ((targets (dune-installed-libraries.1))
  (action  (with-stdout-to ${@}
            (run dune installed-libraries --help=groff)))))

(install
 ((section man)
  (files (dune-installed-libraries.1))))

(rule
 ((targets (dune-printenv.1))
  (action  (with-stdout-to ${@}
            (run dune printenv --help=groff)))))

(install
 ((section man)
  (files (dune-printenv.1))))

(rule
 ((targets (dune-promote.1))
  (action  (with-stdout-to ${@}
            (run dune promote --help=groff)))))

(install
 ((section man)
  (files (dune-promote.1))))

(rule
 ((targets (dune-rules.1))
  (action  (with-stdout-to ${@}
            (run dune rules --help=groff)))))

(install
 ((section man)
  (files (dune-rules.1))))

(rule
 ((targets (dune-runtest.1))
  (action  (with-stdout-to ${@}
            (run dune runtest --help=groff)))))

(install
 ((section man)
  (files (dune-runtest.1))))

(rule
 ((targets (dune-subst.1))
  (action  (with-stdout-to ${@}
            (run dune subst --help=groff)))))

(install
 ((section man)
  (files (dune-subst.1))))

(rule
 ((targets (dune-uninstall.1))
  (action  (with-stdout-to ${@}
            (run dune uninstall --help=groff)))))

(install
 ((section man)
  (files (dune-uninstall.1))))

(rule
 ((targets (dune-utop.1))
  (action  (with-stdout-to ${@}
            (run dune utop --help=groff)))))

(install
 ((section man)
  (files (dune-utop.1))))

