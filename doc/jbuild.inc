
(rule
 ((targets (jbuilder-build.1))
  (action  (with-stdout-to ${@}
            (run ${bin:jbuilder} build --help=groff)))))

(install
 ((section man)
  (files (jbuilder-build.1))))

(rule
 ((targets (jbuilder-clean.1))
  (action  (with-stdout-to ${@}
            (run ${bin:jbuilder} clean --help=groff)))))

(install
 ((section man)
  (files (jbuilder-clean.1))))

(rule
 ((targets (jbuilder-exec.1))
  (action  (with-stdout-to ${@}
            (run ${bin:jbuilder} exec --help=groff)))))

(install
 ((section man)
  (files (jbuilder-exec.1))))

(rule
 ((targets (jbuilder-external-lib-deps.1))
  (action  (with-stdout-to ${@}
            (run ${bin:jbuilder} external-lib-deps --help=groff)))))

(install
 ((section man)
  (files (jbuilder-external-lib-deps.1))))

(rule
 ((targets (jbuilder-install.1))
  (action  (with-stdout-to ${@}
            (run ${bin:jbuilder} install --help=groff)))))

(install
 ((section man)
  (files (jbuilder-install.1))))

(rule
 ((targets (jbuilder-installed-libraries.1))
  (action  (with-stdout-to ${@}
            (run ${bin:jbuilder} installed-libraries --help=groff)))))

(install
 ((section man)
  (files (jbuilder-installed-libraries.1))))

(rule
 ((targets (jbuilder-rules.1))
  (action  (with-stdout-to ${@}
            (run ${bin:jbuilder} rules --help=groff)))))

(install
 ((section man)
  (files (jbuilder-rules.1))))

(rule
 ((targets (jbuilder-runtest.1))
  (action  (with-stdout-to ${@}
            (run ${bin:jbuilder} runtest --help=groff)))))

(install
 ((section man)
  (files (jbuilder-runtest.1))))

(rule
 ((targets (jbuilder-subst.1))
  (action  (with-stdout-to ${@}
            (run ${bin:jbuilder} subst --help=groff)))))

(install
 ((section man)
  (files (jbuilder-subst.1))))

(rule
 ((targets (jbuilder-uninstall.1))
  (action  (with-stdout-to ${@}
            (run ${bin:jbuilder} uninstall --help=groff)))))

(install
 ((section man)
  (files (jbuilder-uninstall.1))))

(rule
 ((targets (jbuilder-utop.1))
  (action  (with-stdout-to ${@}
            (run ${bin:jbuilder} utop --help=groff)))))

(install
 ((section man)
  (files (jbuilder-utop.1))))

