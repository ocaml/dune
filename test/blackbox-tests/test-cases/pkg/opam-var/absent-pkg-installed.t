Test the "installed" variable for packages not in the solution. In opam,
absent packages have installed=false. We test different syntactic contexts:
bare ident, string interpolation, and filter context.

  $ mkrepo

  $ mkpkg "test-installed-var" <<'EOF'
  > build: [
  >   [ "echo" absent:installed ]
  >   [ "echo" "%{absent:installed}%" ]
  >   [ "echo" "%{absent:installed?yes:no}%" ]
  >   [ "echo" "installed" ] {absent:installed}
  >   [ "echo" "not installed" ] {!absent:installed}
  > ]
  > EOF

  $ solve test-installed-var
  Solution for dune.lock:
  - test-installed-var.0.0.1

The installed variable resolves to "false" at solve time (matching opam
semantics). In filter context, commands with always-false conditions are
removed, and commands with always-true conditions have their filters removed:

  $ cat dune.lock/test-installed-var.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms
    ((action
      (progn
       (run echo false)
       (run echo false)
       (run echo no)
       (run echo "not installed"))))))

The "enable" variable is desugared to "installed?enable:disable". Since
"installed" is known to be false for absent packages, the conditional is
evaluated at solve time:

  $ mkpkg "test-enable-var" <<'EOF'
  > build: [
  >   [ "echo" absent:enable ]
  >   [ "echo" "%{absent:enable}%" ]
  >   [ "echo" "%{absent:enable?yes:no}%" ]
  > ]
  > EOF

  $ solve test-enable-var
  Solution for dune.lock:
  - test-enable-var.0.0.1

The first two cases resolve to "disable" because "enable" is desugared to
"installed?enable:disable", and installed=false for absent packages.

The third case "%{absent:enable?yes:no}%" is different: in opam, "enable" is a
pseudo-variable that only gets desugared when used without a "string converter"
(opam's term for the "?if_true:if_false" ternary suffix).
With a converter present, "enable" is treated as a literal variable name, but
no package actually defines an "enable" variable - it only exists as syntactic
sugar. Therefore "enable" is undefined, and undefined variables in ternary
context evaluate to the fallback ("no"). We could simplify this to "no" at
solve time, but it is tricky and this pattern does not appear in practice:

  $ cat dune.lock/test-enable-var.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms
    ((action
      (progn
       (run echo disable)
       (run echo disable)
       (run echo (if (catch_undefined_var %{pkg:absent:enable} false) yes no)))))))

