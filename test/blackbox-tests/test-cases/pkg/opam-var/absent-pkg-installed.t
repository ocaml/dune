Test the "installed" variable for packages not in the solution in different
contexts: string interpolation vs truthy/filter context.

  $ mkrepo

First, test the variable in string interpolation context (command argument):

  $ mkpkg "string-context" <<'EOF'
  > build: [
  >   [ "echo" "%{not-in-lock:installed}%" ]
  > ]
  > EOF

  $ solve string-context
  Solution for dune.lock:
  - string-context.0.0.1

The variable resolves to "false" at solve time:

  $ cat dune.lock/string-context.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms ((action (run echo false)))))




Now test the variable in truthy/filter context (conditional on command):

  $ mkpkg "truthy-context" <<'EOF'
  > build: [
  >   [ "echo" "yes" ] {not-in-lock:installed}
  >   [ "echo" "no" ] {!not-in-lock:installed}
  > ]
  > EOF

  $ solve truthy-context
  Solution for dune.lock:
  - truthy-context.0.0.1

In truthy context, the variable evaluates to false at solve time. Commands with
always-false conditions are removed, and commands with always-true conditions
have their filters removed:

  $ cat dune.lock/truthy-context.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms ((action (run echo no)))))





The "enable" variable is desugared to "installed?enable:disable". Since
"installed" is known to be false for absent packages, the conditional is
evaluated at solve time:

  $ mkpkg "enable-context" <<'EOF'
  > build: [
  >   [ "echo" "%{not-in-lock:enable}%" ]
  > ]
  > EOF

  $ solve enable-context
  Solution for dune.lock:
  - enable-context.0.0.1

The conditional evaluates to "disable" at solve time since "installed" is false:

  $ cat dune.lock/enable-context.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms ((action (run echo disable)))))




