The %{melange.emit:...} macro expands to the output directory for a
melange.emit stanza. The macro argument is interpreted as a path to the
stanza's target directory, relative to the dune file where the macro appears.

  $ make_melange_project 3.25 1.0

  $ mkdir -p lib
  $ cat > lib/dune <<EOF
  > (melange.emit
  >  (target out)
  >  (modules index)
  >  (emit_stdlib false))
  > 
  > (melange.emit
  >  (target out2)
  >  (modules other)
  >  (emit_stdlib false))
  > 
  > (rule
  >  (target local-paths)
  >  (deps
  >   %{melange.emit:out}
  >   %{melange.emit:out2})
  >  (action
  >   (with-stdout-to %{target}
  >    (progn
  >     (echo "%{melange.emit:out}\n")
  >     (echo "%{melange.emit:out2}\n")))))
  > EOF

  $ cat > lib/index.ml <<EOF
  > let () = Js.log "index"
  > EOF
  $ cat > lib/other.ml <<EOF
  > let () = Js.log "other"
  > EOF

  $ cat > dune <<EOF
  > (rule
  >  (target root-paths)
  >  (deps
  >   %{melange.emit:lib/out}
  >   %{melange.emit:lib/out2})
  >  (action
  >   (with-stdout-to %{target}
  >    (progn
  >     (echo "%{melange.emit:lib/out}\n")
  >     (echo "%{melange.emit:lib/out2}\n")))))
  > EOF

  $ dune build lib/local-paths root-paths
  $ cat _build/default/lib/local-paths
  out/lib
  out2/lib
  $ cat _build/default/root-paths
  lib/out/lib
  lib/out2/lib

Using the macro as a dependency also builds the JavaScript outputs for the
selected emits.

  $ test -f _build/default/lib/out/lib/index.js
  $ test -f _build/default/lib/out2/lib/other.js

The macro also works when the melange.emit stanza uses a custom alias.

  $ mkdir -p custom
  $ cat > custom/dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (alias app)
  >  (modules main)
  >  (emit_stdlib false))
  > 
  > (rule
  >  (target custom-path)
  >  (action
  >   (with-stdout-to %{target}
  >    (echo "%{melange.emit:dist}\n"))))
  > EOF

  $ cat > custom/main.ml <<EOF
  > let () = Js.log "custom"
  > EOF

  $ dune build custom/custom-path
  $ cat _build/default/custom/custom-path
  dist/custom
  $ test -f _build/default/custom/dist/custom/main.js

An unknown melange.emit target is rejected.

  $ cat >> dune <<EOF
  > (rule
  >  (alias unknown-emit)
  >  (action (echo "%{melange.emit:missing}")))
  > EOF

  $ dune build @unknown-emit
  File "dune", line 13, characters 16-39:
  13 |  (action (echo "%{melange.emit:missing}")))
                       ^^^^^^^^^^^^^^^^^^^^^^^
  Error: Melange emit target "missing" does not exist.
  [1]

The macro cannot refer to a target outside the workspace root.

  $ cat >> dune <<EOF
  > (rule
  >  (alias escaped-emit)
  >  (action (echo "%{melange.emit:../outside}")))
  > EOF

  $ dune build @escaped-emit
  File "dune", line 16, characters 16-42:
  16 |  (action (echo "%{melange.emit:../outside}")))
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: cannot escape the workspace root directory
  [1]

The macro is only available starting with Dune 3.25.

  $ mkdir old-language
  $ cat > old-language/dune-project <<EOF
  > (lang dune 3.24)
  > EOF
  $ cat > old-language/dune <<EOF
  > (rule
  >  (alias default)
  >  (action (echo "%{melange.emit:out}")))
  > EOF
  $ dune build --root old-language
  Entering directory 'old-language'
  File "dune", line 3, characters 16-35:
  3 |  (action (echo "%{melange.emit:out}")))
                      ^^^^^^^^^^^^^^^^^^^
  Error: %{melange.emit:..} is only available since version 3.25 of the dune
  language. Please update your dune-project file to have (lang dune 3.25).
  Leaving directory 'old-language'
  [1]
