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
  File "dune", line 4, characters 2-25:
  4 |   %{melange.emit:lib/out}
        ^^^^^^^^^^^^^^^^^^^^^^^
  Error: Unknown macro %{melange.emit:..}
  File "lib/dune", line 14, characters 2-21:
  14 |   %{melange.emit:out}
         ^^^^^^^^^^^^^^^^^^^
  Error: Unknown macro %{melange.emit:..}
  [1]
  $ cat _build/default/lib/local-paths
  cat: _build/default/lib/local-paths: No such file or directory
  [1]
  $ cat _build/default/root-paths
  cat: _build/default/root-paths: No such file or directory
  [1]

Using the macro as a dependency also builds the JavaScript outputs for the
selected emits.

  $ test -f _build/default/lib/out/lib/index.js
  [1]
  $ test -f _build/default/lib/out2/lib/other.js
  [1]

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
  File "custom/dune", line 11, characters 10-30:
  11 |    (echo "%{melange.emit:dist}\n"))))
                 ^^^^^^^^^^^^^^^^^^^^
  Error: Unknown macro %{melange.emit:..}
  File "dune", line 4, characters 2-25:
  4 |   %{melange.emit:lib/out}
        ^^^^^^^^^^^^^^^^^^^^^^^
  Error: Unknown macro %{melange.emit:..}
  File "lib/dune", line 14, characters 2-21:
  14 |   %{melange.emit:out}
         ^^^^^^^^^^^^^^^^^^^
  Error: Unknown macro %{melange.emit:..}
  [1]
  $ cat _build/default/custom/custom-path
  cat: _build/default/custom/custom-path: No such file or directory
  [1]
  $ test -f _build/default/custom/dist/custom/main.js
  [1]
