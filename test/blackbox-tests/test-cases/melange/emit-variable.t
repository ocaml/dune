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
