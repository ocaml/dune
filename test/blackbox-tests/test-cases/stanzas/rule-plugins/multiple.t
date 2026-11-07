Multiple plugins in one dune file are called in source order, and each process
is reused.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.26)
  > (using plugin 0.1)
  > EOF
  $ for name in one two; do
  >   mkdir "$name"
  >   cat > "$name/dune" <<EOF
  > (executable
  >  (name rules)
  >  (libraries dune-rules-plugin))
  > (plugin
  >  (name $name)
  >  (executable ./rules.exe))
  > EOF
  >   cat > "$name/rules.ml" <<EOF
  > open Dune_rules_plugin
  > let requests = ref 0
  > let atom = function
  >   | Sexp.Atom atom -> atom
  >   | Quoted_string _ | Template _ | List _ -> invalid_arg "atom expected"
  > ;;
  > let () =
  >   prerr_endline "$name started";
  >   run (function
  >     | [ argument ] ->
  >       incr requests;
  >       Printf.eprintf "$name %s %d\n%!" (atom argument) !requests;
  >       []
  >     | _ -> invalid_arg "one argument expected")
  > ;;
  > EOF
  > done
  $ mkdir use
  $ cat > use/dune <<'EOF'
  > (plugin.one first)
  > (plugin.two second)
  > (plugin.one third)
  > EOF

  $ dune build use
  one started
  one first 1
  two started
  two second 1
  one third 2
