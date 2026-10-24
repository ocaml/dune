A dune file in OCaml syntax is evaluated by running [ocaml] on it, with the
environment taken from [Context.installed_env] rather than the per-directory
environment an action gets. There is no owning directory to narrow to here, so
it sees the [PATH] of every package in the lock directory, the same as a
directory without an owning package does.

  $ make_lockdir

  $ make_lockpkg provider <<'EOF'
  > (version 0.0.1)
  > (build
  >  (progn
  >   (system "\| cat > mybin <<'EOI'
  >           "\| #!/bin/sh
  >           "\| echo from provider
  >           "\| EOI
  >   )
  >   (system "chmod +x mybin")
  >   (system "echo 'bin: [ \"mybin\" ]' > provider.install")
  >  ))
  > EOF

  $ make_dune_project 3.16

The plugin reports whether its own [PATH] mentions any lock directory package,
and emits a rule whose action reports the [PATH] the action itself is given:

  $ cat >dune <<'EOF'
  > (* -*- tuareg -*- *)
  > let path = try Sys.getenv "PATH" with Not_found -> ""
  > let mentions_lockdir =
  >   let needle = ".pkg" in
  >   let n = String.length needle and len = String.length path in
  >   let rec loop i =
  >     if i + n > len then false
  >     else if String.sub path i n = needle then true
  >     else loop (i + 1)
  >   in
  >   loop 0
  > let () =
  >   Jbuild_plugin.V1.send
  >     ("(rule (with-stdout-to plugin-sees-lockdir (echo \"" ^ string_of_bool mentions_lockdir ^ "\")))
  >       (rule (with-stdout-to action-path (bash \"echo $PATH\")))")
  > EOF

  $ dune build plugin-sees-lockdir action-path

The action is given the lock directory's bin directory:

  $ env_added "$(cat _build/default/action-path)" "$PATH" | censor
  $PWD/_build/_private/default/.pkg/provider.0.0.1-$DIGEST/target/bin

The plugin, evaluated through [Context.installed_env], is not:

  $ cat _build/default/plugin-sees-lockdir
  true
