Verify that the run with path action correctly adds Dune to PATH on all platforms.

  $ make_lockdir

  $ pkg() {
  > make_lockpkg $1<<EOF
  > (build (run ocaml $2))
  > (version dev)
  > EOF
  > local files="${source_lock_dir}/$1.files"
  > local exec_path="${source_lock_dir}/$1.files/$2"
  > mkdir -p "${files}"
  > touch "${exec_path}"
  > cat > "${exec_path}"
  > }

  $ pkg foo foo.ml<<'EOF'
  > let () =
  >  let pathsep = if Sys.win32 then ';' else ':' in
  >  let path = Sys.getenv "PATH" |> String.split_on_char pathsep |> List.hd in
  >  if (Filename.basename path |> String.ends_with ~suffix:"self-in-path")
  >  then print_endline "$PATH correct!"
  >  else print_endline "$PATH incorrect!"
  > EOF

  $ dune build @pkg-install 2>&1
  $PATH incorrect!
