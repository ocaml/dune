setup_installed_vlib_projects() {
  mkdir -p producer/vlib consumer/impl
  cat > producer/dune-project <<-'EOF'
	(lang dune 3.24)
	(using melange 1.0)
	(package (name repro))
	EOF
  cat > consumer/dune-project <<-'EOF'
	(lang dune 3.24)
	(using melange 1.0)
	(package (name consumer))
	EOF
  cat > consumer/impl/dune <<-'EOF'
	(library
	 (name impl)
	 (public_name consumer.impl)
	 (modes melange)
	 (implements repro.vlib))
	EOF
}

setup_no_melobjinfo_path() {
  local program
  mkdir no-melobjinfo-bin
  for program in dune melc ocamlc ocamlopt ocamldep ocamlobjinfo; do
    command -v "$program" > "no-melobjinfo-bin/$program.target"
    cat > "no-melobjinfo-bin/$program" <<-'EOF'
	#!/bin/sh
	IFS= read -r target < "$0.target"
	exec "$target" "$@"
	EOF
    chmod +x "no-melobjinfo-bin/$program"
  done
  no_melobjinfo_path="$PWD/no-melobjinfo-bin"
}

write_vlib_emit_stanza() {
  cat > consumer/dune <<-'EOF'
	(melange.emit
	 (target output)
	 (emit_stdlib false)
	 (compile_flags :standard --mel-cross-module-opt)
	 (libraries impl))
	EOF
}

show_installed_vlib_deps() {
  local tool_path="$1"
  shift
  PATH="$tool_path" OCAMLPATH="$PWD/prefix/lib:$OCAMLPATH" \
    dune rules --root consumer --recursive --format=json --deps \
    --display=quiet impl/.impl.objs/melange/vlib__Virt.cmj >deps.json || return
  jq_dune -r '
    [ $ARGS.positional[] | ("vlib__" + .) + (".cmi", ".cmj") ] as $files
    | [.[] | depsFilePaths
       | select(startswith("_build/default/impl/.impl.objs/melange/"))
       | select(. as $path |
           any($files[]; . as $file | $path | endswith($file))) ]
    | unique[]
  ' --args "$@" < deps.json
}
