setup_package_a_requiring_b() {
  mkdir -p source/b
  cat > source/dune-project <<-'EOF'
	(lang dune 3.24)
	(package (name a))
	(package (name b))
	EOF
  cat > source/dune <<-'EOF'
	(library
	 (public_name a)
	 (modes byte)
	 (libraries b))
	EOF
  echo 'let value = B.value + 1' > source/a.ml
}

setup_package_a_consumers() {
  mkdir dune-consumer meta-consumer
  echo '(lang dune 3.24)' > dune-consumer/dune-project
  echo 'let () = Printf.printf "%d\n" A.value' > dune-consumer/main.ml
  cat > dune-consumer/dune <<-'EOF'
	(rule
	 (target main.exe)
	 (deps main.ml (package a))
	 (action
	  (run ocamlfind ocamlc -package a -linkpkg -o %{target} main.ml)))
	EOF
  cp dune-consumer/dune dune-consumer/dune-project dune-consumer/main.ml \
    meta-consumer/
}
