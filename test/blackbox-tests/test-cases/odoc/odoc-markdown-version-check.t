Testing that odoc markdown generation is only enabled with odoc >= 3.1.0

Create a simple project with odoc documentation:
  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > (package (name foo))
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (public_name foo)
  >  (name foo))
  > EOF

  $ cat > foo.ml <<EOF
  > (** This is a test module *)
  > let x = 42
  > EOF

  $ cat > foo.mli <<EOF
  > (** This is the interface for the test module *)
  > val x : int
  > (** The answer to everything *)
  > EOF

  $ cat > odoc << 'EOF'
  > #!/bin/bash
  > case "$1" in
  >   --version) 
  >     echo "2.0.0"
  >     ;;
  >   compile | compile-index) 
  >     # Find the -o flag and create the output file
  >     output=""
  >     while [[ $# -gt 0 ]]; do
  >       if [[ "$1" == "-o" && -n "$2" ]]; then
  >         output="$2"
  >         shift 2
  >       else
  >         shift
  >       fi
  >     done
  >     if [[ -n "$output" ]]; then
  >       mkdir -p $(dirname "$output")
  >       touch "$output"
  >     fi
  >     exit 0;;
  >   link)
  >     # Find the -o flag and create the output .odocl file
  >     output=""
  >     while [[ $# -gt 0 ]]; do
  >       if [[ "$1" == "-o" && -n "$2" ]]; then
  >         output="$2"
  >         shift 2
  >       else
  >         shift
  >       fi
  >     done
  >     if [[ -n "$output" ]]; then
  >       mkdir -p $(dirname "$output")
  >       touch "$output"
  >     fi
  >     exit 0;;
  >   html-generate) 
  >     # Find the -o flag and create output directory
  >     while [[ $# -gt 0 ]]; do
  >       if [[ "$1" == "-o" && -n "$2" ]]; then
  >         mkdir -p "$2"
  >         break
  >       fi
  >       shift
  >     done
  >     exit 0;;
  >   support-files) exit 0;;
  >   *) exit 0;;
  > esac
  > EOF
  $ chmod +x odoc
  $ PATH=.:$PATH dune build @doc-markdown
  $ ls _build/default/_doc/_markdown 2>/dev/null || echo "No markdown directory created (expected for odoc 2.0.0)"
  No markdown directory created (expected for odoc 2.0.0)

  $ cat > odoc << 'EOF'
  > #!/bin/bash
  > case "$1" in
  >   --version) 
  >     echo "3.1.0"
  >     ;;
  >   compile | compile-index) 
  >     # Find the -o flag and create the output file
  >     output=""
  >     while [[ $# -gt 0 ]]; do
  >       if [[ "$1" == "-o" && -n "$2" ]]; then
  >         output="$2"
  >         shift 2
  >       else
  >         shift
  >       fi
  >     done
  >     if [[ -n "$output" ]]; then
  >       mkdir -p $(dirname "$output")
  >       touch "$output"
  >     fi
  >     exit 0;;
  >   link)
  >     # Find the -o flag and create the output .odocl file
  >     output=""
  >     while [[ $# -gt 0 ]]; do
  >       if [[ "$1" == "-o" && -n "$2" ]]; then
  >         output="$2"
  >         shift 2
  >       else
  >         shift
  >       fi
  >     done
  >     if [[ -n "$output" ]]; then
  >       mkdir -p $(dirname "$output")
  >       touch "$output"
  >     fi
  >     exit 0;;
  >   html-generate) 
  >     # Find the -o flag and create output directory
  >     while [[ $# -gt 0 ]]; do
  >       if [[ "$1" == "-o" && -n "$2" ]]; then
  >         mkdir -p "$2"
  >         break
  >       fi
  >       shift
  >     done
  >     exit 0;;
  >   markdown-generate) 
  >     # Find the -o flag and create markdown files
  >     while [[ $# -gt 0 ]]; do
  >       if [[ "$1" == "-o" && -n "$2" ]]; then
  >         mkdir -p "$2"
  >         # Create some dummy markdown files to satisfy the build
  >         touch "$2/index.md"
  >         mkdir -p "$2/foo"
  >         touch "$2/foo/index.md"
  >         break
  >       fi
  >       shift
  >     done
  >     exit 0;;
  >   support-files) exit 0;;
  >   *) exit 0;;
  > esac
  > EOF
  $ chmod +x odoc

  $ PATH=.:$PATH dune build @doc-markdown
  $ ls _build/default/_doc/_markdown/foo/index.md 2>/dev/null && echo "Markdown files created (expected for odoc 3.1.0)"
  _build/default/_doc/_markdown/foo/index.md
  Markdown files created (expected for odoc 3.1.0)

  $ rm -rf _build
  $ cat > odoc << 'EOF'
  > #!/bin/bash
  > case "$1" in
  >   --version) 
  >     echo "not a valid version"
  >     ;;
  >   compile | compile-index) 
  >     # Find the -o flag and create the output file
  >     output=""
  >     while [[ $# -gt 0 ]]; do
  >       if [[ "$1" == "-o" && -n "$2" ]]; then
  >         output="$2"
  >         shift 2
  >       else
  >         shift
  >       fi
  >     done
  >     if [[ -n "$output" ]]; then
  >       mkdir -p $(dirname "$output")
  >       touch "$output"
  >     fi
  >     exit 0;;
  >   link)
  >     # Find the -o flag and create the output .odocl file
  >     output=""
  >     while [[ $# -gt 0 ]]; do
  >       if [[ "$1" == "-o" && -n "$2" ]]; then
  >         output="$2"
  >         shift 2
  >       else
  >         shift
  >       fi
  >     done
  >     if [[ -n "$output" ]]; then
  >       mkdir -p $(dirname "$output")
  >       touch "$output"
  >     fi
  >     exit 0;;
  >   html-generate) 
  >     # Find the -o flag and create output directory
  >     while [[ $# -gt 0 ]]; do
  >       if [[ "$1" == "-o" && -n "$2" ]]; then
  >         mkdir -p "$2"
  >         break
  >       fi
  >       shift
  >     done
  >     exit 0;;
  >   support-files) exit 0;;
  >   *) exit 0;;
  > esac
  > EOF
  $ chmod +x odoc

  $ PATH=.:$PATH dune build @doc-markdown
  $ ls _build/default/_doc/_markdown
  ls: _build/default/_doc/_markdown: No such file or directory
  [1]
