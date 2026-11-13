Public APIs backed by private modules work with a compiler without `-H`.

  $ make_melange_project 3.25 1.0
  $ cat > dune <<'EOF'
  > (library
  >  (name lib)
  >  (modules private public)
  >  (modes melange)
  >  (private_modules private))
  > (melange.emit
  >  (target output)
  >  (modules main)
  >  (emit_stdlib false)
  >  (libraries lib))
  > EOF
  $ echo 'let answer = 42' > private.ml
  $ echo 'let answer = Private.answer' > public.ml
  $ echo 'let answer = Lib.Public.answer' > main.ml

Wrap the compiler to reject hidden includes.

  $ mkdir _bin
  $ cat > _bin/melc <<EOF
  > #!$(command -v sh)
  > for arg do
  >   if [ "\$arg" = -H ]; then
  >     echo 'melc: unknown option -H' >&2
  >     exit 2
  >   fi
  > done
  > exec "$(command -v melc)" "\$@"
  > EOF
  $ chmod +x _bin/melc
  $ _bin/melc -H . --version
  melc: unknown option -H
  [2]

  $ PATH="$PWD/_bin:$PATH" dune build --sandbox copy --cache=disabled output/main.js
