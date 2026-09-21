MSVC x86-64 assembly can be used in stubs and foreign archives.

  $ make_dune_project 3.26
  $ cat > answer.asm <<'EOF'
  > option casemap:none
  > .code
  > PUBLIC asm_answer
  > asm_answer PROC
  >   mov rax, ASM_ANSWER
  >   ret
  > asm_answer ENDP
  > END
  > EOF
  $ cat > main.ml <<'EOF'
  > external answer : unit -> int = "asm_answer"
  > let () = Printf.printf "%d\n" (answer ())
  > EOF

The assembler returns the tagged OCaml representation of 42.

  $ cat > dune <<'EOF'
  > (executable
  >  (name main)
  >  (foreign_stubs
  >   (language asm)
  >   (names answer)
  >   (flags /DASM_ANSWER=85)))
  > EOF
  $ dune exec ./main.exe
  42

The same assembly source also works through a foreign library.

  $ cat > dune <<'EOF'
  > (foreign_library
  >  (archive_name answer)
  >  (language asm)
  >  (names answer)
  >  (flags /DASM_ANSWER=85))
  > (executable
  >  (name main)
  >  (foreign_archives answer))
  > EOF
  $ dune exec ./main.exe
  42
