Test simple interactions between melange.emit and copy_files

  $ mkdir a
  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > (using directory-targets 0.1)
  > EOF

  $ cat > a/dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (target output)
  >  (runtime_deps assets/file.txt assets/file.txt))
  > EOF

  $ mkdir a/assets
  $ cat > a/assets/file.txt <<EOF
  > hello from file
  > EOF

  $ cat > a/main.ml <<EOF
  > let dirname = [%bs.raw "__dirname"]
  > let file_path = "./assets/file.txt"
  > let file_content = Node.Fs.readFileSync (dirname ^ "/" ^ file_path) \`utf8
  > let () = Js.log file_content
  > EOF

Rules created for the assets in the output directory

  $ mkdir a/output
  $ dune rules @mel | grep file.txt
  ((deps ((File (In_build_dir _build/default/a/assets/file.txt))))
   (targets ((files (default/a/output/a/assets/file.txt)) (directories ())))
     (symlink ../../../assets/file.txt a/output/a/assets/file.txt))))

  $ dune build @mel --display=short
          melc a/.output.mobjs/melange/melange__Main.{cmi,cmj,cmt}
          melc a/output/a/main.js

The runtime_dep index.txt was copied to the build folder

  $ ls _build/default/a/output/a/assets
  file.txt

  $ dune build a/output/a/assets/file.txt --display=short
  $ ls _build/default/a/output/a
  assets
  main.js


  $ node _build/default/a/output/a/main.js
  hello from file
  




Test depending on non-existing paths

  $ mkdir another
  $ dune clean
  $ cat > another/dune <<EOF
  > (melange.emit
  >  (alias non-existing-mel)
  >  (target another-output)
  >  (runtime_deps doesnt-exist.txt))
  > EOF

  $ dune build @non-existing-mel --display=short
  Error: No rule found for another/doesnt-exist.txt
  -> required by alias another/non-existing-mel
  [1]

Test depending on paths that "escape" the melange.emit directory

  $ dune clean
  $ cat > another/dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (target another-output)
  >  (runtime_deps ../a/assets/file.txt))
  > EOF
  $ cat > another/main.ml <<EOF
  > let dirname = [%bs.raw "__dirname"]
  > let file_path = "./assets/file.txt"
  > let file_content = Node.Fs.readFileSync (dirname ^ "/" ^ file_path) \`utf8
  > let () = Js.log file_content
  > EOF

Need to create the source dir first for the alias to be picked up

  $ mkdir -p another/another-output/assets
  $ dune rules @mel | grep .txt
  ((deps ((File (In_build_dir _build/default/a/assets/file.txt))))
   (targets ((files (default/a/output/a/assets/file.txt)) (directories ())))
     (symlink ../../../assets/file.txt a/output/a/assets/file.txt))))
  ((deps ((File (In_build_dir _build/default/a/assets/file.txt))))
    ((files (default/another/another-output/a/assets/file.txt))
      ../../../../a/assets/file.txt
      another/another-output/a/assets/file.txt))))

  $ dune build @mel --display=short
          melc a/.output.mobjs/melange/melange__Main.{cmi,cmj,cmt}
          melc another/.another-output.mobjs/melange/melange__Main.{cmi,cmj,cmt}
          melc a/output/a/main.js
          melc another/another-output/another/main.js

Path ends ups being emitted "correctly", but outside the target dir.
  $ ls _build/default/another/another-output/another
  main.js
  $ ls _build/default/another/another-output/a/assets
  file.txt

Test depending on external paths

  $ dune clean
  $ mkdir external
  $ cat > external/dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (target external-output)
  >  (runtime_deps /etc/hosts))
  > EOF
  $ cat > external/main.ml <<EOF
  > let dirname = [%bs.raw "__dirname"]
  > let file_path = "./assets/file.txt"
  > let file_content = Node.Fs.readFileSync (dirname ^ "/" ^ file_path) \`utf8
  > let () = Js.log file_content
  > EOF

  $ dune build @mel --display=short
          melc a/.output.mobjs/melange/melange__Main.{cmi,cmj,cmt}
          melc another/.another-output.mobjs/melange/melange__Main.{cmi,cmj,cmt}
          melc a/output/a/main.js
          melc another/another-output/another/main.js
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("[as_in_build_dir_exn] called on something not in build dir",
    { t = External "/etc/hosts" })
  Raised at Stdune__Code_error.raise in file
    "otherlibs/stdune/src/code_error.ml", line 11, characters 30-62
  Called from Dune_rules__Melange_rules.Runtime_deps.targets.(fun) in file
    "src/dune_rules/melange/melange_rules.ml", line 269, characters 14-45
  Called from Stdune__Set.Make.to_list_map.(fun) in file
    "otherlibs/stdune/src/set.ml", line 16, characters 37-40
  Called from Stdlib__Set.Make.fold in file "set.ml", line 383, characters
    34-55
  Called from Stdune__Set.Make.to_list_map in file
    "otherlibs/stdune/src/set.ml", line 16, characters 4-48
  Called from Dune_rules__Melange_rules.setup_runtime_assets_rules in file
    "src/dune_rules/melange/melange_rules.ml", line 286, characters 16-62
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/src/scheduler.ml",
    line 73, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/src/scheduler.ml",
    line 73, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/src/scheduler.ml",
    line 73, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/src/scheduler.ml",
    line 73, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/src/scheduler.ml",
    line 73, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/src/scheduler.ml",
    line 73, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/src/scheduler.ml",
    line 73, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/src/scheduler.ml",
    line 73, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/src/scheduler.ml",
    line 73, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 36, characters 27-56
  Called from Fiber__Scheduler.exec in file "otherlibs/fiber/src/scheduler.ml",
    line 73, characters 8-11
  -> required by ("<unnamed>", ())
  -> required by ("<unnamed>", ())
  -> required by ("<unnamed>", ())
  -> required by
     ("load-dir", In_build_dir "default/external/external-output/external")
  -> required by
     ("build-file",
     In_build_dir "default/external/external-output/external/main.js")
  -> required by ("<unnamed>", ())
  -> required by
     ("build-alias", { dir = In_build_dir "default/external"; name = "mel" })
  -> required by ("toplevel", ())
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]


