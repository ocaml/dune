jq() {
  command jq -L"$INSIDE_DUNE"/test/blackbox-tests "$@"
}

jq_dune() {
  local args=()
  local filter_set=0

  while [ "$#" -gt 0 ]
  do
    case "$1" in
      -L|-f|--from-file)
        args+=("$1" "$2")
        shift 2
        ;;
      --arg|--argjson|--slurpfile|--rawfile|--argfile)
        args+=("$1" "$2" "$3")
        shift 3
        ;;
      --)
        args+=("$1")
        shift
        ;;
      -*)
        args+=("$1")
        shift
        ;;
      *)
        if [ "$filter_set" -eq 0 ]
        then
          args+=("include \"dune\"; $1")
          filter_set=1
        else
          args+=("$1")
        fi
        shift
        ;;
    esac
  done

  jq "${args[@]}"
}

is_linked() {
  nlinks=$(dune_cmd stat hardlinks "$1")
  [ "$nlinks" -gt 1 ] && echo linked || echo not linked
}

export XDG_CACHE_HOME="$PWD/.cache"

setup_xdg_runtime_dir () {
    export XDG_RUNTIME_DIR="${TMPDIR:-$PWD}/.xdg-runtime"
    mkdir -p "$XDG_RUNTIME_DIR"
    chmod 700 "$XDG_RUNTIME_DIR"
}

setup_basic_shared_cache_project () {
    local mode="$1"

    cat > config <<EOF
(lang dune 3.0)
(cache enabled)
EOF
    if [ "$mode" != "default" ]; then
        cat >> config <<EOF
(cache-storage-mode $mode)
EOF
    fi
    cat > dune-project <<EOF
(lang dune 3.5)
EOF
    cat > dune <<'EOF'
(rule
 (deps source)
 (targets target1 target2)
 (action
  (progn
   (no-infer (with-stdout-to beacon (echo "")))
   (with-stdout-to target1 (cat source))
   (with-stdout-to target2 (cat source source)))))
EOF
    cat > source <<'EOF'
\_o< COIN
EOF
}

init_oxcaml_project() {
  cat > "dune-project" <<- EOF
	(lang dune 3.20)
	(using oxcaml 0.1)
	EOF
}

make_dune_project() {
  version="$1"
  cat > dune-project <<- EOF
	(lang dune $version)
	EOF
}

make_mypkg_bin_project() {
  local message="${1:-hello from mybin}"
  cat > dune-project <<- EOF
	(lang dune 3.24)
	(package (name mypkg))
	EOF
  mkdir src
  cat > src/dune <<-'EOF'
	(executable (public_name mybin) (package mypkg))
	EOF
  cat > src/mybin.ml <<- EOF
	let () = print_endline "$message"
	EOF
}

make_mypkg_lib_project() {
  make_dune_project 3.24
  cat >> dune-project <<-'EOF'
	(package (name mypkg))
	EOF
  mkdir src
  cat > src/dune <<-'EOF'
	(library (public_name mypkg))
	EOF
  cat > src/mypkg.ml <<-'EOF'
	let x = 1
	EOF
}

make_mypkg_stubs_project() {
  make_dune_project 3.24
  cat >> dune-project <<-'EOF'
	(package (name mypkg))
	EOF
  mkdir src
  cat > src/dune <<-'EOF'
	(library
	 (public_name mypkg)
	 (foreign_stubs (language c) (names stub)))
	EOF
  cat > src/mypkg.ml <<-'EOF'
	let x = 1
	EOF
  cat > src/stub.c <<-'EOF'
	#include <caml/mlvalues.h>
	CAMLprim value mypkg_stub(value unit) { return Val_unit; }
	EOF
}

make_install_include_project() {
  make_dune_project 3.5
  cat >> dune-project <<-'EOF'
	(package (name hello))
	EOF
  cat > dune <<-'EOF'
	(executable
	 (public_name hello))
	
	(install
	 (files (include foo.sexp))
	 (section share))
	EOF
  cat > hello.ml <<-'EOF'
	let () = print_endline "Hello, World!"
	EOF
}

write_stdlib_mystdlib_sources() {
  mkdir -p stdlib
  cat > stdlib/other.ml <<-'EOF'
	let other () = Mystdlib.defined_in_stdlib
	EOF
  cat > stdlib/one_module.ml <<-'EOF'
	let foo = "foo"
	EOF
  cat > stdlib/mystdlib.ml <<-'EOF'
	let defined_in_stdlib = "defined"
	module One_module = One_module
	module Other = Other
	EOF
}

make_value_library() {
  local dir="$1"
  local name="$2"
  local value="$3"
  local value_name="${4:-value}"

  mkdir "$dir"
  cat > "$dir/dune" <<- EOF
	(library
	 (name ${name}))
	EOF
  cat > "$dir/${name}.ml" <<- EOF
	let ${value_name} = ${value}
	EOF
  cat > "$dir/${name}.mli" <<- EOF
	val ${value_name} : int
	EOF
}

make_mylib_consumer_executable() {
  cat > dune <<-'EOF'
	(executable
	 (name main)
	 (libraries mylib))
	EOF
  cat > uses_lib.ml <<-'EOF'
	let get_value () = Mylib.value
	EOF
  cat > uses_lib.mli <<-'EOF'
	val get_value : unit -> int
	EOF
  cat > no_use_lib.ml <<-'EOF'
	let compute x = x + 1
	EOF
  cat > no_use_lib.mli <<-'EOF'
	val compute : int -> int
	EOF
  cat > main.ml <<-'EOF'
	let () =
	  print_int (Uses_lib.get_value ());
	  print_int (No_use_lib.compute 5)
	EOF
}

write_original_name_xy() {
  cat > original_name.mli <<-'EOF'
	val x : string
	val y : int
	EOF
  cat > original_name.ml <<-'EOF'
	let x = "hello"
	let y = 42
	EOF
}

write_mylib_with_new_function() {
  cat > lib/mylib.mli <<-'EOF'
	val value : int
	val new_function : unit -> string
	EOF
  cat > lib/mylib.ml <<-'EOF'
	let value = 42
	let new_function () = "hello"
	EOF
}

make_melange_runtime_deps_lib() {
  local dir="${1:-lib}"

  mkdir -p "$dir/nested"
  echo "Some text" > "$dir/index.txt"
  echo "Some nested text" > "$dir/nested/hello.txt"
  cat > "$dir/dune" <<-'EOF'
	(library
	 (public_name foo)
	 (modes melange)
	 (preprocess (pps melange.ppx))
	 (melange.runtime_deps index.txt nested/hello.txt))
	EOF
  cat > "$dir/foo.ml" <<-'EOF'
	external readFileSync : string -> encoding:string -> string = "readFileSync"
	[@@mel.module "fs"]
	let dirname = [%mel.raw "__dirname"]
	let () = Js.log2 "dirname:" dirname
	let file_path = "./index.txt"
	let read_asset () = readFileSync (dirname ^ "/" ^ file_path) ~encoding:"utf8"
	EOF
}

make_old_melange_field_project() {
  local field="$1"
  local value="$2"

  mkdir -p old
  cat > old/dune-project <<-'EOF'
	(lang dune 3.23)
	(using melange 0.1)
	EOF
  cat > old/dune <<- EOF
	(library
	 (name old)
	 (modes melange)
	 (${field} ${value}))
	EOF
}

make_melange_runtime_deps_project() {
  local directory_targets="${1:-}"

  cat > dune-project <<- EOF
	(lang dune 3.8)
	EOF
  if [ -n "$directory_targets" ]; then
    cat >> dune-project <<-'EOF'
	(using directory-targets 0.1)
	EOF
  fi
  cat >> dune-project <<-'EOF'
	(using melange 0.1)
	EOF
  cat > dune <<-'EOF'
	(melange.emit
	 (target output)
	 (alias mel)
	 (libraries foo)
	 (emit_stdlib false)
	 (preprocess (pps melange.ppx))
	 (runtime_deps assets/file.txt))
	EOF
}

write_melange_private_map_library() {
  cat > lib/dune <<-'EOF'
	(library
	 (name foo)
	 (public_name repro.foo)
	 (wrapped true)
	 (libraries melange)
	 (modes melange))
	EOF
  cat > lib/foo.ml <<-'EOF'
	module Foo_map = Foo_map
	EOF
  cat > lib/foo_map.mli <<-'EOF'
	module Int = Foo_mapInt
	val size : Int.t -> int
	EOF
  cat > lib/foo_map.ml <<-'EOF'
	module Int = Foo_mapInt
	let size x = Int.size x
	EOF
  cat > lib/foo_mapInt.mli <<-'EOF'
	type t
	val size : t -> int
	EOF
  cat > lib/foo_mapInt.ml <<-'EOF'
	type t = Foo_internalMapInt.t
	let size x = Foo_internalMapInt.size x
	EOF
  cat > lib/foo_internalMapInt.mli <<-'EOF'
	type t
	val size : t -> int
	EOF
  cat > lib/foo_internalMapInt.ml <<-'EOF'
	type t = int array
	let size x = Foo_internalAVLtree.size x
	EOF
  cat > lib/foo_internalAVLtree.ml <<-'EOF'
	let size x = Array.length x
	EOF
}

write_melange_repro_foo_consumer() {
  cat > app/dune <<-'EOF'
	(melange.emit
	 (target dist)
	 (alias mel)
	 (emit_stdlib false)
	 (libraries repro.foo))
	EOF
  cat > app/main.ml <<-'EOF'
	let () = ignore (Foo.Foo_map.Int.size (Obj.magic 0))
	EOF
}

make_melange_foo_library_project() {
  local dir="$1"
  local package="$2"

  mkdir -p "$dir"
  cat > "$dir/dune-project" <<- EOF
	(lang dune 3.8)
	(package (name ${package}))
	(using melange 0.1)
	EOF
  cat > "$dir/dune"
  cat > "$dir/foo.ml" <<-'EOF'
	let x = "foo"
	EOF
}

make_melange_sandbox_project() {
  local runtime_deps="${1:-}"

  cat > dune-project <<-'EOF'
	(lang dune 3.22)
	(using melange 1.0)
	EOF
  {
    cat <<-'EOF'
	(library
	 (name lib)
	 (modes melange)
	 (modules lib))
	(melange.emit
	 (target output)
	 (alias mel)
	 (modules main)
	 (libraries lib)
	 (emit_stdlib false)
	EOF
    if [ -n "$runtime_deps" ]; then
      echo " (runtime_deps ${runtime_deps})"
    fi
    echo ")"
  } > dune
  cat > lib.ml <<-'EOF'
	let message = "hello from lib"
	EOF
  cat > main.ml <<-'EOF'
	let () = Js.log Lib.message
	EOF
}

write_melange_promote_app_dune() {
  local promote="$1"

  cat > app/dune <<- EOF
	(include_subdirs unqualified)
	(melange.emit
	 (alias dist)
	 (emit_stdlib false)
	 (promote ${promote})
	 (target dist))
	EOF
}

write_melange_asset_reader() {
  local dir="${1:-.}"

  mkdir -p "$dir/assets"
  cat > "$dir/assets/file.txt" <<-'EOF'
	hello from file
	EOF
  cat > "$dir/main.ml" <<-'EOF'
	external readFileSync : string -> encoding:string -> string = "readFileSync"
	[@@mel.module "fs"]
	let dirname = [%mel.raw "__dirname"]
	let file_path = "./assets/file.txt"
	let file_content = readFileSync (dirname ^ "/" ^ file_path) ~encoding:"utf8"
	let () = Js.log file_content
	let () = Js.log (Foo.read_asset ())
	EOF
}

write_menhir_unit_parser_sources() {
  cat >lang/ast.ml <<-'EOF'
	type expr =
	  | Unit
	EOF
  cat >lang/parser.mly <<-'EOF'
	%token EOF
	%start <Ast.expr> expr
	%%
	expr:
	| EOF { Ast.Unit }
	EOF
}

write_melange_dir_target_runtime_deps_lib() {
  local runtime_deps="$1"
  local file_path="$2"
  local dir="${3:-lib}"

  mkdir -p "$dir"
  cat > "$dir/dune" <<- EOF
	(rule (target (dir some_dir))
	 (action
	  (progn (system "mkdir %{target}")
	   (system "echo hello from file inside dir target > %{target}/inside-dir-target.txt"))))
	(library
	 (public_name foo)
	 (modes melange)
	 (preprocess (pps melange.ppx))
	 (melange.runtime_deps ${runtime_deps}))
	EOF
  cat > "$dir/foo.ml" <<- EOF
	external readFileSync : string -> encoding:string -> string = "readFileSync"
	[@@mel.module "fs"]
	let dirname = [%mel.raw "__dirname"]
	let () = Js.log2 "dirname:" dirname
	let file_path = "${file_path}"
	let read_asset () = readFileSync (dirname ^ "/" ^ file_path) ~encoding:"utf8"
	EOF
}

write_bin_pform_inline_tests_fixture() {
  local deps="${1:?}"

  cat > dump_path.ml <<- EOF
	let () =
	  let oc = open_out "$PWD/path.out" in
	  output_string oc (Sys.getenv "PATH");
	  close_out oc
	EOF
  cat > dune <<- EOF
	(library
	 (name check_backend)
	 (modules ())
	 (inline_tests.backend
	  (generate_runner (cat dump_path.ml))))
	(library
	 (name testlib)
	 (inline_tests
	  (backend check_backend)
	  (deps ${deps})))
	EOF
  : > testlib.ml
}

make_menhir_parser_using_dep() {
  mkdir -p parser
  cat > parser/dune <<-'EOF'
	(library
	 (name parser)
	 (libraries dep))
	(menhir (modules grammar))
	EOF
  cat > parser/grammar.mly <<-'EOF'
	%token EOF
	%start <int> main
	%%
	main: EOF { Dep.value }
	EOF
}

write_menhir_merge_into_sources() {
  cat >lang/sub/tokens.mly <<-'EOF'
	%token <char> TOKEN
	%token EOF
	%%
	EOF
  cat >lang/sub/parser.mly <<-'EOF'
	%start <char list> main
	%%
	main:
	| c = TOKEN EOF { [c] }
	| c = TOKEN xs = main  { c :: xs }
	EOF
  cat >lang/sub/dune <<-'EOF'
	(menhir (modules tokens parser) (merge_into both))
	EOF
}

make_melange_app_with_asset_reader() {
  local dir="${1:-app}"

  write_melange_asset_reader "$dir"
  cat > "$dir/dune-project" <<-'EOF'
	(lang dune 3.8)
	(package (name app))
	(using melange 0.1)
	EOF
  cat > "$dir/dune" <<-'EOF'
	(melange.emit
	 (target output)
	 (alias mel)
	 (emit_stdlib false)
	 (libraries foo)
	 (preprocess (pps melange.ppx)))
	EOF
}

make_melange_virtual_time_project() {
  local vlib_public_name="$1"
  local impl_public_name="$2"
  local emit_impl_name="$3"

  mkdir -p vlib js_impl test
  {
    echo "(library"
    echo " (name the_lib)"
    echo " (modes melange native)"
    if [ -n "$vlib_public_name" ]; then
      echo " (public_name ${vlib_public_name})"
    fi
    echo " (virtual_modules virt))"
  } > vlib/dune
  cat > vlib/the_lib.mli <<-'EOF'
	module Time : sig
	  val gettimeofday : unit -> float
	end
	EOF
  cat > vlib/the_lib.ml <<-'EOF'
	module Time = struct
	  let gettimeofday () = Virt.gettimeofday ()
	end
	EOF
  cat > vlib/virt.mli <<-'EOF'
	val gettimeofday : unit -> float
	EOF

  {
    echo "(library"
    echo " (name timeJs)"
    if [ -n "$impl_public_name" ]; then
      echo " (public_name ${impl_public_name})"
    fi
    echo " (implements the_lib)"
    echo " (modes melange))"
  } > js_impl/dune
  cat > js_impl/virt.ml <<-'EOF'
	let gettimeofday : unit -> float = fun () -> 42.
	EOF

  cat > test/dune <<- EOF
	(melange.emit
	 (target output)
	 (libraries the_lib ${emit_impl_name})
	 (emit_stdlib false))
	EOF
}

make_private_module_virtual_lib_fixture() {
  local modes="${1:-}"

  mkdir -p vlib impl
  {
    echo "(library"
    echo " (name vlib)"
    if [ -n "$modes" ]; then
      echo " ${modes}"
    fi
    echo " (public_name pkg.vlib)"
    echo " (virtual_modules virt)"
    echo " (private_modules helper))"
  } > vlib/dune
  cat > vlib/vlib.ml <<-'EOF'
	let run () = Virt.run () + Helper.value
	EOF
  cat > vlib/vlib.mli <<-'EOF'
	val run : unit -> int
	EOF
  cat > vlib/virt.mli <<-'EOF'
	val run : unit -> int
	EOF
  cat > vlib/helper.ml <<-'EOF'
	let value = 42
	EOF

  {
    echo "(library"
    echo " (name impl)"
    if [ -n "$modes" ]; then
      echo " ${modes}"
    fi
    echo " (public_name pkg.impl)"
    echo " (implements pkg.vlib))"
  } > impl/dune
  cat > impl/virt.ml <<-'EOF'
	let run () = 1
	EOF
}

make_noop_ppx_rewriter() {
  mkdir ppx
  cat > ppx/dune <<-'EOF'
	(library
	 (name ppx_noop)
	 (kind ppx_rewriter)
	 (ppx.driver (main Ppx_noop.main)))
	EOF
  cat > ppx/ppx_noop.ml <<-'EOF'
	let main () =
	  let n = Array.length Sys.argv in
	  if n < 4 || Sys.argv.(1) <> "--as-ppx" then assert false;
	  let input = Sys.argv.(n - 2) in
	  let output = Sys.argv.(n - 1) in
	  Filename.quote_command "cp" [input; output]
	  |> Sys.command
	  |> exit
	EOF
}

make_hello_ppx_runtime_fixture() {
  local mode="${1:-byte}"

  mkdir hello
  if [ "$mode" = "melange" ]; then
    cat > hello/dune <<-'EOF'
	(library (name hello) (modes melange))
	EOF
  else
    cat > hello/dune <<-'EOF'
	(library (name hello))
	EOF
  fi
  cat > hello/hello.ml <<-'EOF'
	type t = int
	EOF

  mkdir hello_ppx
  cat > hello_ppx/dune <<-'EOF'
	(library
	 (name hello_ppx)
	 (kind ppx_rewriter)
	 (ppx_runtime_libraries hello)
	 (ppx.driver (main Hello_ppx.main)))
	EOF
  cat > hello_ppx/hello_ppx.ml <<-'EOF'
	let main () =
	  let out = ref "" in
	  let args =
	    [ ("-o", Arg.Set_string out, "")
	    ; ("--impl", Arg.Set_string (ref ""), "")
	    ; ("--as-ppx", Arg.Set (ref false), "")
	    ; ("--cookie", Arg.Set (ref false), "")
	EOF
  if [ "$mode" = "melange" ]; then
    cat >> hello_ppx/hello_ppx.ml <<-'EOF'
	    ; ("-loc-filename", Arg.String ignore, "")
	EOF
  fi
  cat >> hello_ppx/hello_ppx.ml <<-'EOF'
	    ]
	  in
	  let anon _ = () in
	  Arg.parse (Arg.align args) anon "";
	  close_out (open_out !out)
	EOF
}

make_foreign_header_consumer() {
  make_dune_project 3.8
  cat > dune <<-'EOF'
	(executable
	 (name bar)
	 (foreign_stubs
	  (language c)
	  (include_dirs (lib mypkg))
	  (names foo)))
	EOF
  touch bar.ml
  cat > foo.c
}

make_trivial_ocamllex() {
  cat > "$1" <<-'EOF'
	{
	}
	rule lex = parse
	  | _   { true  }
	  | eof { false }
	EOF
}

make_foreign_stubs_include_dirs_project() {
  cat > dune <<-'EOF'
	(library
	 (name foo)
	 (foreign_stubs
	  (language c)
	  (names bar)
	  (include_dirs (include foo))))
	EOF
}

make_directory_targets_project() {
  local version="${1:-3.23}"
  cat > dune-project <<- EOF
	(lang dune ${version})
	(using directory-targets 0.1)
	EOF
}

make_empty_child_directory_target_project() {
  make_directory_targets_project 3.10
  cat > dune <<-'EOF'
	(rule
	 (target (dir output))
	 (action
	  (progn
	   (run mkdir output)
	   (run mkdir output/child)
	   (run touch output/file))))
	EOF
}

write_directory_target_contents_rules() {
  local deps="$1"

  cat > dune <<- EOF
	(rule
	  (deps ${deps})
	  (targets (dir output))
	  (action (bash "\| echo running;
	                "\| mkdir -p output/subdir;
	                "\| cat src_a > output/new-a;
	                "\| cat src_b > output/subdir/b
	 )))
	(rule
	  (deps output)
	  (target contents)
	  (action (bash "echo running; echo 'new-a:' > contents; cat output/new-a >> contents; echo 'b:' >> contents; cat output/subdir/b >> contents")))
	EOF
}

write_directory_diff_keep_rule() {
  cat > dune <<-'EOF'
	(rule
	 (targets (dir actual))
	 (action (system "mkdir -p actual && printf 'keep\n' > actual/keep")))
	
	(rule
	 (alias runtest)
	 (action (diff expected actual)))
	EOF
}

write_cross_compilation_repro_project() {
  local dir="${1:-.}"
  local modules="${2:-}"

  mkdir -p "$dir"
  cat > "$dir/dune-project" <<-'EOF'
	(lang dune 3.7)
	(package (name repro))
	EOF
  cat > "$dir/dune" <<- EOF
	(executable
	 (name gen)
	 (modules gen)
	 (enabled_if
	  (= %{context_name} "default"))
	 (libraries libdep))
	(rule
	 (with-stdout-to
	  gen.ml
	  (echo "let () = Format.printf \"let x = 1\"")))
	(library
	 (name repro)
	 (public_name repro)
	 (modules${modules:+ ${modules}}))
	EOF
  if [ -n "$modules" ]; then
    cat >> "$dir/dune" <<-'EOF'
	(rule
	 (with-stdout-to
	  foo.ml
	  (run ./gen.exe)))
	EOF
  fi
}

make_dune_cache_copy_project() {
  make_dune_project 2.1
  cat > dune <<-'EOF'
	(rule
	  (deps source)
	  (targets target)
	  (action (copy source target)))
	EOF
  cat > source <<-'EOF'
	\_o< COIN
	EOF
}

make_two_context_workspace() {
  local version="${1:-3.13}"
  local name="${2:-alt-context}"

  cat > dune-workspace <<- EOF
	(lang dune ${version})
	
	(context default)
	
	(context
	 (default
	  (name ${name})))
	EOF
}

make_promotion_test_project() {
  make_dune_project 2.0
  cat > dune <<-'EOF'
	(rule
	 (alias runtest)
	 (action
	  (diff a.expected a.actual)))
	
	(rule
	 (with-stdout-to a.actual
	  (echo "A actual\n")))
	
	(rule
	 (alias runtest)
	 (action
	  (progn
	   (with-stdout-to b.actual
	    (echo "B actual\n"))
	   (diff? b.expected b.actual))))
	EOF
  if [ "${1:-}" = "with-c" ]; then
    cat >> dune <<-'EOF'
	
	(rule
	 (with-stdout-to c.actual
	  (echo "C actual\n")))
	
	(rule
	 (alias runtest)
	 (action
	  (diff? c.expected c.actual)))
	EOF
  fi
}

make_bar_baz_packages_project() {
  cat > dune-project <<-'EOF'
	(lang dune 3.13)
	(package (name bar) (allow_empty))
	(package (name baz) (allow_empty))
	EOF
}

write_duplicate_foo_public_libraries() {
  cat > dune <<-'EOF'
	(library
	 (name foo)
	 (public_name bar.foo))
	(library
	 (name foo)
	 (public_name baz.foo))
	EOF
}

write_target_promotion_rules() {
  local mode="$1"

  cat > dune <<- EOF
	(rule
	 (mode ${mode})
	 (deps original)
	 (target promoted)
	 (action (copy %{deps} %{target})))
	(rule
	 (deps promoted)
	 (target result)
	 (action (system "cat promoted promoted > result")))
	EOF
}

write_sites_plugin_app_dune() {
  local version="$1"
  local executable_package="${2:-}"

  cat > dune-project <<- EOF
	(lang dune ${version})
	(using dune_site 0.1)
	(name app)
	
	(package
	 (name app)
	 (sites (lib plugins)))
	EOF
  {
    echo "(executable"
    echo " (public_name app)"
    if [ -n "$executable_package" ]; then
      echo " ${executable_package}"
    fi
    cat <<-'EOF'
	 (modules sites app)
	 (libraries app.register dune-site dune-site.plugins))
	
	(library
	 (public_name app.register)
	 (name registration)
	 (modules registration))
	
	(generate_sites_module
	 (module sites)
	 (plugins (app plugins)))
	EOF
  } > dune
}

write_sites_plugin_app_sources() {
  cat > registration.ml <<-'EOF'
	let todo : (unit -> unit) Queue.t = Queue.create ()
	EOF
  cat > app.ml <<-'EOF'
	(* load all the available plugins *)
	let () = Sites.Plugins.Plugins.load_all ()
	
	let () = print_endline "Main app starts..."
	(* Execute the code registered by the plugins *)
	let () = Queue.iter (fun f -> f ()) Registration.todo
	EOF
}

write_sites_plugin_impl() {
  cat > plugin/plugin1_impl.ml <<-'EOF'
	let () =
	print_endline "Registration of Plugin1";
	Queue.add (fun () -> print_endline "Plugin1 is doing something...") Registration.todo
	EOF
}

write_sites_plugin_dune() {
  cat > plugin/dune <<-'EOF'
	(library
	 (public_name plugin1.plugin1_impl)
	 (name plugin1_impl)
	 (modules plugin1_impl)
	 (libraries app.register))
	
	(plugin
	 (name plugin1)
	 (libraries plugin1.plugin1_impl)
	 (site (app plugins)))
	EOF
}

make_toplevel_plugin_host() {
  local loader="${1:-}"

  cat > dune-project <<-'EOF'
	(lang dune 3.7)
	(using dune_site 0.1)
	(name top_with_plugins)
	(wrapped_executables false)
	(map_workspace_root false)
	
	(package
	 (name top_with_plugins)
	 (sites (lib top_plugins)))
	EOF

  cat > dune <<-'EOF'
	(executable
	 (public_name top_with_plugins)
	 (name top_with_plugins)
	 (modes byte)
	 (flags :standard -safe-string)
	 (modules sites top_with_plugins)
	 (link_flags (-linkall))
	 (libraries compiler-libs.toplevel
	  top_with_plugins.register dune-site dune-site.plugins
	EOF
  if [ -n "$loader" ]; then
    printf '\t  %s\n' "$loader" >> dune
  fi
  cat >> dune <<-'EOF'
	))
	
	(library
	 (public_name top_with_plugins.register)
	 (modes byte)
	 (name registration)
	 (modules registration))
	
	(generate_sites_module
	 (module sites)
	 (plugins (top_with_plugins top_plugins)))
	EOF
}

write_toplevel_plugin_sources() {
  cat > top_with_plugins.ml <<-'EOF'
	let main () =
	  print_endline "\nMain app really starts...";
	  (* load all the available plugins *)
	  Sites.Plugins.Top_plugins.load_all ();
	  print_endline "Main app after loading plugins...";
	  (* Execute the code registered by the plugins *)
	  print_endline "Main app executing registered plugins...";
	  Queue.iter (fun f -> f ()) Registration.todo;
	  print_endline "Main app after executing registered plugins...";
	  exit (Topmain.main ())
	
	let () =
	    main()
	EOF

  cat > registration.ml <<-'EOF'
	let todo : (unit -> unit) Queue.t = Queue.create ()
	let register f =
	  print_endline "In register";
	  Queue.add f todo;
	  print_endline "Done in register";
	EOF
}

make_toplevel_plugin() {
  local n="$1"
  local dir="plugin${n}"
  local impl="plugin${n}_impl"

  mkdir "$dir"
  cat > "$dir/dune-project" <<- EOF
	(lang dune 3.7)
	(using dune_site 0.1)
	
	(generate_opam_files true)
	(wrapped_executables false)
	(map_workspace_root false)
	(package
	 (name top-plugin${n}))
	EOF

  cat > "$dir/dune" <<- EOF
	(library
	 (public_name top-plugin${n}.${impl})
	 (modes byte)
	 (name ${impl})
	 (modules ${impl})
	 (libraries top_with_plugins.register))
	
	(plugin
	 (name plugin${n})
	 (libraries top-plugin${n}.${impl})
	 (site (top_with_plugins top_plugins)))
	EOF

  cat > "$dir/${impl}.ml" <<- EOF
	let myfun () =
	  print_endline "Plugin${n} is doing something..."
	
	let () =
	  print_endline "Registration of Plugin${n}";
	  Registration.register myfun;
	  print_endline "Done with registration of Plugin${n}";
	EOF
}

make_simple_rpc_watch_project() {
  make_dune_project 3.23
  cat > dune <<-'EOF'
	(rule
	 (target x)
	 (action (write-file %{target} ok)))
	EOF
}

odoc_detect_syntax() {
  if grep -q '>sig<' "$1"
  then
    echo it is ocaml
  elif grep -q '{ ... }' "$1"
  then
    echo it is reason
  else
    echo it is unknown
  fi
}

query_ocaml_merlin() {
  file="$1"
  shift
  query=$(mktemp "${TMPDIR:-.}/merlin-query.XXXXXX")
  printf '(File "%s")\n' "$file" | dune internal sexp-to-csexp > "$query"
  dune ocaml-merlin "$@" < "$query"
  rm -f "$query"
}

query_ocaml_merlin_pp() {
  output=$(mktemp "${TMPDIR:-.}/merlin-output.XXXXXX")
  query_ocaml_merlin "$@" > "$output"
  dune internal sexp-pp --format=csexp "$output"
  rm -f "$output"
}

query_ocaml_merlin_sexp() {
  output=$(mktemp "${TMPDIR:-.}/merlin-output.XXXXXX")
  query_ocaml_merlin "$@" > "$output"
  dune internal sexp-pp --format=csexp --compact "$output"
  rm -f "$output"
}

make_dir_with_dune() {
  path="$1"
  mkdir -p "$path"
  cat > "$path/dune"
}

make_dummy_intf() {
  dir="$1"
  name="$2"
  cat >> "$dir/$name.mli" <<- EOF
	type t
	val f : t -> unit
	EOF
}

make_dummy_impl() {
  dir="$1"
  name="$2"
  cat >> "$dir/$name.ml" <<- EOF
	type t = int
	let f _ = ()
	EOF
}

make_lib_impl() {
  name="$1"
  implements="$2"
  make_dir_with_dune "$name" <<- EOF
	(library
	  (name $name)
	  (implements $implements))
	EOF
}

target_cmi() {
  echo "./$1/.$1.objs/byte/$1.cmi"
}

build_target_cmi() {
  echo "./_build/default/$1/.$1.objs/byte/$1.cmi"
}

# Git related helper scripts

# These variables are used by Git and set here so the name and email of the
# committer stay the same between different runs
export GIT_AUTHOR_NAME="Test Name"
export GIT_AUTHOR_EMAIL="test@example.com"
export GIT_COMMITTER_NAME="${GIT_AUTHOR_NAME}"
export GIT_COMMITTER_EMAIL="${GIT_AUTHOR_EMAIL}"
# Set various GIT variables to ensure git behaves as "default" as possible in the tests
export GIT_CONFIG_GLOBAL="/dev/null"
export GIT_CONFIG_SYSTEM="/dev/null"

export DUNE_RUNNING=0

# Called from cram tests with optional dune build arguments.
# shellcheck disable=SC2120
start_dune () {
    ( (dune build "$@" --passive-watch-mode > .#dune-output 2>&1) || (echo exit $? >> .#dune-output) ) &
    DUNE_PID=$!;
    export DUNE_RUNNING=1;
    wait_for_rpc_server
}

timeout="$(command -v timeout || echo gtimeout)"

with_timeout () {
    $timeout 2 "$@"
    exit_code=$?
    if [ "$exit_code" = 124 ]
    then
        echo Timed out
        cat .#dune-output
    else
        return "$exit_code"
    fi
}

with_timeout_quiet () {
    output=$(mktemp)
    $timeout 2 "$@" >"$output" 2>&1
    exit_code=$?
    if [ "$exit_code" = 124 ]
    then
        echo Timed out
        cat "$output"
        cat .#dune-output
    elif [ "$exit_code" != 0 ]
    then
        cat "$output"
    fi
    rm -f "$output"
    return "$exit_code"
}

shutdown_dune () {
    with_timeout dune shutdown
}

shutdown_dune_quiet () {
    with_timeout_quiet dune shutdown
}

wait_for_dune_exit () {
    # On Linux, we may run into a bash pid aliasing bug that causes wait to
    # reject the pid. Therefore we use tail to wait instead.
    if [ "$(uname -s)" = "Linux" ]
    then
        # wait for all child processes
        tail --pid="$DUNE_PID" -f /dev/null;
    else
        # wait for dune to exit
        wait "$DUNE_PID";
    fi
}

wait_for_pid_to_exit_with_timeout () {
    pid=$1
    iterations=$2
    while kill -0 "$pid" 2>/dev/null
    do
        if [ "$iterations" = 0 ]
        then
            return 124
        fi
        iterations=$((iterations - 1))
        sleep 0.01
    done
}

stop_dune () {
    shutdown_dune;
    wait_for_dune_exit;
    cat .#dune-output;
}

build () {
    with_timeout dune rpc build --wait "$@"
}

make_fs_memo_project () {
    make_dune_project 2.0
    cat >dune <<-'EOF'
	(rule
	 (alias default)
	 (deps dep
	  (glob_files file-?)
	  (glob_files dir/file-?)
	  (glob_files dir/subdir/file-?))
	 (target result)
	 (action (system "\| echo Executing rule...
	               "\| echo %{deps}       |
	               "\|   tr ' ' '\n'      |
	               "\|   xargs -n 1       |
	               "\|   grep -v dep      |
	               "\|   xargs cat > result
	)))
	EOF
}

fs_memo_test () {
    local action="$1"
    local before between after

    echo "------------------------------------------"
    before=$(cat _build/default/result 2>/dev/null)
    # No initial targets: start the passive watch server only.
    # shellcheck disable=SC2119
    start_dune
    build . | grep -v Success
    between=$(cat _build/default/result)
    bash -c "$action"
    build . | grep -v Success
    stop_dune >> .#tmp
    after=$(cat _build/default/result)
    cat .#tmp
    echo "------------------------------------------"
    echo "result = '$before' -> '$between' -> '$after'"
    echo "------------------------------------------"
    dune trace cat | jq -c 'select(.name == "fs_update") | .args' | sort
    rm .#tmp
}

stop_dune_quiet () {
    shutdown_dune_quiet;
    wait_for_dune_exit_with_timeout;
}

build_quiet () {
    with_timeout_quiet dune rpc build --wait "$@"
}

wait_for_rpc_server () {
    with_timeout_quiet dune rpc ping --wait
}

summarize_rpc_trace () {
    dune trace cat | jq -r '
      select(.cat == "rpc")
      | if .name == "accept"
           and .args.stage == "stop"
           and .args.status == "close"
        then "accept stop close"
        elif .name == "accept"
             and .args.stage == "stop"
             and (.args | has("error"))
        then "accept stop error"
        elif .name == "startup-failure"
        then "startup failure"
        elif .name == "request" and .args.meth == "build"
        then "build \(.args.stage)"
        elif .name == "shutdown"
        then "shutdown \(.args.stage)"
        else empty
        end'
}

trace_rocq_flags () {
    local target="${1}"

    dune clean
    dune build "${target}"
    dune trace cat | jq_dune -c 'rocqFlags'
}

wait_for_dune_exit_with_timeout () {
    exit_code=0
    wait_for_pid_to_exit_with_timeout "$DUNE_PID" 200 || exit_code=$?
    if [ "$exit_code" = 124 ]
    then
        summarize_rpc_trace
    fi
    return "$exit_code"
}

wait_for_file () {
    until [ -e "$1" ]
    do
        sleep 0.01
    done
}

file_status() {
  [ -e "$1" ] && echo "$1 exists" || echo "$1 missing"
}

censor() {
  # Strip $PWD first so that sandbox hashes in the cram test's own path
  # aren't caught as digests. Each unique remaining digest gets a distinct
  # label ($DIGEST when unique, $DIGEST1/$DIGEST2/etc. when multiple).
  # shellcheck disable=SC2016
  dune_cmd subst "$PWD" '$PWD' \
    | dune_cmd subst-unique '[0-9a-f]{32}' '$DIGEST' \
    | dune_cmd subst-unique '\.cinaps\.[0-9a-f]{8}' '.cinaps.$CINAPS'
}

# Print PATH-like entries in $1 that aren't in $2, preserving their
# order in $1. Assumes ":" as the separator.
env_added() {
  tr ':' '\n' <<< "$1" | grep -vFxf <(tr ':' '\n' <<< "$2")
}
