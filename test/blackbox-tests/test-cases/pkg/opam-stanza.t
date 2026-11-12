The unreleased opam entrance builds prepared sources in an opaque sandbox.
Its install tree is shared by consumers with different package sets.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name opaque) (allow_empty))
  > (package (name native) (allow_empty))
  > EOF
  $ mkdir -p opaque/src/nested native
  $ echo '(not-a-dune-stanza)' > opaque/src/nested/dune
  $ echo '(' > opaque/src/nested/dune-project
  $ echo prepared > opaque/input
  $ cat > opaque/recipe.sh <<'EOF'
  > #!/bin/sh
  > set -eu
  > test -w input
  > cat input > result
  > echo modified > input
  > printf 'share: [ "result" ]\n' > opaque.install
  > printf 'opam-version: "2.0"\nvariables { answer: "42" version: "exposed" dev: false }\n' > opaque.config
  > EOF
  $ chmod +x opaque/recipe.sh
  $ cat > opaque/dune <<'EOF'
  > (opam
  >  (package opaque)
  >  (dir src)
  >  (version 1.2.3)
  >  (dev true)
  >  (source ..)
  >  (build
  >   (progn
  >    (run sh -c "test '%{pkg-self:version}' = 1.2.3")
  >    (run sh -c "test '%{pkg-self:dev}' = true")
  >    (run ./recipe.sh)))
  >  (install
  >   (run sh -c "printf '#!/bin/sh\necho opaque-tool\n' > '%{bin}/opaque-tool'; chmod +x '%{bin}/opaque-tool'"))
  >  (exported_env (= OPAQUE_DATA %{pkg-self:share}/result)))
  > EOF
  $ cat > opaque/src/dune <<'EOF'
  > (library (public_name native))
  > (include missing-file)
  > (rule (target forbidden) (action (run false)))
  > EOF
  $ echo native > native/data
  $ cat > native/dune <<'EOF'
  > (install (package native) (section share) (files data))
  > EOF
  $ cat > dune <<'EOF'
  > (subdir opaque
  >  (rule
  >   (target parent-forbidden)
  >   (action (write-file %{target} forbidden))))
  > (rule
  >  (target result)
  >  (deps (package opaque))
  >  (action
  >   (with-stdout-to %{target}
  >    (progn
  >     (cat %{pkg:opaque:share:result})
  >     (echo "%{pkg:opaque:answer}\n")
  >     (echo "%{pkg:opaque:version} %{pkg:opaque:dev}\n")
  >     (run %{bin:opaque-tool})
  >     (system "opaque-tool")
  >     (system "cat \"$OPAQUE_DATA\"")))))
  > (rule
  >  (target mixed)
  >  (deps (package native) (package opaque))
  >  (action
  >   (with-stdout-to %{target}
  >    (cat %{pkg:native:share:data} %{pkg:opaque:share:result}))))
  > (rule
  >  (target package-dep)
  >  (deps (package opaque))
  >  (action
  >   (with-stdout-to %{target}
  >    (system "cat \"$OPAQUE_DATA\""))))
  > EOF
  $ dune build result package-dep
  $ recipe_runs() {
  >   dune trace cat | jq_dune -s '
  >     [.[] | processes | select(.args.prog | endswith("recipe.sh"))] | length'
  > }
  $ recipe_runs
  1
  $ cp _build/_private/default/.opam/opaque-*/target/cookie cookie.before
  $ dune build mixed
  $ recipe_runs
  0
  $ cat _build/default/result
  prepared
  42
  exposed false
  opaque-tool
  opaque-tool
  prepared
  $ cat _build/default/mixed
  native
  prepared
  $ cat _build/default/package-dep
  prepared
  $ cat opaque/input
  prepared

Changing a prepared source invalidates all consumers even though the installed
file names and configuration, and thus the cookie contents, do not change. The
package-dep consumer has no installed-file pform and reaches the output only
through the exported environment, so its explicit package dependency must track
the installed file.

  $ echo updated > opaque/input
  $ dune build result mixed package-dep
  $ recipe_runs
  1
  $ cmp cookie.before _build/_private/default/.opam/opaque-*/target/cookie
  $ cat _build/default/mixed
  native
  updated
  $ cat _build/default/package-dep
  updated
  $ cat opaque/input
  updated

The opaque child's own configuration is ignored. Explicit parent declarations
remain active, as with data_only_dirs.

  $ dune build opaque/src/forbidden
  Error: Don't know how to build opaque/src/forbidden
  [1]
  $ dune build opaque/parent-forbidden
  $ cat _build/default/opaque/parent-forbidden
  forbidden

Replacing installed files removes the old file from both the producer's tree
and its installed-file capabilities.

  $ cat > opaque/recipe.sh <<'EOF'
  > #!/bin/sh
  > set -eu
  > echo replacement > replacement
  > printf 'share: [ "replacement" ]\n' > opaque.install
  > EOF
  $ cat >> dune <<'EOF'
  > (rule
  >  (target replacement)
  >  (deps (package opaque))
  >  (action (with-stdout-to %{target} (cat %{pkg:opaque:share:replacement}))))
  > (rule
  >  (target removed)
  >  (deps (package opaque))
  >  (action (with-stdout-to %{target} (cat %{pkg:opaque:share:result}))))
  > EOF
  $ dune build replacement
  $ cat _build/default/replacement
  replacement
  $ test ! -e _build/_private/default/.opam/*/target/share/opaque/result
  $ dune build removed
  File "dune", line 36, characters 40-66:
  36 |  (action (with-stdout-to %{target} (cat %{pkg:opaque:share:result}))))
                                               ^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: File result not found in section share of package opaque
  [1]

The extension is required even for an empty, source-less package.

  $ mkdir empty-package
  $ cd empty-package
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name empty) (allow_empty))
  > EOF
  $ mkdir empty
  $ cat > empty/dune <<'EOF'
  > (opam (package empty) (dir src) (version 1.0))
  > EOF
  $ cat > dune <<'EOF'
  > (rule
  >  (target result)
  >  (deps (package empty))
  >  (action (write-file %{target} "%{pkg:empty:version} %{pkg:empty:dev}")))
  > EOF
  $ dune build result
  File "empty/dune", line 1, characters 0-46:
  1 | (opam (package empty) (dir src) (version 1.0))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'opam' is available only when unreleased is enabled in the
  dune-project or workspace file. You must enable it using (using unreleased
  0.1) in the file.
  Note however that unreleased is experimental and might change without notice
  in the future.
  [1]
  $ echo '(using unreleased 0.1)' >> dune-project
  $ dune build result
  $ cat _build/default/result
  1.0 false
  $ test -f _build/_private/default/.opam/*/target/cookie
  $ test ! -e _build/_private/default/.opam/*/source

Source-less recipes get an empty writable working directory. Separate contexts
must build separate outputs rather than reusing another context's cookie.

  $ cat > empty/dune <<'EOF'
  > (opam
  >  (package empty)
  >  (dir src)
  >  (version 1.0)
  >  (build
  >   (run sh -c
  >    "\| set -eu
  >    "\| test -z "$(ls -A)"
  >    "\| printf '%{context_name}' > context
  >    "\| printf 'share: [ "context" ]' > empty.install
  >    )))
  > EOF
  $ cat > dune <<'EOF'
  > (rule
  >  (target result)
  >  (deps (package empty))
  >  (action (with-stdout-to %{target} (cat %{pkg:empty:share:context}))))
  > EOF
  $ cat > dune-workspace <<'EOF'
  > (lang dune 3.25)
  > (context (default))
  > (context (default (name other)))
  > EOF
  $ dune build _build/default/result _build/other/result
  $ cat _build/default/result
  default
  $ cat _build/other/result
  other

Requesting an opaque build in a cross-compilation target context is rejected.

  $ cat > dune-workspace <<'EOF'
  > (lang dune 3.25)
  > (context (default))
  > (context (default (name target) (host default)))
  > EOF
  $ dune build _build/target/result
  File "empty/dune", lines 1-11, characters 0-212:
   1 | (opam
   2 |  (package empty)
   3 |  (dir src)
   4 |  (version 1.0)
   5 |  (build
   6 |   (run sh -c
   7 |    "\| set -eu
   8 |    "\| test -z "$(ls -A)"
   9 |    "\| printf '%{context_name}' > context
  10 |    "\| printf 'share: [ "context" ]' > empty.install
  11 |    )))
  Error: Opam builds are not supported in cross-compilation target contexts.
  [1]

Opaque recipes can read direct dependency files and variables, and use direct
and transitive binaries. Exported paths survive the producer's sandbox, and
updates from a single package retain their declaration order.

  $ mkdir ../dependencies
  $ cd ../dependencies
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name consumer) (dir consumer) (depends direct) (allow_empty))
  > (package (name direct) (dir direct) (depends transitive) (allow_empty))
  > (package (name transitive) (dir transitive) (allow_empty))
  > (package (name unrelated) (dir unrelated) (allow_empty))
  > EOF
  $ mkdir consumer direct transitive unrelated
  $ cat > unrelated/dune <<'EOF'
  > (opam
  >  (package unrelated)
  >  (dir src)
  >  (version 1.0)
  >  (build (run sh -c "echo unrelated >&2; exit 1")))
  > EOF
  $ cat > transitive/dune <<'EOF'
  > (opam
  >  (package transitive)
  >  (dir src)
  >  (version 3.0)
  >  (build
  >   (progn
  >    (write-file data "transitive-data\n")
  >    (write-file transitive-tool "#!/bin/sh\necho transitive-tool >\"$1\"\n")
  >    (run chmod +x transitive-tool)
  >    (write-file transitive.install
  >     "share: [ \"data\" ] bin: [ \"transitive-tool\" ]"))))
  > EOF
  $ cat > direct/dune <<'EOF'
  > (opam
  >  (package direct)
  >  (dir src)
  >  (version 2.0)
  >  (build
  >   (progn
  >    (run cp %{pkg:transitive:share:data} data)
  >    (write-file direct-tool "#!/bin/sh\necho direct-tool >\"$1\"\n")
  >    (run chmod +x direct-tool)
  >    (write-file direct.config
  >     "opam-version: \"2.0\"\nvariables { answer: \"direct-value\" }\n")
  >    (write-file direct.install
  >     "share: [ \"data\" ] bin: [ \"direct-tool\" ]")))
  >  (exported_env
  >   (= ORDER first)
  >   (= ORDER second)
  >   (= DIRECT_DATA %{pkg-self:share}/data)))
  > EOF
  $ cat > consumer/dune <<'EOF'
  > (opam
  >  (package consumer)
  >  (dir src)
  >  (version 1.0)
  >  (build
  >   (progn
  >    (run direct-tool direct.out)
  >    (run transitive-tool transitive.out)
  >    (run sh -c
  >     "\| set -eu
  >     "\| test '%{pkg:direct:version}' = 2.0
  >     "\| test "$ORDER" = second
  >     "\| transitive-tool path.out
  >     "\| {
  >     "\| cat '%{pkg:direct:share:data}'
  >     "\| printf '%{pkg:direct:answer}\n'
  >     "\| cat "$DIRECT_DATA" direct.out transitive.out path.out
  >     "\| } > result
  >     )
  >    (write-file consumer.install "share: [ \"result\" ]"))))
  > EOF
  $ cat > dune <<'EOF'
  > (rule
  >  (target result)
  >  (deps (package consumer))
  >  (action (with-stdout-to %{target} (cat %{pkg:consumer:share:result}))))
  > EOF
  $ dune build result
  $ cat _build/default/result
  transitive-data
  direct-value
  transitive-data
  direct-tool
  transitive-tool
  transitive-tool

Transitive binaries do not expose transitive installed files or variables.

  $ cat > consumer/dune <<'EOF'
  > (opam
  >  (package consumer)
  >  (dir src)
  >  (version 1.0)
  >  (build (run cat %{pkg:transitive:share:data})))
  > EOF
  $ dune build result
  File "consumer/dune", line 5, characters 17-45:
  5 |  (build (run cat %{pkg:transitive:share:data})))
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Package transitive is not a direct dependency
  [1]
  $ cat > consumer/dune <<'EOF'
  > (opam
  >  (package consumer)
  >  (dir src)
  >  (version 1.0)
  >  (build (run echo %{pkg:transitive:version})))
  > EOF
  $ dune build result
  File "consumer/dune", line 5, characters 18-43:
  5 |  (build (run echo %{pkg:transitive:version})))
                        ^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Undefined package variable "version"
  [1]

Failed commands retain their stderr diagnostics, identify the package, and
include its declared external dependencies in the hint.

  $ export DUNE_DEBUG_PACKAGE_LOGS=0
  $ cat > consumer/dune <<'EOF'
  > (opam
  >  (package consumer)
  >  (dir src)
  >  (version 1.0)
  >  (depexts missing-system-package)
  >  (build (run sh -c "echo opaque-failure >&2; exit 1")))
  > EOF
  $ dune build result
  File "consumer/dune", line 6, characters 13-15:
  6 |  (build (run sh -c "echo opaque-failure >&2; exit 1")))
                   ^^
  Error: Logs for package consumer
  opaque-failure
  
  Hint:
  You may want to verify the following depexts are installed:
  missing-system-package
  [1]

A connected acceptance scenario starts with a native package supplying the
opaque recipe. An ordinary rule explicitly depends on the opaque package and
consumes its installed file, binary, and variable. Both native and opaque
sources remain immutable, and changing the native producer invalidates the
opaque build and its consumer.

  $ mkdir ../acceptance && cd ../acceptance
  $ cat >dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name producer) (dir producer))
  > (package (name opaque) (dir opaque) (depends producer) (allow_empty))
  > EOF
  $ mkdir producer opaque
  $ cat >producer/recipe.sh <<'EOF'
  > #!/bin/sh
  > set -eu
  > {
  >   cat input
  >   echo recipe-v1
  > } > result
  > cat > acceptance-tool <<'TOOL'
  > #!/bin/sh
  > echo tool-v1
  > TOOL
  > chmod +x acceptance-tool
  > cat > opaque.install <<'INSTALL'
  > share: [ "result" ]
  > bin: [ "acceptance-tool" ]
  > INSTALL
  > printf 'opam-version: "2.0"\nvariables { answer: "variable-v1" }\n' \
  >   > opaque.config
  > echo sandbox-input > input
  > echo '# sandbox-recipe' >> recipe.sh
  > EOF
  $ cat >producer/dune <<'EOF'
  > (install
  >  (package producer)
  >  (section share)
  >  (files recipe.sh))
  > EOF
  $ echo source-v1 >opaque/input
  $ cat >opaque/dune <<'EOF'
  > (opam
  >  (package opaque)
  >  (dir src)
  >  (version 1.0)
  >  (source ..)
  >  (files (%{pkg:producer:share:recipe.sh} recipe.sh))
  >  (build (run sh ./recipe.sh)))
  > EOF
  $ cat >dune <<'EOF'
  > (rule
  >  (target consumed)
  >  (deps (package opaque))
  >  (action
  >   (with-stdout-to %{target}
  >    (progn
  >     (cat %{pkg:opaque:share:result})
  >     (echo "%{pkg:opaque:answer}\n")
  >     (run %{bin:acceptance-tool})))))
  > EOF
  $ cp producer/recipe.sh recipe.before
  $ dune build consumed
  $ cat _build/default/consumed
  source-v1
  recipe-v1
  variable-v1
  tool-v1
  $ cmp recipe.before producer/recipe.sh
  $ cat opaque/input
  source-v1

Changing only the native producer changes every consumed capability. The
recipe's attempts to modify its prepared inputs remain confined to its sandbox.

  $ sed 's/v1/v2/g' producer/recipe.sh >recipe.new
  $ mv recipe.new producer/recipe.sh
  $ cp producer/recipe.sh recipe.before
  $ dune build consumed
  $ cat _build/default/consumed
  source-v1
  recipe-v2
  variable-v2
  tool-v2
  $ cmp recipe.before producer/recipe.sh
  $ cat opaque/input
  source-v1

Changing only exports must invalidate ordinary and opaque consumers, even when
installed files and cookie contents remain unchanged.

  $ mkdir ../exports && cd ../exports
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name producer) (dir producer) (allow_empty))
  > (package (name consumer) (dir consumer) (depends producer) (allow_empty))
  > EOF
  $ mkdir producer consumer
  $ cat > producer/dune <<'EOF'
  > (opam
  >  (package producer)
  >  (dir src)
  >  (version 1.0)
  >  (exported_env (= OPAM_STANZA_EXPORT one)))
  > EOF
  $ cat > consumer/dune <<'EOF'
  > (opam
  >  (package consumer)
  >  (dir src)
  >  (version 1.0)
  >  (build
  >   (progn
  >    (run sh -c "printf '%s\\n' \"$OPAM_STANZA_EXPORT\" > result")
  >    (write-file consumer.install "share: [ \"result\" ]"))))
  > EOF
  $ cat > dune <<'EOF'
  > (rule
  >  (target ordinary)
  >  (deps (package producer))
  >  (action
  >   (with-stdout-to %{target}
  >    (system "printf '%s\\n' \"$OPAM_STANZA_EXPORT\""))))
  > (rule
  >  (target opaque)
  >  (deps (package consumer))
  >  (action (with-stdout-to %{target} (cat %{pkg:consumer:share:result}))))
  > EOF
  $ dune build ordinary opaque
  $ cat _build/default/ordinary _build/default/opaque
  one
  one
  $ cp _build/_private/default/.opam/producer-*/target/cookie cookie.before
  $ sed 's/OPAM_STANZA_EXPORT one/OPAM_STANZA_EXPORT two/' producer/dune > producer/dune.new
  $ mv producer/dune.new producer/dune
  $ dune build ordinary opaque
  $ cmp cookie.before _build/_private/default/.opam/producer-*/target/cookie
  $ cat _build/default/ordinary _build/default/opaque
  two
  two

Absent optional dependencies are search candidates, not packages to materialize.
Missing required workspace dependencies instead get a user-facing diagnostic.

  $ mkdir ../optional-dependency && cd ../optional-dependency
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name a) (dir a) (depends b) (allow_empty))
  > (package (name b) (dir b) (depopts absent) (allow_empty))
  > EOF
  $ mkdir a b
  $ echo '(opam (package a) (dir src) (version 1.0))' > a/dune
  $ cat > dune <<'EOF'
  > (rule
  >  (target result)
  >  (deps (package a))
  >  (action (write-file %{target} ok)))
  > EOF
  $ dune build result
  $ cat _build/default/result
  ok
  $ sed 's/depopts/depends/' dune-project > dune-project.new
  $ mv dune-project.new dune-project
  $ dune build result 2>error
  [1]
  $ grep '^Error:' error
  Error: The dependencies of package "b" could not be satisfied:

Included package dependencies must enable binary lookup as well as PATH. The
include below is generated and contains another include.

  $ mkdir ../included-dependencies && cd ../included-dependencies
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name opaque) (allow_empty))
  > EOF
  $ mkdir opaque
  $ cat > opaque/dune <<'EOF'
  > (opam
  >  (package opaque)
  >  (dir src)
  >  (version 1.0)
  >  (build
  >   (progn
  >    (write-file included-opam-tool "#!/bin/sh\necho included\n")
  >    (run chmod +x included-opam-tool)
  >    (write-file opaque.install "bin: [ \"included-opam-tool\" ]"))))
  > EOF
  $ echo '((package opaque))' > inner.sexp
  $ cat > dune <<'EOF'
  > (rule
  >  (target included.sexp)
  >  (action (write-file %{target} "((include inner.sexp))")))
  > (rule
  >  (target included-bin)
  >  (deps (include included.sexp))
  >  (action (with-stdout-to %{target} (run %{bin:included-opam-tool}))))
  > (rule
  >  (target path-bin)
  >  (deps (include included.sexp))
  >  (action (with-stdout-to %{target} (system "included-opam-tool"))))
  > EOF
  $ dune build included-bin
  $ cat _build/default/included-bin
  included
  $ dune build path-bin
  $ cat _build/default/path-bin
  included

Opaque recipes use context-local tools, just like ordinary rules. Each context
also supplies its own make.

  $ mkdir ../context-tools && cd ../context-tools
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name opaque) (allow_empty))
  > EOF
  $ mkdir opaque
  $ for context in default other; do
  >   mkdir "$context-tools"
  >   printf '#!/bin/sh\necho %s > "$1"\n' "$context" > "$context-tools/opam-context-tool"
  >   for make in make gmake; do
  >     printf '#!/bin/sh\necho make-%s > "$1"\n' "$context" > "$context-tools/$make"
  >   done
  >   chmod +x "$context-tools/"*
  > done
  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (context (default (paths (PATH "$PWD/default-tools" :standard))))
  > (context (default (name other) (paths (PATH "$PWD/other-tools" :standard))))
  > EOF
  $ cat > opaque/dune <<'EOF'
  > (opam
  >  (package opaque)
  >  (dir src)
  >  (version 1.0)
  >  (build
  >   (progn
  >    (run opam-context-tool result)
  >    (run %{make} make-result)
  >    (write-file opaque.install "share: [ \"result\" \"make-result\" ]"))))
  > EOF
  $ cat > dune <<'EOF'
  > (rule (target native-result) (action (run opam-context-tool %{target})))
  > (rule
  >  (target result)
  >  (deps (package opaque))
  >  (action
  >   (with-stdout-to %{target}
  >    (cat %{pkg:opaque:share:result} %{pkg:opaque:share:make-result}))))
  > EOF
  $ dune build _build/default/native-result _build/other/native-result
  $ cat _build/default/native-result _build/other/native-result
  default
  other
  $ dune build _build/default/result _build/other/result
  $ cat _build/default/result _build/other/result
  default
  make-default
  other
  make-other

An install action and a manifest can describe the same installed file. Discovery
must not turn that into a package conflicting with itself. The manifest's section
also takes precedence over the install scanner's executable-file guess.

  $ mkdir ../install-overlap && cd ../install-overlap
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name p) (allow_empty))
  > EOF
  $ mkdir p
  $ cat > p/dune <<'EOF'
  > (opam
  >  (package p)
  >  (dir src)
  >  (version 1.0)
  >  (build
  >   (progn
  >    (write-file payload "value\n")
  >    (write-file p.install "share: [ \"payload\" ]")))
  >  (install (run cp payload %{pkg-self:share}/payload)))
  > EOF
  $ cat > dune <<'EOF'
  > (rule
  >  (target result)
  >  (deps (package p))
  >  (action (with-stdout-to %{target} (cat %{pkg:p:share:payload}))))
  > EOF
  $ dune build result
  $ cat _build/default/result
  value

  $ cat > p/dune <<'EOF'
  > (opam
  >  (package p)
  >  (dir src)
  >  (version 1.0)
  >  (build
  >   (progn
  >    (write-file payload "executable\n")
  >    (run chmod +x payload)
  >    (write-file p.install "lib: [ \"payload\" ]")))
  >  (install (run cp payload %{pkg-self:lib}/payload)))
  > EOF
  $ sed 's/p:share:/p:lib:/' dune > dune.new && mv dune.new dune
  $ dune build result
  $ cat _build/default/result
  executable

Nested bin files remain addressable by package and path, but are not executable
search candidates. Distinct nested files with the same basename do not collide.

  $ mkdir ../nested-binaries && cd ../nested-binaries
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name producer) (dir producer) (allow_empty))
  > (package (name consumer) (dir consumer) (depends producer) (allow_empty))
  > EOF
  $ mkdir producer consumer
  $ echo 'bin: [ "tool" {"a/nested-opam-tool"} ]' > producer/producer.install
  $ cat > producer/dune <<'EOF'
  > (opam
  >  (package producer)
  >  (dir src)
  >  (version 1.0)
  >  (source ..)
  >  (build
  >   (progn
  >    (write-file tool "#!/bin/sh\necho nested > \"$1\"\n")
  >    (run chmod +x tool))))
  > EOF
  $ cat > consumer/dune <<'EOF'
  > (opam
  >  (package consumer)
  >  (dir src)
  >  (version 1.0)
  >  (build
  >   (progn
  >    (run nested-opam-tool result)
  >    (write-file consumer.install "share: [ \"result\" ]"))))
  > EOF
  $ cat > dune <<'EOF'
  > (rule
  >  (target direct)
  >  (deps (package producer))
  >  (action (run %{pkg:producer:bin:a/nested-opam-tool} %{target})))
  > (rule
  >  (target ordinary)
  >  (deps (package producer))
  >  (action (run %{bin:nested-opam-tool} %{target})))
  > (rule
  >  (target opaque)
  >  (deps (package consumer))
  >  (action (with-stdout-to %{target} (cat %{pkg:consumer:share:result}))))
  > (rule
  >  (target empty-consumer)
  >  (deps (package consumer))
  >  (action (write-file %{target} ok)))
  > EOF
  $ dune build direct
  $ cat _build/default/direct
  nested
  $ dune build ordinary 2>error
  [1]
  $ grep '^Error:' error
  Error: Program nested-opam-tool not found in the tree or in PATH
  $ dune build opaque 2>error
  [1]
  $ grep '^Error:' error
  Error: Program nested-opam-tool not found in the tree or in PATH
  $ echo 'bin: [ "tool" {"a/nested-opam-tool"} "tool" {"b/nested-opam-tool"} ]' > producer/producer.install
  $ echo '(opam (package consumer) (dir src) (version 1.0))' > consumer/dune
  $ dune build empty-consumer
  $ cat _build/default/empty-consumer
  ok

A subdir declaration changes the effective parent of the Opam directory. Its
native configuration and data_only_dirs settings coexist with that declaration.

  $ mkdir ../inherited && cd ../inherited
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name opaque) (allow_empty))
  > EOF
  $ mkdir -p wrapper/src wrapper/ignored
  $ echo '(include missing)' > wrapper/src/dune
  $ echo '(' > wrapper/ignored/dune
  $ cat > wrapper/dune <<'EOF'
  > (data_only_dirs ignored)
  > (rule (target parent) (action (write-file %{target} native)))
  > EOF
  $ cat > dune <<'EOF'
  > (subdir wrapper
  >  (opam (package opaque) (dir src) (version 1.0)))
  > (rule
  >  (target result)
  >  (deps (package opaque))
  >  (action (write-file %{target} "%{pkg:opaque:version}")))
  > EOF
  $ dune build result wrapper/parent
  $ cat _build/default/result _build/default/wrapper/parent
  1.0native
  $ echo '(' > wrapper/src/dune
  $ echo '(' > wrapper/src/dune-project
  $ dune build result
  $ cat _build/default/result
  1.0
  $ echo '(data_only_dirs ignored src)' > wrapper/dune
  $ dune build result
  $ cat _build/default/result
  1.0

Configured scalar and list values are available to ordinary rules and exported
environments.

  $ mkdir ../config-variables && cd ../config-variables
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name p) (allow_empty))
  > EOF
  $ mkdir p
  $ cat > p/dune <<'EOF'
  > (opam
  >  (package p)
  >  (dir src)
  >  (version 1.0)
  >  (build
  >   (progn
  >    (write-file data "payload\n")
  >    (write-file p.config
  >     "\| opam-version: "2.0"
  >     "\| variables {
  >     "\|   message: "configured"
  >     "\|   values: ["first" "second"]
  >     "\| }
  >    )
  >    (write-file p.install "share: [ \"data\" ]")))
  >  (exported_env (= MESSAGE %{pkg-self:message})))
  > EOF
  $ cat > dune <<'EOF'
  > (rule
  >  (target raw)
  >  (deps (package p))
  >  (action (write-file %{target} "%{pkg:p:message}")))
  > (rule
  >  (target values)
  >  (deps (package p))
  >  (action (with-stdout-to %{target} (echo %{pkg:p:values}))))
  > (rule
  >  (target result)
  >  (deps (package p))
  >  (action (with-stdout-to %{target} (system "echo \"$MESSAGE\""))))
  > EOF
  $ dune build raw values
  $ cat _build/default/raw
  configured
  $ cat _build/default/values
  first second
  $ dune build result
  $ cat _build/default/result
  configured

Materializing a file alone does not apply exports. The invalid export below is
only diagnosed when constructing a consumer's package environment.

  $ sed 's/pkg-self:message/pkg-self:missing/' p/dune > p/dune.new
  $ mv p/dune.new p/dune
  $ cat >> dune <<'EOF'
  > (rule
  >  (target file-only)
  >  (action
  >   (with-stdout-to %{target} (cat %{pkg:p:share:data}))))
  > EOF
  $ dune build file-only
  $ cat _build/default/file-only
  payload
  $ dune build result 2>error
  [1]
  $ grep '^Error:' error
  Error: Undefined package variable: missing

An Opam directory is declared in its parent. The parent's native rules remain
active, while even malformed configuration in the selected child is data.

  $ mkdir ../parent-dir && cd ../parent-dir
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name geometry) (allow_empty))
  > EOF
  $ mkdir geometry sibling
  $ echo '(' > geometry/dune
  $ echo '(' > geometry/dune-project
  $ echo child-input > geometry/input
  $ cat > dune <<'EOF'
  > (opam
  >  (package geometry)
  >  (dir geometry)
  >  (version 1.0)
  >  (files (input input))
  >  (build (write-file geometry.install "share: [ \"input\" ]")))
  > (rule (target parent) (action (write-file %{target} parent)))
  > (rule
  >  (target result)
  >  (deps (package geometry))
  >  (action (with-stdout-to %{target} (cat %{pkg:geometry:share:input}))))
  > EOF
  $ echo '(rule (target result) (action (write-file %{target} sibling)))' > sibling/dune
  $ dune build parent sibling/result result
  $ cat _build/default/parent _build/default/sibling/result _build/default/result
  parentsiblingchild-input

Changing dir switches the prepared inputs and restores native loading in the
previous child. The selected directory is independent of the package name.

  $ rm geometry/dune-project
  $ echo '(rule (target restored) (action (write-file %{target} native)))' > geometry/dune
  $ mkdir alternate
  $ echo '(' > alternate/dune
  $ echo '(' > alternate/dune-project
  $ echo alternate-input > alternate/input
  $ sed 's/(dir geometry)/(dir alternate)/' dune > dune.new
  $ mv dune.new dune
  $ dune build result geometry/restored
  $ cat _build/default/result _build/default/geometry/restored
  alternate-input
  native

Static includes, multiple Opam declarations, and explicit data_only_dirs can
coexist. Directory names are literal, not globs.

  $ echo '(package (name literal) (allow_empty))' >> dune-project
  $ mkdir 'data[0]' data0 ignored
  $ echo '(' > 'data[0]/dune'
  $ echo '(' > ignored/dune
  $ echo '(rule (target result) (action (write-file %{target} literal-neighbor)))' > data0/dune
  $ cat > packages.inc <<'EOF'
  > (opam
  >  (package geometry)
  >  (dir alternate)
  >  (version 1.0)
  >  (source .)
  >  (build (write-file geometry.install "share: [ \"input\" ]")))
  > (opam (package literal) (dir "data[0]") (version 2.0))
  > EOF
  $ cat > dune <<'EOF'
  > (include packages.inc)
  > (data_only_dirs ignored alternate)
  > (rule
  >  (target result)
  >  (deps (package geometry) (package literal))
  >  (action
  >   (with-stdout-to %{target}
  >    (progn
  >     (cat %{pkg:geometry:share:input})
  >     (echo "%{pkg:literal:version}")))))
  > EOF
  $ dune build result data0/result
  $ cat _build/default/result _build/default/data0/result
  alternate-input
  2.0literal-neighbor


Ordinary binary lookup can discover an opaque tool without an explicit package
materialization dependency, including when there is no lock directory.

  $ mkdir ../binary-lookup && cd ../binary-lookup
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name tools) (dir tools) (allow_empty))
  > (package (name client) (dir client) (depends tools) (allow_empty))
  > EOF
  $ mkdir tools client
  $ cat > tools/dune <<'EOF'
  > (opam
  >  (package tools)
  >  (dir src)
  >  (version 1.0)
  >  (install
  >   (run sh -c "printf '#!/bin/sh\\necho discovered\\n' > '%{bin}/opaque-tool'; chmod +x '%{bin}/opaque-tool'")))
  > EOF
  $ cat > client/dune <<'EOF'
  > (rule
  >  (target result)
  >  (action (with-stdout-to %{target} (run %{bin:opaque-tool}))))
  > EOF
  $ dune build client/result
  $ cat _build/default/client/result
  discovered

Opaque directory declarations must be static. Neither dynamic includes nor
OCaml scripts can establish the directory's status before native traversal.

  $ mkdir ../generated-opam && cd ../generated-opam
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name opaque) (allow_empty))
  > EOF
  $ mkdir included
  $ cat > recipe.inc <<'EOF'
  > (opam (package opaque) (dir src) (version 1.0) (build (run false)))
  > (rule (target result) (deps (package opaque))
  >  (action (write-file %{target} built)))
  > EOF
  $ echo '(dynamic_include ../recipe.inc)' > included/dune
  $ dune build included/result 2>error
  [1]
  $ grep '^Error:' error
  Error: Opam stanzas must be declared statically.
  $ rm -rf included
  $ cat > dune <<'EOF'
  > (* -*- tuareg -*- *)
  > let () = Jbuild_plugin.V1.send {|
  > (opam (package opaque) (dir src) (version 1.0))
  > (rule (target result) (deps (package opaque))
  >  (action (write-file %{target} built)))
  > |}
  > EOF
  $ dune build result 2>error
  [1]
  $ grep '^Error:' error
  Error: Opam stanzas must be declared statically.

Versions must not introduce path components into the opaque package key.

  $ mkdir ../unsafe-version && cd ../unsafe-version
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name opaque) (allow_empty))
  > EOF
  $ cat > dune <<'EOF'
  > (opam (package opaque) (dir src) (version dev/topic))
  > (rule (target result) (deps (package opaque))
  >  (action (write-file %{target} %{pkg:opaque:version})))
  > EOF
  $ dune build result
  $ cat _build/default/result
  dev/topic
  $ sed 's,dev/topic,dev/other,' dune > dune.new
  $ mv dune.new dune
  $ dune build result
  $ cat _build/default/result
  dev/other

External prepared inputs must become writable copies, preserving executable
bits without changing the supplied files or their permissions.

  $ mkdir ../external-inputs && cd ../external-inputs
  $ export PREPARED_DIR=$(mktemp -d)
  $ echo original > "$PREPARED_DIR/input"
  $ cat > "$PREPARED_DIR/recipe.sh" <<'EOF'
  > #!/bin/sh
  > set -eu
  > test -w input
  > test -w recipe.sh
  > test ! -x input
  > cat input > output
  > echo changed > input
  > printf 'share: [ "output" ]\n' > opaque.install
  > EOF
  $ chmod 444 "$PREPARED_DIR/input"
  $ chmod 555 "$PREPARED_DIR/recipe.sh"
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name opaque) (allow_empty))
  > EOF
  $ cat > dune <<'EOF'
  > (opam
  >  (package opaque)
  >  (dir src)
  >  (version 1.0)
  >  (files
  >   (%{env:PREPARED_DIR=unset}/input input)
  >   (%{env:PREPARED_DIR=unset}/recipe.sh recipe.sh))
  >  (build (run ./recipe.sh)))
  > (rule (target result) (deps (package opaque))
  >  (action (with-stdout-to %{target} (cat %{pkg:opaque:share:output}))))
  > EOF
  $ dune build result
  $ cat _build/default/result
  original
  $ cat "$PREPARED_DIR/input"
  original
  $ test ! -w "$PREPARED_DIR/input"
  $ test ! -w "$PREPARED_DIR/recipe.sh"
  $ test -x "$PREPARED_DIR/recipe.sh"
  $ rm -rf "$PREPARED_DIR"
