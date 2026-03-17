# How to Use Multiple Build Contexts

Build contexts let Dune compile the same sources in multiple isolated
environments. This is useful when one build is part of the normal workflow and
another is only needed on demand, such as a ThreadSanitizer build.

If you have used Dune before, you have been using one context already: the
`default` context. You can see this in the build directory at `_build/default`,
which is where Dune will put the build artifacts of your build. By adding more
contexts, you can ask Dune to build the same sources in different environments
while keeping the artifacts separate.

This can be useful in certain cases: for example you want to test your project
with multiple compilers without having to edit your `dune-project` file, or opam
switches every time. We will see a use case of this with **ThreadSanitizer**
below.

::: {note}
ThreadSanitizer is a dynamic data race detector that requires a
special version of the OCaml compiler. You can find more information at the
[manual page](https://ocaml.org/manual/5.3/tsan.html). However, familiarity with
ThreadSanitizer is not necessary as this is only an example to show how Dune
contexts can be used.
:::

This guide shows two ways to do that:

- with Dune package management and a separate `lock_dir`
- with an `opam` switch for the alternate compiler

## Example Program

The examples below use a small program with a data race.

:::{literalinclude} use-multiple-build-contexts/dune-project
:language: dune
:caption: dune-project
:::

:::{literalinclude} use-multiple-build-contexts/dune
:language: dune
:caption: dune
:::

:::{literalinclude} use-multiple-build-contexts/tsan_check.ml
:language: ocaml
:caption: tsan_check.ml
:::

Build and run it with your current compiler:

```sh
$ dune exec ./tsan_check.exe
```

With a stock compiler, you may see the assertion fail, but you will not get a
ThreadSanitizer report explaining the race.

## Use Dune Package Management

First, declare `ocaml-option-tsan` as an optional dependency in
`dune-workspace`, for the ThreadSanitizer build. Then create a `dune-workspace`
file that defines two lock directories and attaches one context to each:

:::{literalinclude} use-multiple-build-contexts/dune-workspace.pkg
:language: dune
:emphasize-lines: 6-8,10-15
:caption: dune-workspace
:::

Generate the lock directories:

```sh
$ dune pkg lock dune.lock dune-tsan.lock
```

Build and run the binaries:

```sh
$ dune exec --context=default ./tsan_check.exe
$ dune exec --context=tsan ./tsan_check.exe
```


The `tsan` binary reports the data race, while the `default` binary uses your
normal compiler setup.

::::{dropdown} ThreadSanitizer output
:icon: file-code

```sh
WARNING: ThreadSanitizer: data race (pid=1662846)
  Write of size 8 at 0x7fa28ecffa58 by main thread (mutexes: write M78):
    #0 camlDune__exe__Tsan_check.inc_272 <null> (tsan_check.exe+0x483e2)
    #1 camlDune__exe__Tsan_check.entry <null> (tsan_check.exe+0x484df)
    #2 caml_program <null> (tsan_check.exe+0x460a9)
    #3 caml_start_program <null> (tsan_check.exe+0xdca47)
    #4 caml_startup_common runtime/startup_nat.c:132 (tsan_check.exe+0xdc1f0)
    #5 caml_startup_common runtime/startup_nat.c:88 (tsan_check.exe+0xdc1f0)
    #6 caml_startup_exn runtime/startup_nat.c:139 (tsan_check.exe+0xdc29b)
    #7 caml_startup runtime/startup_nat.c:144 (tsan_check.exe+0xdc29b)
    #8 caml_main runtime/startup_nat.c:151 (tsan_check.exe+0xdc29b)
    #9 main runtime/main.c:37 (tsan_check.exe+0x45bf9)

  Previous write of size 8 at 0x7fa28ecffa58 by thread T1 (mutexes: write M83):
    #0 camlDune__exe__Tsan_check.inc_272 <null> (tsan_check.exe+0x483e2)
    #1 camlStdlib__Domain.body_735 <null> (tsan_check.exe+0x7b8ef)
    #2 caml_start_program <null> (tsan_check.exe+0xdca47)
    #3 caml_callback_exn runtime/callback.c:201 (tsan_check.exe+0x9ee53)
    #4 domain_thread_func runtime/domain.c:1215 (tsan_check.exe+0xa32d6)

  Mutex M78 (0x561082d9e728) created at:
    #0 pthread_mutex_init ../../../../src/libsanitizer/tsan/tsan_interceptors_posix.cpp:1227 (libtsan.so.0+0x4bee1)
    #1 caml_plat_mutex_init runtime/platform.c:57 (tsan_check.exe+0xc9e6a)
    #2 caml_init_domains runtime/domain.c:943 (tsan_check.exe+0xa301e)
    #3 caml_init_gc runtime/gc_ctrl.c:353 (tsan_check.exe+0xaff83)
    #4 caml_startup_common runtime/startup_nat.c:111 (tsan_check.exe+0xdc0d7)
    #5 caml_startup_common runtime/startup_nat.c:88 (tsan_check.exe+0xdc0d7)
    #6 caml_startup_exn runtime/startup_nat.c:139 (tsan_check.exe+0xdc29b)
    #7 caml_startup runtime/startup_nat.c:144 (tsan_check.exe+0xdc29b)
    #8 caml_main runtime/startup_nat.c:151 (tsan_check.exe+0xdc29b)
    #9 main runtime/main.c:37 (tsan_check.exe+0x45bf9)

  Mutex M83 (0x561082d9e840) created at:
    #0 pthread_mutex_init ../../../../src/libsanitizer/tsan/tsan_interceptors_posix.cpp:1227 (libtsan.so.0+0x4bee1)
    #1 caml_plat_mutex_init runtime/platform.c:57 (tsan_check.exe+0xc9e6a)
    #2 caml_init_domains runtime/domain.c:943 (tsan_check.exe+0xa301e)
    #3 caml_init_gc runtime/gc_ctrl.c:353 (tsan_check.exe+0xaff83)
    #4 caml_startup_common runtime/startup_nat.c:111 (tsan_check.exe+0xdc0d7)
    #5 caml_startup_common runtime/startup_nat.c:88 (tsan_check.exe+0xdc0d7)
    #6 caml_startup_exn runtime/startup_nat.c:139 (tsan_check.exe+0xdc29b)
    #7 caml_startup runtime/startup_nat.c:144 (tsan_check.exe+0xdc29b)
    #8 caml_main runtime/startup_nat.c:151 (tsan_check.exe+0xdc29b)
    #9 main runtime/main.c:37 (tsan_check.exe+0x45bf9)

  Thread T1 (tid=1662848, running) created by main thread at:
    #0 pthread_create ../../../../src/libsanitizer/tsan/tsan_interceptors_posix.cpp:969 (libtsan.so.0+0x605b8)
    #1 caml_domain_spawn runtime/domain.c:1265 (tsan_check.exe+0xa499a)
    #2 caml_c_call <null> (tsan_check.exe+0xdc92b)
    #3 camlStdlib__Domain.spawn_730 <null> (tsan_check.exe+0x7b806)
    #4 camlDune__exe__Tsan_check.entry <null> (tsan_check.exe+0x484d1)
    #5 caml_program <null> (tsan_check.exe+0x460a9)
    #6 caml_start_program <null> (tsan_check.exe+0xdca47)
    #7 caml_startup_common runtime/startup_nat.c:132 (tsan_check.exe+0xdc1f0)
    #8 caml_startup_common runtime/startup_nat.c:88 (tsan_check.exe+0xdc1f0)
    #9 caml_startup_exn runtime/startup_nat.c:139 (tsan_check.exe+0xdc29b)
    #10 caml_startup runtime/startup_nat.c:144 (tsan_check.exe+0xdc29b)
    #11 caml_main runtime/startup_nat.c:151 (tsan_check.exe+0xdc29b)
    #12 main runtime/main.c:37 (tsan_check.exe+0x45bf9)

SUMMARY: ThreadSanitizer: data race (/path/to/directory/hello-tsan/_build/tsan/tsan_check.exe+0x483e2) in camlDune__exe__Tsan_check.inc_272
==================
x=5014663 expected=10000000
Fatal error: exception Assert_failure("tsan_check.ml", 19, 2)
ThreadSanitizer: reported 1 warnings
```

::::

## Use an `opam` Context

The second approach uses separate `opam` switches instead of separate lock
directories.

:::{important}
This guide assumes that your ThreadSanitizer compiler environment
already exists, you can find instructions in the [manual
page](https://ocaml.org/manual/5.3/tsan.html). You may also want to install
depexts listed in `dune show depexts`.

:::

You can setup a ThreadSanitizer switch with the following command:

```sh
$ opam switch create 5.3.0+tsan ocaml-option-tsan
```

Create a `dune-workspace` file:

:::{literalinclude} use-multiple-build-contexts/dune-workspace.for-opam
:language: dune
:emphasize-lines: 5-9
:caption: dune-workspace
:::

This keeps the current environment as the `default` context and adds a second
context named `tsan`.

Check the contexts:

```sh
$ dune describe contexts
default
tsan
```

Run the stock binary:

```sh
$ dune exec --context=default ./tsan_check.exe
```

Run the ThreadSanitizer binary:

```sh
$ dune exec --context=tsan ./tsan_check.exe
```

This lets you keep your regular compiler in the `default` context while using
an alternate `opam` switch for the ThreadSanitizer build.
