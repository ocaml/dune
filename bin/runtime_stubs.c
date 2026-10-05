#include <stdlib.h>

#include <caml/mlvalues.h>

#define DUNE_RUNTIME_DEFAULTS_MARKER "DUNE_INTERNAL__RUNTIME_DEFAULTS"

static int runtime_defaults_set = 0;

/* OCaml reads its runtime parameters after native constructors have run. */
static void dune_set_ocaml_runtime_defaults(void) {
  /* Any runtime setting is treated as an explicit user choice. */
  if (getenv("OCAMLRUNPARAM") != NULL || getenv("CAMLRUNPARAM") != NULL) {
    return;
  }
#ifdef _WIN32
  _putenv_s("OCAMLRUNPARAM", "s=512k");
  _putenv_s(DUNE_RUNTIME_DEFAULTS_MARKER, "1");
#else
  setenv("OCAMLRUNPARAM", "s=512k", 0);
  setenv(DUNE_RUNTIME_DEFAULTS_MARKER, "1", 0);
#endif
  runtime_defaults_set = 1;
}

CAMLprim value dune_restore_runtime_environment(value unit) {
  (void)unit;
  if (runtime_defaults_set) {
#ifdef _WIN32
    _putenv_s("OCAMLRUNPARAM", "");
    _putenv_s(DUNE_RUNTIME_DEFAULTS_MARKER, "");
#else
    unsetenv("OCAMLRUNPARAM");
    unsetenv(DUNE_RUNTIME_DEFAULTS_MARKER);
#endif
  }
  return Val_unit;
}

#ifdef _MSC_VER
typedef void(__cdecl *dune_initializer)(void);
#pragma section(".CRT$XCU", read)
__declspec(allocate(".CRT$XCU")) static dune_initializer
    dune_runtime_defaults = dune_set_ocaml_runtime_defaults;
#else
__attribute__((constructor)) static void dune_runtime_defaults(void) {
  dune_set_ocaml_runtime_defaults();
}
#endif
