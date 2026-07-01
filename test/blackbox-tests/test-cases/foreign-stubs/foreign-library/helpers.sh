export DUNE_SANDBOX=symlink

setup_foreign_library_project() {
  make_dune_project 3.0
  mkdir -p lib
}

write_calc_dune() {
  cat >lib/dune <<'EOF'
(foreign_library
 (archive_name addmul)
 (language c)
 (names add mul))
(library
 (name calc)
 (modules calc)
 (foreign_archives addmul config))
EOF
  cat >>lib/dune
}

write_calc_with_month_dune() {
  local day="${1:-}"
  cat >lib/dune <<'EOF'
(foreign_library
 (archive_name addmul)
 (language c)
 (names add mul))
(library
 (name calc)
 (modules calc)
 (foreign_stubs (language c) (names month))
 (foreign_archives addmul config))
EOF
  if [ "$day" = "with-day" ]; then
    cat >>lib/dune <<'EOF'
(foreign_library
 (archive_name day)
 (language c)
 (names day))
EOF
  fi
  cat >>lib/dune
}

write_headers_config_foreign_library() {
  cat <<'EOF'
(foreign_library
 (archive_name config)
 (language cxx)
 (include_dirs headers)
 (extra_deps eight.h)
 (flags :standard -DCONFIG_VALUE=2000)
 (names config))
EOF
}

write_calc_dune_with_headers_config() {
  write_headers_config_foreign_library | write_calc_dune
}

write_calc_with_month_headers_config_dune() {
  write_headers_config_foreign_library | write_calc_with_month_dune "$@"
}

write_add_mul_sources() {
  cat >lib/add.c <<'EOF'
#include <caml/mlvalues.h>
value add(value x, value y) { return Val_int(Int_val(x) + Int_val(y)); }
EOF

  cat >lib/mul.c <<'EOF'
#include <caml/mlvalues.h>
value mul(value x, value y) { return Val_int(Int_val(x) * Int_val(y)); }
EOF
}

write_basic_config_source() {
  cat >lib/config.cpp <<'EOF'
#include <caml/mlvalues.h>
extern "C" value config(value unit) { return Val_int(CONFIG_VALUE); }
EOF
}

write_basic_calc_module() {
  cat >lib/calc.ml <<'EOF'
external add : int -> int -> int = "add"
external mul : int -> int -> int = "mul"
external config : unit -> int = "config"
let calc x y z = add (mul (add x y) z) (config ())
EOF

  cat >lib/calc.mli <<'EOF'
val calc : int -> int -> int -> int
EOF
}

write_basic_main() {
  cat >dune <<'EOF'
(executable
 (name main)
 (modes exe byte)
 (libraries calc)
 (modules main))
EOF

  cat >main.ml <<'EOF'
let () = Printf.printf "%d" (Calc.calc 1 2 3)
EOF
}

write_basic_calc_project() {
  write_add_mul_sources
  write_basic_config_source
  write_basic_calc_module
  write_basic_main
}

write_headers_config_sources() {
  cat >lib/config.cpp <<'EOF'
#include <caml/mlvalues.h>
#include "ten.h"
extern "C" value config(value unit) { return Val_int(CONFIG_VALUE + TEN); }
EOF

  mkdir -p lib/headers/some/deep/path

  cat >lib/headers/ten.h <<'EOF'
#include "../eight.h"
#include "some/deep/path/one.h"
#define TEN (1 + EIGHT + ONE)
EOF

  cat >lib/headers/some/deep/path/one.h <<'EOF'
#define ONE 1
EOF

  cat >lib/eight.h <<'EOF'
#define EIGHT 8
EOF
}

write_month_source() {
  cat >lib/month.c <<'EOF'
#include <caml/mlvalues.h>
#include <caml/alloc.h>
value month(value unit) { return caml_copy_string("October"); }
EOF
}

write_month_calc_module() {
  cat >lib/calc.ml <<'EOF'
external add : int -> int -> int = "add"
external mul : int -> int -> int = "mul"
external config : unit -> int = "config"
external month : unit -> string = "month"
let calc x y z = add (mul (add x y) z) (config ())
EOF

  cat >lib/calc.mli <<'EOF'
val calc : int -> int -> int -> int
val month : unit -> string
EOF
}

write_month_main() {
  cat >main.ml <<'EOF'
let () = Printf.printf "%s %d" (Calc.month ()) (Calc.calc 1 2 3)
EOF
}

write_month_calc_project() {
  write_month_source
  write_month_calc_module
  write_basic_main
  write_month_main
}

write_day_source() {
  cat >lib/day.c <<'EOF'
#include <caml/mlvalues.h>
value day(value unit) { return Val_int(8); }
EOF
}

write_day_main() {
  cat >main.ml <<'EOF'
external day : unit -> int = "day"
let () = Printf.printf "%d %s %d" (day ()) (Calc.month ()) (Calc.calc 1 2 3)
EOF
}
