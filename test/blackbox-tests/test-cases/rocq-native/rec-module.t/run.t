  $ dune build --display short --debug-dependency-path @all
        coqdep .rec_module.theory.d
          coqc b/Nrec_module_b_foo.{cmi,cmxs},b/foo.{glob,vo}
          coqc c/Nrec_module_c_ooo.{cmi,cmxs},c/ooo.{glob,vo}
          coqc c/d/Nrec_module_c_d_bar.{cmi,cmxs},c/d/bar.{glob,vo}
          coqc a/Nrec_module_a_bar.{cmi,cmxs},a/bar.{glob,vo}

  $ dune build --debug-dependency-path @default
  lib: [
    "_build/install/default/lib/rec/META"
    "_build/install/default/lib/rec/dune-package"
    "_build/install/default/lib/rec/opam"
  ]
  lib_root: [
    "_build/install/default/lib/coq/user-contrib/rec_module/a/.coq-native/Nrec_module_a_bar.cmi" {"coq/user-contrib/rec_module/a/.coq-native/Nrec_module_a_bar.cmi"}
    "_build/install/default/lib/coq/user-contrib/rec_module/a/.coq-native/Nrec_module_a_bar.cmxs" {"coq/user-contrib/rec_module/a/.coq-native/Nrec_module_a_bar.cmxs"}
    "_build/install/default/lib/coq/user-contrib/rec_module/a/bar.glob" {"coq/user-contrib/rec_module/a/bar.glob"}
    "_build/install/default/lib/coq/user-contrib/rec_module/a/bar.v" {"coq/user-contrib/rec_module/a/bar.v"}
    "_build/install/default/lib/coq/user-contrib/rec_module/a/bar.vo" {"coq/user-contrib/rec_module/a/bar.vo"}
    "_build/install/default/lib/coq/user-contrib/rec_module/b/.coq-native/Nrec_module_b_foo.cmi" {"coq/user-contrib/rec_module/b/.coq-native/Nrec_module_b_foo.cmi"}
    "_build/install/default/lib/coq/user-contrib/rec_module/b/.coq-native/Nrec_module_b_foo.cmxs" {"coq/user-contrib/rec_module/b/.coq-native/Nrec_module_b_foo.cmxs"}
    "_build/install/default/lib/coq/user-contrib/rec_module/b/foo.glob" {"coq/user-contrib/rec_module/b/foo.glob"}
    "_build/install/default/lib/coq/user-contrib/rec_module/b/foo.v" {"coq/user-contrib/rec_module/b/foo.v"}
    "_build/install/default/lib/coq/user-contrib/rec_module/b/foo.vo" {"coq/user-contrib/rec_module/b/foo.vo"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/.coq-native/Nrec_module_c_ooo.cmi" {"coq/user-contrib/rec_module/c/.coq-native/Nrec_module_c_ooo.cmi"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/.coq-native/Nrec_module_c_ooo.cmxs" {"coq/user-contrib/rec_module/c/.coq-native/Nrec_module_c_ooo.cmxs"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/d/.coq-native/Nrec_module_c_d_bar.cmi" {"coq/user-contrib/rec_module/c/d/.coq-native/Nrec_module_c_d_bar.cmi"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/d/.coq-native/Nrec_module_c_d_bar.cmxs" {"coq/user-contrib/rec_module/c/d/.coq-native/Nrec_module_c_d_bar.cmxs"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/d/bar.glob" {"coq/user-contrib/rec_module/c/d/bar.glob"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/d/bar.v" {"coq/user-contrib/rec_module/c/d/bar.v"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/d/bar.vo" {"coq/user-contrib/rec_module/c/d/bar.vo"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/ooo.glob" {"coq/user-contrib/rec_module/c/ooo.glob"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/ooo.v" {"coq/user-contrib/rec_module/c/ooo.v"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/ooo.vo" {"coq/user-contrib/rec_module/c/ooo.vo"}
  ]
