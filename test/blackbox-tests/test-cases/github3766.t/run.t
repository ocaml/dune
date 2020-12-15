This exercises the difference in behavior when creating archives introduced in
ocaml 4.12.

In 4.11 - .a are always created
In 4.12 - they are omitted if there are no modules or stubs

There's a combination of 3 options:
- With or without stubs
- Wrapped or unwrapped
- Contains an mli only module

Our test checks each of the above with an internal and external library.

  $ ./test.exe
  # mli_only_wrapped_stubs
  -> creating dune-project
  # build the library and see if .a is present
  -> creating dune
  -> creating stub.c
  -> creating foo.mli
  % dune build --root . @install
  Error: exception { exn =
      ("External.cm_dir",
      { t =
          { public_dir = In_build_dir "default/lib"
          ; private_dir = None
          ; public_cmi_dir = None
          }
      })
  ; backtrace =
      [ { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("stanzas-to-entries", "default")
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Memo.Lazy.force in file \"src/memo/memo.ml\"
  (inlined), line 1061, characters 16-20\n\
             Called from Memo.Lazy.bind.(fun) in file \"src/memo/memo.ml\",
  line 1067, characters 45-54\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Memo.Lazy.force in file \"src/memo/memo.ml\"
  (inlined), line 1061, characters 16-20\n\
             Called from Memo.Lazy.bind.(fun) in file \"src/memo/memo.ml\",
  line 1067, characters 45-54\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.get_rules in file
  \"src/dune_rules/scheme.ml\", line 103, characters 6-34\n\
             Called from Dune_rules__Install_rules.gen_rules in file
  \"src/dune_rules/install_rules.ml\", line 871, characters 4-72\n\
             Called from Dune_rules__Gen_rules.gen_rules in file
  \"src/dune_rules/gen_rules.ml\", line 295, characters 25-58\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from
  Dune_engine__Build_system.Load_rules.load_dir_step2_exn in file
  \"src/dune_engine/build_system.ml\", line 895, characters 6-76\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("load-dir", In_build_dir "default")
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Memo.Lazy.force in file \"src/memo/memo.ml\"
  (inlined), line 1061, characters 16-20\n\
             Called from Memo.Lazy.bind.(fun) in file \"src/memo/memo.ml\",
  line 1067, characters 45-54\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.get_rules in file
  \"src/dune_rules/scheme.ml\", line 103, characters 6-34\n\
             Called from Dune_rules__Install_rules.gen_rules in file
  \"src/dune_rules/install_rules.ml\", line 871, characters 4-72\n\
             Called from Dune_rules__Gen_rules.gen_rules in file
  \"src/dune_rules/gen_rules.ml\", line 295, characters 25-58\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from
  Dune_engine__Build_system.Load_rules.load_dir_step2_exn in file
  \"src/dune_engine/build_system.ml\", line 895, characters 6-76\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_engine__Build_system.Load_rules.load_dir_impl in
  file \"src/dune_engine/build_system.ml\", line 1042, characters 12-43\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("load-dir", In_build_dir ".aliases/default")
        }
      ]
  ; outer_call_stack = []
  }
  Raised at Stdune__Code_error.raise in file "src/stdune/code_error.ml", line
    9, characters 30-62
  Called from
    Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in
    file "src/dune_rules/install_rules.ml", line 149, characters 19-69
  Called from
    Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in
    file "src/dune_rules/install_rules.ml" (inlined), line 166, characters
    23-31
  Called from
    Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in
    file "src/dune_rules/install_rules.ml", line 181, characters 49-63
  Called from Stdlib__list.rev_map.rmap_f in file "list.ml", line 103,
    characters 22-25
  Called from Stdune__List.map in file "src/stdune/list.ml", line 5, characters
    19-33
  Called from Stdlib__list.rev_map.rmap_f in file "list.ml", line 103,
    characters 22-25
  Called from Stdune__List.map in file "src/stdune/list.ml" (inlined), line 5,
    characters 19-33
  Called from Stdune__List.concat_map in file "src/stdune/list.ml", line 40,
    characters 29-39
  Called from Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in
    file "src/dune_rules/install_rules.ml", line 156, characters 6-1023
  Called from Dune_rules__Dir_with_dune.inner_fold in file
    "src/dune_rules/dir_with_dune.ml", line 24, characters 55-67
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Install_rules.install_entries in file
    "src/dune_rules/install_rules.ml", line 699, characters 17-59
  Called from Dune_rules__Install_rules.install_rules in file
    "src/dune_rules/install_rules.ml", line 708, characters 4-32
  Called from Dune_rules__Install_rules.memo.(fun) in file
    "src/dune_rules/install_rules.ml", line 839, characters 16-38
  Called from Stdune__Exn.protectx in file "src/stdune/exn.ml", line 12,
    characters 8-11
  Re-raised at Stdune__Exn.protectx in file "src/stdune/exn.ml", line 18,
    characters 4-11
  Called from Memo__Implicit_output.collect_sync in file
    "src/memo/implicit_output.ml", line 119, characters 4-162
  Called from Dune_engine__Rules.collect_opt in file "src/dune_engine/rules.ml"
    (inlined), line 192, characters 20-71
  Called from Dune_engine__Rules.collect in file "src/dune_engine/rules.ml",
    line 195, characters 20-33
  Called from Dune_engine__Rules.collect_unit in file
    "src/dune_engine/rules.ml", line 199, characters 18-27
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Install_rules.memo.(fun) in file
    "src/dune_rules/install_rules.ml", line 847, characters 48-71
  Called from Dune_rules__Scheme.evaluate.loop in file
    "src/dune_rules/scheme.ml", line 138, characters 27-33
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
    "src/dune_rules/scheme.ml", line 76, characters 51-70
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Memo.Lazy.force in file "src/memo/memo.ml" (inlined), line 1061,
    characters 16-20
  Called from Memo.Lazy.bind.(fun) in file "src/memo/memo.ml", line 1067,
    characters 45-54
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Scheme.Evaluated.get_rules in file
    "src/dune_rules/scheme.ml", line 103, characters 6-34
  Called from Dune_rules__Install_rules.gen_rules in file
    "src/dune_rules/install_rules.ml", line 871, characters 4-72
  Called from Dune_rules__Gen_rules.gen_rules in file
    "src/dune_rules/gen_rules.ml", line 295, characters 25-58
  Called from Stdune__Exn.protectx in file "src/stdune/exn.ml", line 12,
    characters 8-11
  Re-raised at Stdune__Exn.protectx in file "src/stdune/exn.ml", line 18,
    characters 4-11
  Called from Memo__Implicit_output.collect_sync in file
    "src/memo/implicit_output.ml", line 119, characters 4-162
  Called from Dune_engine__Rules.collect_opt in file "src/dune_engine/rules.ml"
    (inlined), line 192, characters 20-71
  Called from Dune_engine__Rules.collect in file "src/dune_engine/rules.ml",
    line 195, characters 20-33
  Called from Dune_engine__Build_system.Load_rules.load_dir_step2_exn in file
    "src/dune_engine/build_system.ml", line 895, characters 6-76
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_engine__Build_system.Load_rules.load_dir_impl in file
    "src/dune_engine/build_system.ml", line 1042, characters 12-43
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_engine__Build_system.Load_rules.targets_of in file
    "src/dune_engine/build_system.ml", line 684, characters 10-23
  Called from Dune_engine__Build_system.Load_rules.file_exists in file
    "src/dune_engine/build_system.ml", line 681, characters 17-34
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 305, characters 9-22
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 296, characters 58-73
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 296, characters 42-57
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 296, characters 42-57
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 294, characters 55-70
  Called from
    Dune_engine__Build_system.Exported.Build_request.evaluate_and_wait_for_dynamic_dependencies
    in file "src/dune_engine/build_system.ml", line 1230, characters 24-39
  Called from Dune_engine__Build_system.do_build.(fun) in file
    "src/dune_engine/build_system.ml", line 1845, characters 8-97
  Called from Fiber.Execution_context.safe_run_k in file "src/fiber/fiber.ml",
    line 129, characters 18-21
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Fiber.Execution_context.forward_exn_with_bt in file
    "src/fiber/fiber.ml", line 136, characters 10-17
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]
  % ls _build/install/default/lib/foo/*.a
  ls: _build/install/default/lib/foo/*.a: No such file or directory
  [1]
  
  # create a dummy executable to test
  -> creating dune
  -> creating b.ml
  
  # make sure that this library is usable locally
  % dune exec ./exe/b.exe
  exe working
  
  # make sure that this library is usable externally
  % rm -rf lib
  % OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  File "exe/dune", line 5, characters 12-15:
  5 |  (libraries foo))
                  ^^^
  Error: Library "foo" not found.
  Hint: try:
    dune external-lib-deps --missing --build-dir _b2 ./exe/b.exe
  [1]
  
  
  # mli_only_wrapped_no_stubs
  -> creating dune-project
  # build the library and see if .a is present
  -> creating dune
  -> creating foo.mli
  % dune build --root . @install
  Error: exception { exn =
      ("External.cm_dir",
      { t =
          { public_dir = In_build_dir "default/lib"
          ; private_dir = None
          ; public_cmi_dir = None
          }
      })
  ; backtrace =
      [ { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("stanzas-to-entries", "default")
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Memo.Lazy.force in file \"src/memo/memo.ml\"
  (inlined), line 1061, characters 16-20\n\
             Called from Memo.Lazy.bind.(fun) in file \"src/memo/memo.ml\",
  line 1067, characters 45-54\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Memo.Lazy.force in file \"src/memo/memo.ml\"
  (inlined), line 1061, characters 16-20\n\
             Called from Memo.Lazy.bind.(fun) in file \"src/memo/memo.ml\",
  line 1067, characters 45-54\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.get_rules in file
  \"src/dune_rules/scheme.ml\", line 103, characters 6-34\n\
             Called from Dune_rules__Install_rules.gen_rules in file
  \"src/dune_rules/install_rules.ml\", line 871, characters 4-72\n\
             Called from Dune_rules__Gen_rules.gen_rules in file
  \"src/dune_rules/gen_rules.ml\", line 295, characters 25-58\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from
  Dune_engine__Build_system.Load_rules.load_dir_step2_exn in file
  \"src/dune_engine/build_system.ml\", line 895, characters 6-76\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("load-dir", In_build_dir "default")
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Memo.Lazy.force in file \"src/memo/memo.ml\"
  (inlined), line 1061, characters 16-20\n\
             Called from Memo.Lazy.bind.(fun) in file \"src/memo/memo.ml\",
  line 1067, characters 45-54\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.get_rules in file
  \"src/dune_rules/scheme.ml\", line 103, characters 6-34\n\
             Called from Dune_rules__Install_rules.gen_rules in file
  \"src/dune_rules/install_rules.ml\", line 871, characters 4-72\n\
             Called from Dune_rules__Gen_rules.gen_rules in file
  \"src/dune_rules/gen_rules.ml\", line 295, characters 25-58\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from
  Dune_engine__Build_system.Load_rules.load_dir_step2_exn in file
  \"src/dune_engine/build_system.ml\", line 895, characters 6-76\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_engine__Build_system.Load_rules.load_dir_impl in
  file \"src/dune_engine/build_system.ml\", line 1042, characters 12-43\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("load-dir", In_build_dir ".aliases/default")
        }
      ]
  ; outer_call_stack = []
  }
  Raised at Stdune__Code_error.raise in file "src/stdune/code_error.ml", line
    9, characters 30-62
  Called from
    Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in
    file "src/dune_rules/install_rules.ml", line 149, characters 19-69
  Called from
    Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in
    file "src/dune_rules/install_rules.ml" (inlined), line 166, characters
    23-31
  Called from
    Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in
    file "src/dune_rules/install_rules.ml", line 181, characters 49-63
  Called from Stdlib__list.rev_map.rmap_f in file "list.ml", line 103,
    characters 22-25
  Called from Stdune__List.map in file "src/stdune/list.ml", line 5, characters
    19-33
  Called from Stdlib__list.rev_map.rmap_f in file "list.ml", line 103,
    characters 22-25
  Called from Stdune__List.map in file "src/stdune/list.ml" (inlined), line 5,
    characters 19-33
  Called from Stdune__List.concat_map in file "src/stdune/list.ml", line 40,
    characters 29-39
  Called from Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in
    file "src/dune_rules/install_rules.ml", line 156, characters 6-1023
  Called from Dune_rules__Dir_with_dune.inner_fold in file
    "src/dune_rules/dir_with_dune.ml", line 24, characters 55-67
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Install_rules.install_entries in file
    "src/dune_rules/install_rules.ml", line 699, characters 17-59
  Called from Dune_rules__Install_rules.install_rules in file
    "src/dune_rules/install_rules.ml", line 708, characters 4-32
  Called from Dune_rules__Install_rules.memo.(fun) in file
    "src/dune_rules/install_rules.ml", line 839, characters 16-38
  Called from Stdune__Exn.protectx in file "src/stdune/exn.ml", line 12,
    characters 8-11
  Re-raised at Stdune__Exn.protectx in file "src/stdune/exn.ml", line 18,
    characters 4-11
  Called from Memo__Implicit_output.collect_sync in file
    "src/memo/implicit_output.ml", line 119, characters 4-162
  Called from Dune_engine__Rules.collect_opt in file "src/dune_engine/rules.ml"
    (inlined), line 192, characters 20-71
  Called from Dune_engine__Rules.collect in file "src/dune_engine/rules.ml",
    line 195, characters 20-33
  Called from Dune_engine__Rules.collect_unit in file
    "src/dune_engine/rules.ml", line 199, characters 18-27
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Install_rules.memo.(fun) in file
    "src/dune_rules/install_rules.ml", line 847, characters 48-71
  Called from Dune_rules__Scheme.evaluate.loop in file
    "src/dune_rules/scheme.ml", line 138, characters 27-33
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
    "src/dune_rules/scheme.ml", line 76, characters 51-70
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Memo.Lazy.force in file "src/memo/memo.ml" (inlined), line 1061,
    characters 16-20
  Called from Memo.Lazy.bind.(fun) in file "src/memo/memo.ml", line 1067,
    characters 45-54
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Scheme.Evaluated.get_rules in file
    "src/dune_rules/scheme.ml", line 103, characters 6-34
  Called from Dune_rules__Install_rules.gen_rules in file
    "src/dune_rules/install_rules.ml", line 871, characters 4-72
  Called from Dune_rules__Gen_rules.gen_rules in file
    "src/dune_rules/gen_rules.ml", line 295, characters 25-58
  Called from Stdune__Exn.protectx in file "src/stdune/exn.ml", line 12,
    characters 8-11
  Re-raised at Stdune__Exn.protectx in file "src/stdune/exn.ml", line 18,
    characters 4-11
  Called from Memo__Implicit_output.collect_sync in file
    "src/memo/implicit_output.ml", line 119, characters 4-162
  Called from Dune_engine__Rules.collect_opt in file "src/dune_engine/rules.ml"
    (inlined), line 192, characters 20-71
  Called from Dune_engine__Rules.collect in file "src/dune_engine/rules.ml",
    line 195, characters 20-33
  Called from Dune_engine__Build_system.Load_rules.load_dir_step2_exn in file
    "src/dune_engine/build_system.ml", line 895, characters 6-76
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_engine__Build_system.Load_rules.load_dir_impl in file
    "src/dune_engine/build_system.ml", line 1042, characters 12-43
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_engine__Build_system.Load_rules.targets_of in file
    "src/dune_engine/build_system.ml", line 684, characters 10-23
  Called from Dune_engine__Build_system.Load_rules.file_exists in file
    "src/dune_engine/build_system.ml", line 681, characters 17-34
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 305, characters 9-22
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 296, characters 58-73
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 296, characters 42-57
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 296, characters 42-57
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 294, characters 55-70
  Called from
    Dune_engine__Build_system.Exported.Build_request.evaluate_and_wait_for_dynamic_dependencies
    in file "src/dune_engine/build_system.ml", line 1230, characters 24-39
  Called from Dune_engine__Build_system.do_build.(fun) in file
    "src/dune_engine/build_system.ml", line 1845, characters 8-97
  Called from Fiber.Execution_context.safe_run_k in file "src/fiber/fiber.ml",
    line 129, characters 18-21
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Fiber.Execution_context.forward_exn_with_bt in file
    "src/fiber/fiber.ml", line 136, characters 10-17
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]
  % ls _build/install/default/lib/foo/*.a
  ls: _build/install/default/lib/foo/*.a: No such file or directory
  [1]
  
  # create a dummy executable to test
  -> creating dune
  -> creating b.ml
  
  # make sure that this library is usable locally
  % dune exec ./exe/b.exe
  exe working
  
  # make sure that this library is usable externally
  % rm -rf lib
  % OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  File "exe/dune", line 5, characters 12-15:
  5 |  (libraries foo))
                  ^^^
  Error: Library "foo" not found.
  Hint: try:
    dune external-lib-deps --missing --build-dir _b2 ./exe/b.exe
  [1]
  
  
  # mli_only_unwrapped_stubs
  -> creating dune-project
  # build the library and see if .a is present
  -> creating dune
  -> creating stub.c
  -> creating foo.mli
  % dune build --root . @install
  Error: exception { exn =
      ("External.cm_dir",
      { t =
          { public_dir = In_build_dir "default/lib"
          ; private_dir = None
          ; public_cmi_dir = None
          }
      })
  ; backtrace =
      [ { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("stanzas-to-entries", "default")
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Memo.Lazy.force in file \"src/memo/memo.ml\"
  (inlined), line 1061, characters 16-20\n\
             Called from Memo.Lazy.bind.(fun) in file \"src/memo/memo.ml\",
  line 1067, characters 45-54\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Memo.Lazy.force in file \"src/memo/memo.ml\"
  (inlined), line 1061, characters 16-20\n\
             Called from Memo.Lazy.bind.(fun) in file \"src/memo/memo.ml\",
  line 1067, characters 45-54\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.get_rules in file
  \"src/dune_rules/scheme.ml\", line 103, characters 6-34\n\
             Called from Dune_rules__Install_rules.gen_rules in file
  \"src/dune_rules/install_rules.ml\", line 871, characters 4-72\n\
             Called from Dune_rules__Gen_rules.gen_rules in file
  \"src/dune_rules/gen_rules.ml\", line 295, characters 25-58\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from
  Dune_engine__Build_system.Load_rules.load_dir_step2_exn in file
  \"src/dune_engine/build_system.ml\", line 895, characters 6-76\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("load-dir", In_build_dir "default")
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Memo.Lazy.force in file \"src/memo/memo.ml\"
  (inlined), line 1061, characters 16-20\n\
             Called from Memo.Lazy.bind.(fun) in file \"src/memo/memo.ml\",
  line 1067, characters 45-54\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.get_rules in file
  \"src/dune_rules/scheme.ml\", line 103, characters 6-34\n\
             Called from Dune_rules__Install_rules.gen_rules in file
  \"src/dune_rules/install_rules.ml\", line 871, characters 4-72\n\
             Called from Dune_rules__Gen_rules.gen_rules in file
  \"src/dune_rules/gen_rules.ml\", line 295, characters 25-58\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from
  Dune_engine__Build_system.Load_rules.load_dir_step2_exn in file
  \"src/dune_engine/build_system.ml\", line 895, characters 6-76\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_engine__Build_system.Load_rules.load_dir_impl in
  file \"src/dune_engine/build_system.ml\", line 1042, characters 12-43\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("load-dir", In_build_dir ".aliases/default")
        }
      ]
  ; outer_call_stack = []
  }
  Raised at Stdune__Code_error.raise in file "src/stdune/code_error.ml", line
    9, characters 30-62
  Called from
    Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in
    file "src/dune_rules/install_rules.ml", line 149, characters 19-69
  Called from
    Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in
    file "src/dune_rules/install_rules.ml" (inlined), line 166, characters
    23-31
  Called from
    Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in
    file "src/dune_rules/install_rules.ml", line 181, characters 49-63
  Called from Stdlib__list.rev_map.rmap_f in file "list.ml", line 103,
    characters 22-25
  Called from Stdune__List.map in file "src/stdune/list.ml", line 5, characters
    19-33
  Called from Stdlib__list.rev_map.rmap_f in file "list.ml", line 103,
    characters 22-25
  Called from Stdune__List.map in file "src/stdune/list.ml" (inlined), line 5,
    characters 19-33
  Called from Stdune__List.concat_map in file "src/stdune/list.ml", line 40,
    characters 29-39
  Called from Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in
    file "src/dune_rules/install_rules.ml", line 156, characters 6-1023
  Called from Dune_rules__Dir_with_dune.inner_fold in file
    "src/dune_rules/dir_with_dune.ml", line 24, characters 55-67
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Install_rules.install_entries in file
    "src/dune_rules/install_rules.ml", line 699, characters 17-59
  Called from Dune_rules__Install_rules.install_rules in file
    "src/dune_rules/install_rules.ml", line 708, characters 4-32
  Called from Dune_rules__Install_rules.memo.(fun) in file
    "src/dune_rules/install_rules.ml", line 839, characters 16-38
  Called from Stdune__Exn.protectx in file "src/stdune/exn.ml", line 12,
    characters 8-11
  Re-raised at Stdune__Exn.protectx in file "src/stdune/exn.ml", line 18,
    characters 4-11
  Called from Memo__Implicit_output.collect_sync in file
    "src/memo/implicit_output.ml", line 119, characters 4-162
  Called from Dune_engine__Rules.collect_opt in file "src/dune_engine/rules.ml"
    (inlined), line 192, characters 20-71
  Called from Dune_engine__Rules.collect in file "src/dune_engine/rules.ml",
    line 195, characters 20-33
  Called from Dune_engine__Rules.collect_unit in file
    "src/dune_engine/rules.ml", line 199, characters 18-27
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Install_rules.memo.(fun) in file
    "src/dune_rules/install_rules.ml", line 847, characters 48-71
  Called from Dune_rules__Scheme.evaluate.loop in file
    "src/dune_rules/scheme.ml", line 138, characters 27-33
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
    "src/dune_rules/scheme.ml", line 76, characters 51-70
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Memo.Lazy.force in file "src/memo/memo.ml" (inlined), line 1061,
    characters 16-20
  Called from Memo.Lazy.bind.(fun) in file "src/memo/memo.ml", line 1067,
    characters 45-54
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Scheme.Evaluated.get_rules in file
    "src/dune_rules/scheme.ml", line 103, characters 6-34
  Called from Dune_rules__Install_rules.gen_rules in file
    "src/dune_rules/install_rules.ml", line 871, characters 4-72
  Called from Dune_rules__Gen_rules.gen_rules in file
    "src/dune_rules/gen_rules.ml", line 295, characters 25-58
  Called from Stdune__Exn.protectx in file "src/stdune/exn.ml", line 12,
    characters 8-11
  Re-raised at Stdune__Exn.protectx in file "src/stdune/exn.ml", line 18,
    characters 4-11
  Called from Memo__Implicit_output.collect_sync in file
    "src/memo/implicit_output.ml", line 119, characters 4-162
  Called from Dune_engine__Rules.collect_opt in file "src/dune_engine/rules.ml"
    (inlined), line 192, characters 20-71
  Called from Dune_engine__Rules.collect in file "src/dune_engine/rules.ml",
    line 195, characters 20-33
  Called from Dune_engine__Build_system.Load_rules.load_dir_step2_exn in file
    "src/dune_engine/build_system.ml", line 895, characters 6-76
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_engine__Build_system.Load_rules.load_dir_impl in file
    "src/dune_engine/build_system.ml", line 1042, characters 12-43
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_engine__Build_system.Load_rules.targets_of in file
    "src/dune_engine/build_system.ml", line 684, characters 10-23
  Called from Dune_engine__Build_system.Load_rules.file_exists in file
    "src/dune_engine/build_system.ml", line 681, characters 17-34
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 305, characters 9-22
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 296, characters 58-73
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 296, characters 42-57
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 296, characters 42-57
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 294, characters 55-70
  Called from
    Dune_engine__Build_system.Exported.Build_request.evaluate_and_wait_for_dynamic_dependencies
    in file "src/dune_engine/build_system.ml", line 1230, characters 24-39
  Called from Dune_engine__Build_system.do_build.(fun) in file
    "src/dune_engine/build_system.ml", line 1845, characters 8-97
  Called from Fiber.Execution_context.safe_run_k in file "src/fiber/fiber.ml",
    line 129, characters 18-21
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Fiber.Execution_context.forward_exn_with_bt in file
    "src/fiber/fiber.ml", line 136, characters 10-17
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]
  % ls _build/install/default/lib/foo/*.a
  ls: _build/install/default/lib/foo/*.a: No such file or directory
  [1]
  
  # create a dummy executable to test
  -> creating dune
  -> creating b.ml
  
  # make sure that this library is usable locally
  % dune exec ./exe/b.exe
  exe working
  
  # make sure that this library is usable externally
  % rm -rf lib
  % OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  File "exe/dune", line 5, characters 12-15:
  5 |  (libraries foo))
                  ^^^
  Error: Library "foo" not found.
  Hint: try:
    dune external-lib-deps --missing --build-dir _b2 ./exe/b.exe
  [1]
  
  
  # mli_only_unwrapped_no_stubs
  -> creating dune-project
  # build the library and see if .a is present
  -> creating dune
  -> creating foo.mli
  % dune build --root . @install
  Error: exception { exn =
      ("External.cm_dir",
      { t =
          { public_dir = In_build_dir "default/lib"
          ; private_dir = None
          ; public_cmi_dir = None
          }
      })
  ; backtrace =
      [ { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("stanzas-to-entries", "default")
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Memo.Lazy.force in file \"src/memo/memo.ml\"
  (inlined), line 1061, characters 16-20\n\
             Called from Memo.Lazy.bind.(fun) in file \"src/memo/memo.ml\",
  line 1067, characters 45-54\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Memo.Lazy.force in file \"src/memo/memo.ml\"
  (inlined), line 1061, characters 16-20\n\
             Called from Memo.Lazy.bind.(fun) in file \"src/memo/memo.ml\",
  line 1067, characters 45-54\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.get_rules in file
  \"src/dune_rules/scheme.ml\", line 103, characters 6-34\n\
             Called from Dune_rules__Install_rules.gen_rules in file
  \"src/dune_rules/install_rules.ml\", line 871, characters 4-72\n\
             Called from Dune_rules__Gen_rules.gen_rules in file
  \"src/dune_rules/gen_rules.ml\", line 295, characters 25-58\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from
  Dune_engine__Build_system.Load_rules.load_dir_step2_exn in file
  \"src/dune_engine/build_system.ml\", line 895, characters 6-76\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("load-dir", In_build_dir "default")
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Memo.Lazy.force in file \"src/memo/memo.ml\"
  (inlined), line 1061, characters 16-20\n\
             Called from Memo.Lazy.bind.(fun) in file \"src/memo/memo.ml\",
  line 1067, characters 45-54\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.get_rules in file
  \"src/dune_rules/scheme.ml\", line 103, characters 6-34\n\
             Called from Dune_rules__Install_rules.gen_rules in file
  \"src/dune_rules/install_rules.ml\", line 871, characters 4-72\n\
             Called from Dune_rules__Gen_rules.gen_rules in file
  \"src/dune_rules/gen_rules.ml\", line 295, characters 25-58\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from
  Dune_engine__Build_system.Load_rules.load_dir_step2_exn in file
  \"src/dune_engine/build_system.ml\", line 895, characters 6-76\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_engine__Build_system.Load_rules.load_dir_impl in
  file \"src/dune_engine/build_system.ml\", line 1042, characters 12-43\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("load-dir", In_build_dir ".aliases/default")
        }
      ]
  ; outer_call_stack = []
  }
  Raised at Stdune__Code_error.raise in file "src/stdune/code_error.ml", line
    9, characters 30-62
  Called from
    Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in
    file "src/dune_rules/install_rules.ml", line 149, characters 19-69
  Called from
    Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in
    file "src/dune_rules/install_rules.ml" (inlined), line 166, characters
    23-31
  Called from
    Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in
    file "src/dune_rules/install_rules.ml", line 181, characters 49-63
  Called from Stdlib__list.rev_map.rmap_f in file "list.ml", line 103,
    characters 22-25
  Called from Stdune__List.map in file "src/stdune/list.ml", line 5, characters
    19-33
  Called from Stdlib__list.rev_map.rmap_f in file "list.ml", line 103,
    characters 22-25
  Called from Stdune__List.map in file "src/stdune/list.ml" (inlined), line 5,
    characters 19-33
  Called from Stdune__List.concat_map in file "src/stdune/list.ml", line 40,
    characters 29-39
  Called from Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in
    file "src/dune_rules/install_rules.ml", line 156, characters 6-1023
  Called from Dune_rules__Dir_with_dune.inner_fold in file
    "src/dune_rules/dir_with_dune.ml", line 24, characters 55-67
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Install_rules.install_entries in file
    "src/dune_rules/install_rules.ml", line 699, characters 17-59
  Called from Dune_rules__Install_rules.install_rules in file
    "src/dune_rules/install_rules.ml", line 708, characters 4-32
  Called from Dune_rules__Install_rules.memo.(fun) in file
    "src/dune_rules/install_rules.ml", line 839, characters 16-38
  Called from Stdune__Exn.protectx in file "src/stdune/exn.ml", line 12,
    characters 8-11
  Re-raised at Stdune__Exn.protectx in file "src/stdune/exn.ml", line 18,
    characters 4-11
  Called from Memo__Implicit_output.collect_sync in file
    "src/memo/implicit_output.ml", line 119, characters 4-162
  Called from Dune_engine__Rules.collect_opt in file "src/dune_engine/rules.ml"
    (inlined), line 192, characters 20-71
  Called from Dune_engine__Rules.collect in file "src/dune_engine/rules.ml",
    line 195, characters 20-33
  Called from Dune_engine__Rules.collect_unit in file
    "src/dune_engine/rules.ml", line 199, characters 18-27
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Install_rules.memo.(fun) in file
    "src/dune_rules/install_rules.ml", line 847, characters 48-71
  Called from Dune_rules__Scheme.evaluate.loop in file
    "src/dune_rules/scheme.ml", line 138, characters 27-33
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
    "src/dune_rules/scheme.ml", line 76, characters 51-70
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Memo.Lazy.force in file "src/memo/memo.ml" (inlined), line 1061,
    characters 16-20
  Called from Memo.Lazy.bind.(fun) in file "src/memo/memo.ml", line 1067,
    characters 45-54
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Scheme.Evaluated.get_rules in file
    "src/dune_rules/scheme.ml", line 103, characters 6-34
  Called from Dune_rules__Install_rules.gen_rules in file
    "src/dune_rules/install_rules.ml", line 871, characters 4-72
  Called from Dune_rules__Gen_rules.gen_rules in file
    "src/dune_rules/gen_rules.ml", line 295, characters 25-58
  Called from Stdune__Exn.protectx in file "src/stdune/exn.ml", line 12,
    characters 8-11
  Re-raised at Stdune__Exn.protectx in file "src/stdune/exn.ml", line 18,
    characters 4-11
  Called from Memo__Implicit_output.collect_sync in file
    "src/memo/implicit_output.ml", line 119, characters 4-162
  Called from Dune_engine__Rules.collect_opt in file "src/dune_engine/rules.ml"
    (inlined), line 192, characters 20-71
  Called from Dune_engine__Rules.collect in file "src/dune_engine/rules.ml",
    line 195, characters 20-33
  Called from Dune_engine__Build_system.Load_rules.load_dir_step2_exn in file
    "src/dune_engine/build_system.ml", line 895, characters 6-76
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_engine__Build_system.Load_rules.load_dir_impl in file
    "src/dune_engine/build_system.ml", line 1042, characters 12-43
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_engine__Build_system.Load_rules.targets_of in file
    "src/dune_engine/build_system.ml", line 684, characters 10-23
  Called from Dune_engine__Build_system.Load_rules.file_exists in file
    "src/dune_engine/build_system.ml", line 681, characters 17-34
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 305, characters 9-22
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 296, characters 58-73
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 296, characters 42-57
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 296, characters 42-57
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 294, characters 55-70
  Called from
    Dune_engine__Build_system.Exported.Build_request.evaluate_and_wait_for_dynamic_dependencies
    in file "src/dune_engine/build_system.ml", line 1230, characters 24-39
  Called from Dune_engine__Build_system.do_build.(fun) in file
    "src/dune_engine/build_system.ml", line 1845, characters 8-97
  Called from Fiber.Execution_context.safe_run_k in file "src/fiber/fiber.ml",
    line 129, characters 18-21
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Fiber.Execution_context.forward_exn_with_bt in file
    "src/fiber/fiber.ml", line 136, characters 10-17
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]
  % ls _build/install/default/lib/foo/*.a
  ls: _build/install/default/lib/foo/*.a: No such file or directory
  [1]
  
  # create a dummy executable to test
  -> creating dune
  -> creating b.ml
  
  # make sure that this library is usable locally
  % dune exec ./exe/b.exe
  exe working
  
  # make sure that this library is usable externally
  % rm -rf lib
  % OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  File "exe/dune", line 5, characters 12-15:
  5 |  (libraries foo))
                  ^^^
  Error: Library "foo" not found.
  Hint: try:
    dune external-lib-deps --missing --build-dir _b2 ./exe/b.exe
  [1]
  
  
  # no_mli_wrapped_stubs
  -> creating dune-project
  # build the library and see if .a is present
  -> creating dune
  -> creating stub.c
  % dune build --root . @install
  Error: exception { exn =
      ("External.cm_dir",
      { t =
          { public_dir = In_build_dir "default/lib"
          ; private_dir = None
          ; public_cmi_dir = None
          }
      })
  ; backtrace =
      [ { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("stanzas-to-entries", "default")
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Memo.Lazy.force in file \"src/memo/memo.ml\"
  (inlined), line 1061, characters 16-20\n\
             Called from Memo.Lazy.bind.(fun) in file \"src/memo/memo.ml\",
  line 1067, characters 45-54\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Memo.Lazy.force in file \"src/memo/memo.ml\"
  (inlined), line 1061, characters 16-20\n\
             Called from Memo.Lazy.bind.(fun) in file \"src/memo/memo.ml\",
  line 1067, characters 45-54\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.get_rules in file
  \"src/dune_rules/scheme.ml\", line 103, characters 6-34\n\
             Called from Dune_rules__Install_rules.gen_rules in file
  \"src/dune_rules/install_rules.ml\", line 871, characters 4-72\n\
             Called from Dune_rules__Gen_rules.gen_rules in file
  \"src/dune_rules/gen_rules.ml\", line 295, characters 25-58\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from
  Dune_engine__Build_system.Load_rules.load_dir_step2_exn in file
  \"src/dune_engine/build_system.ml\", line 895, characters 6-76\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("load-dir", In_build_dir "default")
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Memo.Lazy.force in file \"src/memo/memo.ml\"
  (inlined), line 1061, characters 16-20\n\
             Called from Memo.Lazy.bind.(fun) in file \"src/memo/memo.ml\",
  line 1067, characters 45-54\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.get_rules in file
  \"src/dune_rules/scheme.ml\", line 103, characters 6-34\n\
             Called from Dune_rules__Install_rules.gen_rules in file
  \"src/dune_rules/install_rules.ml\", line 871, characters 4-72\n\
             Called from Dune_rules__Gen_rules.gen_rules in file
  \"src/dune_rules/gen_rules.ml\", line 295, characters 25-58\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from
  Dune_engine__Build_system.Load_rules.load_dir_step2_exn in file
  \"src/dune_engine/build_system.ml\", line 895, characters 6-76\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_engine__Build_system.Load_rules.load_dir_impl in
  file \"src/dune_engine/build_system.ml\", line 1042, characters 12-43\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("load-dir", In_build_dir ".aliases/default")
        }
      ]
  ; outer_call_stack = []
  }
  Raised at Stdune__Code_error.raise in file "src/stdune/code_error.ml", line
    9, characters 30-62
  Called from
    Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in
    file "src/dune_rules/install_rules.ml", line 149, characters 19-69
  Called from
    Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in
    file "src/dune_rules/install_rules.ml" (inlined), line 166, characters
    23-31
  Called from
    Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in
    file "src/dune_rules/install_rules.ml", line 181, characters 49-63
  Called from Stdlib__list.rev_map.rmap_f in file "list.ml", line 103,
    characters 22-25
  Called from Stdune__List.map in file "src/stdune/list.ml", line 5, characters
    19-33
  Called from Stdlib__list.rev_map.rmap_f in file "list.ml", line 103,
    characters 22-25
  Called from Stdune__List.map in file "src/stdune/list.ml" (inlined), line 5,
    characters 19-33
  Called from Stdune__List.concat_map in file "src/stdune/list.ml", line 40,
    characters 29-39
  Called from Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in
    file "src/dune_rules/install_rules.ml", line 156, characters 6-1023
  Called from Dune_rules__Dir_with_dune.inner_fold in file
    "src/dune_rules/dir_with_dune.ml", line 24, characters 55-67
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Install_rules.install_entries in file
    "src/dune_rules/install_rules.ml", line 699, characters 17-59
  Called from Dune_rules__Install_rules.install_rules in file
    "src/dune_rules/install_rules.ml", line 708, characters 4-32
  Called from Dune_rules__Install_rules.memo.(fun) in file
    "src/dune_rules/install_rules.ml", line 839, characters 16-38
  Called from Stdune__Exn.protectx in file "src/stdune/exn.ml", line 12,
    characters 8-11
  Re-raised at Stdune__Exn.protectx in file "src/stdune/exn.ml", line 18,
    characters 4-11
  Called from Memo__Implicit_output.collect_sync in file
    "src/memo/implicit_output.ml", line 119, characters 4-162
  Called from Dune_engine__Rules.collect_opt in file "src/dune_engine/rules.ml"
    (inlined), line 192, characters 20-71
  Called from Dune_engine__Rules.collect in file "src/dune_engine/rules.ml",
    line 195, characters 20-33
  Called from Dune_engine__Rules.collect_unit in file
    "src/dune_engine/rules.ml", line 199, characters 18-27
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Install_rules.memo.(fun) in file
    "src/dune_rules/install_rules.ml", line 847, characters 48-71
  Called from Dune_rules__Scheme.evaluate.loop in file
    "src/dune_rules/scheme.ml", line 138, characters 27-33
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
    "src/dune_rules/scheme.ml", line 76, characters 51-70
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Memo.Lazy.force in file "src/memo/memo.ml" (inlined), line 1061,
    characters 16-20
  Called from Memo.Lazy.bind.(fun) in file "src/memo/memo.ml", line 1067,
    characters 45-54
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Scheme.Evaluated.get_rules in file
    "src/dune_rules/scheme.ml", line 103, characters 6-34
  Called from Dune_rules__Install_rules.gen_rules in file
    "src/dune_rules/install_rules.ml", line 871, characters 4-72
  Called from Dune_rules__Gen_rules.gen_rules in file
    "src/dune_rules/gen_rules.ml", line 295, characters 25-58
  Called from Stdune__Exn.protectx in file "src/stdune/exn.ml", line 12,
    characters 8-11
  Re-raised at Stdune__Exn.protectx in file "src/stdune/exn.ml", line 18,
    characters 4-11
  Called from Memo__Implicit_output.collect_sync in file
    "src/memo/implicit_output.ml", line 119, characters 4-162
  Called from Dune_engine__Rules.collect_opt in file "src/dune_engine/rules.ml"
    (inlined), line 192, characters 20-71
  Called from Dune_engine__Rules.collect in file "src/dune_engine/rules.ml",
    line 195, characters 20-33
  Called from Dune_engine__Build_system.Load_rules.load_dir_step2_exn in file
    "src/dune_engine/build_system.ml", line 895, characters 6-76
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_engine__Build_system.Load_rules.load_dir_impl in file
    "src/dune_engine/build_system.ml", line 1042, characters 12-43
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_engine__Build_system.Load_rules.targets_of in file
    "src/dune_engine/build_system.ml", line 684, characters 10-23
  Called from Dune_engine__Build_system.Load_rules.file_exists in file
    "src/dune_engine/build_system.ml", line 681, characters 17-34
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 305, characters 9-22
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 296, characters 58-73
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 296, characters 42-57
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 296, characters 42-57
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 294, characters 55-70
  Called from
    Dune_engine__Build_system.Exported.Build_request.evaluate_and_wait_for_dynamic_dependencies
    in file "src/dune_engine/build_system.ml", line 1230, characters 24-39
  Called from Dune_engine__Build_system.do_build.(fun) in file
    "src/dune_engine/build_system.ml", line 1845, characters 8-97
  Called from Fiber.Execution_context.safe_run_k in file "src/fiber/fiber.ml",
    line 129, characters 18-21
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Fiber.Execution_context.forward_exn_with_bt in file
    "src/fiber/fiber.ml", line 136, characters 10-17
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]
  % ls _build/install/default/lib/foo/*.a
  ls: _build/install/default/lib/foo/*.a: No such file or directory
  [1]
  
  # create a dummy executable to test
  -> creating dune
  -> creating b.ml
  
  # make sure that this library is usable locally
  % dune exec ./exe/b.exe
  exe working
  
  # make sure that this library is usable externally
  % rm -rf lib
  % OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  File "exe/dune", line 5, characters 12-15:
  5 |  (libraries foo))
                  ^^^
  Error: Library "foo" not found.
  Hint: try:
    dune external-lib-deps --missing --build-dir _b2 ./exe/b.exe
  [1]
  
  
  # no_mli_wrapped_no_stubs
  -> creating dune-project
  # build the library and see if .a is present
  -> creating dune
  % dune build --root . @install
  Error: exception { exn =
      ("External.cm_dir",
      { t =
          { public_dir = In_build_dir "default/lib"
          ; private_dir = None
          ; public_cmi_dir = None
          }
      })
  ; backtrace =
      [ { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("stanzas-to-entries", "default")
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Memo.Lazy.force in file \"src/memo/memo.ml\"
  (inlined), line 1061, characters 16-20\n\
             Called from Memo.Lazy.bind.(fun) in file \"src/memo/memo.ml\",
  line 1067, characters 45-54\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Memo.Lazy.force in file \"src/memo/memo.ml\"
  (inlined), line 1061, characters 16-20\n\
             Called from Memo.Lazy.bind.(fun) in file \"src/memo/memo.ml\",
  line 1067, characters 45-54\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.get_rules in file
  \"src/dune_rules/scheme.ml\", line 103, characters 6-34\n\
             Called from Dune_rules__Install_rules.gen_rules in file
  \"src/dune_rules/install_rules.ml\", line 871, characters 4-72\n\
             Called from Dune_rules__Gen_rules.gen_rules in file
  \"src/dune_rules/gen_rules.ml\", line 295, characters 25-58\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from
  Dune_engine__Build_system.Load_rules.load_dir_step2_exn in file
  \"src/dune_engine/build_system.ml\", line 895, characters 6-76\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("load-dir", In_build_dir "default")
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Memo.Lazy.force in file \"src/memo/memo.ml\"
  (inlined), line 1061, characters 16-20\n\
             Called from Memo.Lazy.bind.(fun) in file \"src/memo/memo.ml\",
  line 1067, characters 45-54\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.get_rules in file
  \"src/dune_rules/scheme.ml\", line 103, characters 6-34\n\
             Called from Dune_rules__Install_rules.gen_rules in file
  \"src/dune_rules/install_rules.ml\", line 871, characters 4-72\n\
             Called from Dune_rules__Gen_rules.gen_rules in file
  \"src/dune_rules/gen_rules.ml\", line 295, characters 25-58\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from
  Dune_engine__Build_system.Load_rules.load_dir_step2_exn in file
  \"src/dune_engine/build_system.ml\", line 895, characters 6-76\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_engine__Build_system.Load_rules.load_dir_impl in
  file \"src/dune_engine/build_system.ml\", line 1042, characters 12-43\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("load-dir", In_build_dir ".aliases/default")
        }
      ]
  ; outer_call_stack = []
  }
  Raised at Stdune__Code_error.raise in file "src/stdune/code_error.ml", line
    9, characters 30-62
  Called from
    Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in
    file "src/dune_rules/install_rules.ml", line 149, characters 19-69
  Called from
    Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in
    file "src/dune_rules/install_rules.ml" (inlined), line 166, characters
    23-31
  Called from
    Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in
    file "src/dune_rules/install_rules.ml", line 181, characters 49-63
  Called from Stdlib__list.rev_map.rmap_f in file "list.ml", line 103,
    characters 22-25
  Called from Stdune__List.map in file "src/stdune/list.ml", line 5, characters
    19-33
  Called from Stdlib__list.rev_map.rmap_f in file "list.ml", line 103,
    characters 22-25
  Called from Stdune__List.map in file "src/stdune/list.ml" (inlined), line 5,
    characters 19-33
  Called from Stdune__List.concat_map in file "src/stdune/list.ml", line 40,
    characters 29-39
  Called from Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in
    file "src/dune_rules/install_rules.ml", line 156, characters 6-1023
  Called from Dune_rules__Dir_with_dune.inner_fold in file
    "src/dune_rules/dir_with_dune.ml", line 24, characters 55-67
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Install_rules.install_entries in file
    "src/dune_rules/install_rules.ml", line 699, characters 17-59
  Called from Dune_rules__Install_rules.install_rules in file
    "src/dune_rules/install_rules.ml", line 708, characters 4-32
  Called from Dune_rules__Install_rules.memo.(fun) in file
    "src/dune_rules/install_rules.ml", line 839, characters 16-38
  Called from Stdune__Exn.protectx in file "src/stdune/exn.ml", line 12,
    characters 8-11
  Re-raised at Stdune__Exn.protectx in file "src/stdune/exn.ml", line 18,
    characters 4-11
  Called from Memo__Implicit_output.collect_sync in file
    "src/memo/implicit_output.ml", line 119, characters 4-162
  Called from Dune_engine__Rules.collect_opt in file "src/dune_engine/rules.ml"
    (inlined), line 192, characters 20-71
  Called from Dune_engine__Rules.collect in file "src/dune_engine/rules.ml",
    line 195, characters 20-33
  Called from Dune_engine__Rules.collect_unit in file
    "src/dune_engine/rules.ml", line 199, characters 18-27
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Install_rules.memo.(fun) in file
    "src/dune_rules/install_rules.ml", line 847, characters 48-71
  Called from Dune_rules__Scheme.evaluate.loop in file
    "src/dune_rules/scheme.ml", line 138, characters 27-33
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
    "src/dune_rules/scheme.ml", line 76, characters 51-70
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Memo.Lazy.force in file "src/memo/memo.ml" (inlined), line 1061,
    characters 16-20
  Called from Memo.Lazy.bind.(fun) in file "src/memo/memo.ml", line 1067,
    characters 45-54
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Scheme.Evaluated.get_rules in file
    "src/dune_rules/scheme.ml", line 103, characters 6-34
  Called from Dune_rules__Install_rules.gen_rules in file
    "src/dune_rules/install_rules.ml", line 871, characters 4-72
  Called from Dune_rules__Gen_rules.gen_rules in file
    "src/dune_rules/gen_rules.ml", line 295, characters 25-58
  Called from Stdune__Exn.protectx in file "src/stdune/exn.ml", line 12,
    characters 8-11
  Re-raised at Stdune__Exn.protectx in file "src/stdune/exn.ml", line 18,
    characters 4-11
  Called from Memo__Implicit_output.collect_sync in file
    "src/memo/implicit_output.ml", line 119, characters 4-162
  Called from Dune_engine__Rules.collect_opt in file "src/dune_engine/rules.ml"
    (inlined), line 192, characters 20-71
  Called from Dune_engine__Rules.collect in file "src/dune_engine/rules.ml",
    line 195, characters 20-33
  Called from Dune_engine__Build_system.Load_rules.load_dir_step2_exn in file
    "src/dune_engine/build_system.ml", line 895, characters 6-76
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_engine__Build_system.Load_rules.load_dir_impl in file
    "src/dune_engine/build_system.ml", line 1042, characters 12-43
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_engine__Build_system.Load_rules.targets_of in file
    "src/dune_engine/build_system.ml", line 684, characters 10-23
  Called from Dune_engine__Build_system.Load_rules.file_exists in file
    "src/dune_engine/build_system.ml", line 681, characters 17-34
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 305, characters 9-22
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 296, characters 58-73
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 296, characters 42-57
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 296, characters 42-57
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 294, characters 55-70
  Called from
    Dune_engine__Build_system.Exported.Build_request.evaluate_and_wait_for_dynamic_dependencies
    in file "src/dune_engine/build_system.ml", line 1230, characters 24-39
  Called from Dune_engine__Build_system.do_build.(fun) in file
    "src/dune_engine/build_system.ml", line 1845, characters 8-97
  Called from Fiber.Execution_context.safe_run_k in file "src/fiber/fiber.ml",
    line 129, characters 18-21
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Fiber.Execution_context.forward_exn_with_bt in file
    "src/fiber/fiber.ml", line 136, characters 10-17
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]
  % ls _build/install/default/lib/foo/*.a
  ls: _build/install/default/lib/foo/*.a: No such file or directory
  [1]
  
  # create a dummy executable to test
  -> creating dune
  -> creating b.ml
  
  # make sure that this library is usable locally
  % dune exec ./exe/b.exe
  exe working
  
  # make sure that this library is usable externally
  % rm -rf lib
  % OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  File "exe/dune", line 5, characters 12-15:
  5 |  (libraries foo))
                  ^^^
  Error: Library "foo" not found.
  Hint: try:
    dune external-lib-deps --missing --build-dir _b2 ./exe/b.exe
  [1]
  
  
  # no_mli_unwrapped_stubs
  -> creating dune-project
  # build the library and see if .a is present
  -> creating dune
  -> creating stub.c
  % dune build --root . @install
  Error: exception { exn =
      ("External.cm_dir",
      { t =
          { public_dir = In_build_dir "default/lib"
          ; private_dir = None
          ; public_cmi_dir = None
          }
      })
  ; backtrace =
      [ { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("stanzas-to-entries", "default")
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Memo.Lazy.force in file \"src/memo/memo.ml\"
  (inlined), line 1061, characters 16-20\n\
             Called from Memo.Lazy.bind.(fun) in file \"src/memo/memo.ml\",
  line 1067, characters 45-54\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Memo.Lazy.force in file \"src/memo/memo.ml\"
  (inlined), line 1061, characters 16-20\n\
             Called from Memo.Lazy.bind.(fun) in file \"src/memo/memo.ml\",
  line 1067, characters 45-54\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.get_rules in file
  \"src/dune_rules/scheme.ml\", line 103, characters 6-34\n\
             Called from Dune_rules__Install_rules.gen_rules in file
  \"src/dune_rules/install_rules.ml\", line 871, characters 4-72\n\
             Called from Dune_rules__Gen_rules.gen_rules in file
  \"src/dune_rules/gen_rules.ml\", line 295, characters 25-58\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from
  Dune_engine__Build_system.Load_rules.load_dir_step2_exn in file
  \"src/dune_engine/build_system.ml\", line 895, characters 6-76\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("load-dir", In_build_dir "default")
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Memo.Lazy.force in file \"src/memo/memo.ml\"
  (inlined), line 1061, characters 16-20\n\
             Called from Memo.Lazy.bind.(fun) in file \"src/memo/memo.ml\",
  line 1067, characters 45-54\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.get_rules in file
  \"src/dune_rules/scheme.ml\", line 103, characters 6-34\n\
             Called from Dune_rules__Install_rules.gen_rules in file
  \"src/dune_rules/install_rules.ml\", line 871, characters 4-72\n\
             Called from Dune_rules__Gen_rules.gen_rules in file
  \"src/dune_rules/gen_rules.ml\", line 295, characters 25-58\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from
  Dune_engine__Build_system.Load_rules.load_dir_step2_exn in file
  \"src/dune_engine/build_system.ml\", line 895, characters 6-76\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_engine__Build_system.Load_rules.load_dir_impl in
  file \"src/dune_engine/build_system.ml\", line 1042, characters 12-43\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("load-dir", In_build_dir ".aliases/default")
        }
      ]
  ; outer_call_stack = []
  }
  Raised at Stdune__Code_error.raise in file "src/stdune/code_error.ml", line
    9, characters 30-62
  Called from
    Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in
    file "src/dune_rules/install_rules.ml", line 149, characters 19-69
  Called from
    Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in
    file "src/dune_rules/install_rules.ml" (inlined), line 166, characters
    23-31
  Called from
    Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in
    file "src/dune_rules/install_rules.ml", line 181, characters 49-63
  Called from Stdlib__list.rev_map.rmap_f in file "list.ml", line 103,
    characters 22-25
  Called from Stdune__List.map in file "src/stdune/list.ml", line 5, characters
    19-33
  Called from Stdlib__list.rev_map.rmap_f in file "list.ml", line 103,
    characters 22-25
  Called from Stdune__List.map in file "src/stdune/list.ml" (inlined), line 5,
    characters 19-33
  Called from Stdune__List.concat_map in file "src/stdune/list.ml", line 40,
    characters 29-39
  Called from Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in
    file "src/dune_rules/install_rules.ml", line 156, characters 6-1023
  Called from Dune_rules__Dir_with_dune.inner_fold in file
    "src/dune_rules/dir_with_dune.ml", line 24, characters 55-67
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Install_rules.install_entries in file
    "src/dune_rules/install_rules.ml", line 699, characters 17-59
  Called from Dune_rules__Install_rules.install_rules in file
    "src/dune_rules/install_rules.ml", line 708, characters 4-32
  Called from Dune_rules__Install_rules.memo.(fun) in file
    "src/dune_rules/install_rules.ml", line 839, characters 16-38
  Called from Stdune__Exn.protectx in file "src/stdune/exn.ml", line 12,
    characters 8-11
  Re-raised at Stdune__Exn.protectx in file "src/stdune/exn.ml", line 18,
    characters 4-11
  Called from Memo__Implicit_output.collect_sync in file
    "src/memo/implicit_output.ml", line 119, characters 4-162
  Called from Dune_engine__Rules.collect_opt in file "src/dune_engine/rules.ml"
    (inlined), line 192, characters 20-71
  Called from Dune_engine__Rules.collect in file "src/dune_engine/rules.ml",
    line 195, characters 20-33
  Called from Dune_engine__Rules.collect_unit in file
    "src/dune_engine/rules.ml", line 199, characters 18-27
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Install_rules.memo.(fun) in file
    "src/dune_rules/install_rules.ml", line 847, characters 48-71
  Called from Dune_rules__Scheme.evaluate.loop in file
    "src/dune_rules/scheme.ml", line 138, characters 27-33
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
    "src/dune_rules/scheme.ml", line 76, characters 51-70
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Memo.Lazy.force in file "src/memo/memo.ml" (inlined), line 1061,
    characters 16-20
  Called from Memo.Lazy.bind.(fun) in file "src/memo/memo.ml", line 1067,
    characters 45-54
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Scheme.Evaluated.get_rules in file
    "src/dune_rules/scheme.ml", line 103, characters 6-34
  Called from Dune_rules__Install_rules.gen_rules in file
    "src/dune_rules/install_rules.ml", line 871, characters 4-72
  Called from Dune_rules__Gen_rules.gen_rules in file
    "src/dune_rules/gen_rules.ml", line 295, characters 25-58
  Called from Stdune__Exn.protectx in file "src/stdune/exn.ml", line 12,
    characters 8-11
  Re-raised at Stdune__Exn.protectx in file "src/stdune/exn.ml", line 18,
    characters 4-11
  Called from Memo__Implicit_output.collect_sync in file
    "src/memo/implicit_output.ml", line 119, characters 4-162
  Called from Dune_engine__Rules.collect_opt in file "src/dune_engine/rules.ml"
    (inlined), line 192, characters 20-71
  Called from Dune_engine__Rules.collect in file "src/dune_engine/rules.ml",
    line 195, characters 20-33
  Called from Dune_engine__Build_system.Load_rules.load_dir_step2_exn in file
    "src/dune_engine/build_system.ml", line 895, characters 6-76
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_engine__Build_system.Load_rules.load_dir_impl in file
    "src/dune_engine/build_system.ml", line 1042, characters 12-43
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_engine__Build_system.Load_rules.targets_of in file
    "src/dune_engine/build_system.ml", line 684, characters 10-23
  Called from Dune_engine__Build_system.Load_rules.file_exists in file
    "src/dune_engine/build_system.ml", line 681, characters 17-34
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 305, characters 9-22
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 296, characters 58-73
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 296, characters 42-57
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 296, characters 42-57
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 294, characters 55-70
  Called from
    Dune_engine__Build_system.Exported.Build_request.evaluate_and_wait_for_dynamic_dependencies
    in file "src/dune_engine/build_system.ml", line 1230, characters 24-39
  Called from Dune_engine__Build_system.do_build.(fun) in file
    "src/dune_engine/build_system.ml", line 1845, characters 8-97
  Called from Fiber.Execution_context.safe_run_k in file "src/fiber/fiber.ml",
    line 129, characters 18-21
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Fiber.Execution_context.forward_exn_with_bt in file
    "src/fiber/fiber.ml", line 136, characters 10-17
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]
  % ls _build/install/default/lib/foo/*.a
  ls: _build/install/default/lib/foo/*.a: No such file or directory
  [1]
  
  # create a dummy executable to test
  -> creating dune
  -> creating b.ml
  
  # make sure that this library is usable locally
  % dune exec ./exe/b.exe
  exe working
  
  # make sure that this library is usable externally
  % rm -rf lib
  % OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  File "exe/dune", line 5, characters 12-15:
  5 |  (libraries foo))
                  ^^^
  Error: Library "foo" not found.
  Hint: try:
    dune external-lib-deps --missing --build-dir _b2 ./exe/b.exe
  [1]
  
  
  # no_mli_unwrapped_no_stubs
  -> creating dune-project
  # build the library and see if .a is present
  -> creating dune
  % dune build --root . @install
  Error: exception { exn =
      ("External.cm_dir",
      { t =
          { public_dir = In_build_dir "default/lib"
          ; private_dir = None
          ; public_cmi_dir = None
          }
      })
  ; backtrace =
      [ { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("stanzas-to-entries", "default")
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Memo.Lazy.force in file \"src/memo/memo.ml\"
  (inlined), line 1061, characters 16-20\n\
             Called from Memo.Lazy.bind.(fun) in file \"src/memo/memo.ml\",
  line 1067, characters 45-54\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("<unnamed>", ())
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Memo.Lazy.force in file \"src/memo/memo.ml\"
  (inlined), line 1061, characters 16-20\n\
             Called from Memo.Lazy.bind.(fun) in file \"src/memo/memo.ml\",
  line 1067, characters 45-54\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.get_rules in file
  \"src/dune_rules/scheme.ml\", line 103, characters 6-34\n\
             Called from Dune_rules__Install_rules.gen_rules in file
  \"src/dune_rules/install_rules.ml\", line 871, characters 4-72\n\
             Called from Dune_rules__Gen_rules.gen_rules in file
  \"src/dune_rules/gen_rules.ml\", line 295, characters 25-58\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from
  Dune_engine__Build_system.Load_rules.load_dir_step2_exn in file
  \"src/dune_engine/build_system.ml\", line 895, characters 6-76\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("load-dir", In_build_dir "default")
        }
      ; { ocaml =
            "Raised at Stdune__Code_error.raise in file
  \"src/stdune/code_error.ml\", line 9, characters 30-62\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in file
  \"src/dune_rules/install_rules.ml\", line 149, characters 19-69\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\" (inlined), line 166, characters 23-31\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 181, characters 49-63\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\", line
  5, characters 19-33\n\
             Called from Stdlib__list.rev_map.rmap_f in file \"list.ml\", line
  103, characters 22-25\n\
             Called from Stdune__List.map in file \"src/stdune/list.ml\"
  (inlined), line 5, characters 19-33\n\
             Called from Stdune__List.concat_map in file
  \"src/stdune/list.ml\", line 40, characters 29-39\n\
             Called from
  Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in file
  \"src/dune_rules/install_rules.ml\", line 156, characters 6-1023\n\
             Called from Dune_rules__Dir_with_dune.inner_fold in file
  \"src/dune_rules/dir_with_dune.ml\", line 24, characters 55-67\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.install_entries in file
  \"src/dune_rules/install_rules.ml\", line 699, characters 17-59\n\
             Called from Dune_rules__Install_rules.install_rules in file
  \"src/dune_rules/install_rules.ml\", line 708, characters 4-32\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 839, characters 16-38\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from Dune_engine__Rules.collect_unit in file
  \"src/dune_engine/rules.ml\", line 199, characters 18-27\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Install_rules.memo.(fun) in file
  \"src/dune_rules/install_rules.ml\", line 847, characters 48-71\n\
             Called from Dune_rules__Scheme.evaluate.loop in file
  \"src/dune_rules/scheme.ml\", line 138, characters 27-33\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
  \"src/dune_rules/scheme.ml\", line 76, characters 51-70\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Memo.Lazy.force in file \"src/memo/memo.ml\"
  (inlined), line 1061, characters 16-20\n\
             Called from Memo.Lazy.bind.(fun) in file \"src/memo/memo.ml\",
  line 1067, characters 45-54\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_rules__Scheme.Evaluated.get_rules in file
  \"src/dune_rules/scheme.ml\", line 103, characters 6-34\n\
             Called from Dune_rules__Install_rules.gen_rules in file
  \"src/dune_rules/install_rules.ml\", line 871, characters 4-72\n\
             Called from Dune_rules__Gen_rules.gen_rules in file
  \"src/dune_rules/gen_rules.ml\", line 295, characters 25-58\n\
             Called from Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 12, characters 8-11\n\
             Re-raised at Stdune__Exn.protectx in file \"src/stdune/exn.ml\",
  line 18, characters 4-11\n\
             Called from Memo__Implicit_output.collect_sync in file
  \"src/memo/implicit_output.ml\", line 119, characters 4-162\n\
             Called from Dune_engine__Rules.collect_opt in file
  \"src/dune_engine/rules.ml\" (inlined), line 192, characters 20-71\n\
             Called from Dune_engine__Rules.collect in file
  \"src/dune_engine/rules.ml\", line 195, characters 20-33\n\
             Called from
  Dune_engine__Build_system.Load_rules.load_dir_step2_exn in file
  \"src/dune_engine/build_system.ml\", line 895, characters 6-76\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             Re-raised at Stdune__Exn.raise_with_backtrace in file
  \"src/stdune/exn.ml\", line 36, characters 27-56\n\
             Called from Dune_engine__Build_system.Load_rules.load_dir_impl in
  file \"src/dune_engine/build_system.ml\", line 1042, characters 12-43\n\
             Called from Stdune__Exn_with_backtrace.try_with in file
  \"src/stdune/exn_with_backtrace.ml\", line 9, characters 8-12\n\
             "
        ; memo = ("load-dir", In_build_dir ".aliases/default")
        }
      ]
  ; outer_call_stack = []
  }
  Raised at Stdune__Code_error.raise in file "src/stdune/code_error.ml", line
    9, characters 30-62
  Called from
    Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.cm_dir in
    file "src/dune_rules/install_rules.ml", line 149, characters 19-69
  Called from
    Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in
    file "src/dune_rules/install_rules.ml" (inlined), line 166, characters
    23-31
  Called from
    Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files.(fun) in
    file "src/dune_rules/install_rules.ml", line 181, characters 49-63
  Called from Stdlib__list.rev_map.rmap_f in file "list.ml", line 103,
    characters 22-25
  Called from Stdune__List.map in file "src/stdune/list.ml", line 5, characters
    19-33
  Called from Stdlib__list.rev_map.rmap_f in file "list.ml", line 103,
    characters 22-25
  Called from Stdune__List.map in file "src/stdune/list.ml" (inlined), line 5,
    characters 19-33
  Called from Stdune__List.concat_map in file "src/stdune/list.ml", line 40,
    characters 29-39
  Called from Dune_rules__Install_rules.Stanzas_to_entries.lib_install_files in
    file "src/dune_rules/install_rules.ml", line 156, characters 6-1023
  Called from Dune_rules__Dir_with_dune.inner_fold in file
    "src/dune_rules/dir_with_dune.ml", line 24, characters 55-67
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Install_rules.install_entries in file
    "src/dune_rules/install_rules.ml", line 699, characters 17-59
  Called from Dune_rules__Install_rules.install_rules in file
    "src/dune_rules/install_rules.ml", line 708, characters 4-32
  Called from Dune_rules__Install_rules.memo.(fun) in file
    "src/dune_rules/install_rules.ml", line 839, characters 16-38
  Called from Stdune__Exn.protectx in file "src/stdune/exn.ml", line 12,
    characters 8-11
  Re-raised at Stdune__Exn.protectx in file "src/stdune/exn.ml", line 18,
    characters 4-11
  Called from Memo__Implicit_output.collect_sync in file
    "src/memo/implicit_output.ml", line 119, characters 4-162
  Called from Dune_engine__Rules.collect_opt in file "src/dune_engine/rules.ml"
    (inlined), line 192, characters 20-71
  Called from Dune_engine__Rules.collect in file "src/dune_engine/rules.ml",
    line 195, characters 20-33
  Called from Dune_engine__Rules.collect_unit in file
    "src/dune_engine/rules.ml", line 199, characters 18-27
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Install_rules.memo.(fun) in file
    "src/dune_rules/install_rules.ml", line 847, characters 48-71
  Called from Dune_rules__Scheme.evaluate.loop in file
    "src/dune_rules/scheme.ml", line 138, characters 27-33
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Scheme.Evaluated.restrict.(fun) in file
    "src/dune_rules/scheme.ml", line 76, characters 51-70
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Memo.Lazy.force in file "src/memo/memo.ml" (inlined), line 1061,
    characters 16-20
  Called from Memo.Lazy.bind.(fun) in file "src/memo/memo.ml", line 1067,
    characters 45-54
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_rules__Scheme.Evaluated.get_rules in file
    "src/dune_rules/scheme.ml", line 103, characters 6-34
  Called from Dune_rules__Install_rules.gen_rules in file
    "src/dune_rules/install_rules.ml", line 871, characters 4-72
  Called from Dune_rules__Gen_rules.gen_rules in file
    "src/dune_rules/gen_rules.ml", line 295, characters 25-58
  Called from Stdune__Exn.protectx in file "src/stdune/exn.ml", line 12,
    characters 8-11
  Re-raised at Stdune__Exn.protectx in file "src/stdune/exn.ml", line 18,
    characters 4-11
  Called from Memo__Implicit_output.collect_sync in file
    "src/memo/implicit_output.ml", line 119, characters 4-162
  Called from Dune_engine__Rules.collect_opt in file "src/dune_engine/rules.ml"
    (inlined), line 192, characters 20-71
  Called from Dune_engine__Rules.collect in file "src/dune_engine/rules.ml",
    line 195, characters 20-33
  Called from Dune_engine__Build_system.Load_rules.load_dir_step2_exn in file
    "src/dune_engine/build_system.ml", line 895, characters 6-76
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_engine__Build_system.Load_rules.load_dir_impl in file
    "src/dune_engine/build_system.ml", line 1042, characters 12-43
  Called from Stdune__Exn_with_backtrace.try_with in file
    "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Dune_engine__Build_system.Load_rules.targets_of in file
    "src/dune_engine/build_system.ml", line 684, characters 10-23
  Called from Dune_engine__Build_system.Load_rules.file_exists in file
    "src/dune_engine/build_system.ml", line 681, characters 17-34
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 305, characters 9-22
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 296, characters 58-73
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 296, characters 42-57
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 296, characters 42-57
  Called from Dune_engine__Build.Analysis.static_deps in file
    "src/dune_engine/build.ml", line 294, characters 55-70
  Called from
    Dune_engine__Build_system.Exported.Build_request.evaluate_and_wait_for_dynamic_dependencies
    in file "src/dune_engine/build_system.ml", line 1230, characters 24-39
  Called from Dune_engine__Build_system.do_build.(fun) in file
    "src/dune_engine/build_system.ml", line 1845, characters 8-97
  Called from Fiber.Execution_context.safe_run_k in file "src/fiber/fiber.ml",
    line 129, characters 18-21
  Re-raised at Stdune__Exn.raise_with_backtrace in file "src/stdune/exn.ml",
    line 36, characters 27-56
  Called from Fiber.Execution_context.forward_exn_with_bt in file
    "src/fiber/fiber.ml", line 136, characters 10-17
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]
  % ls _build/install/default/lib/foo/*.a
  ls: _build/install/default/lib/foo/*.a: No such file or directory
  [1]
  
  # create a dummy executable to test
  -> creating dune
  -> creating b.ml
  
  # make sure that this library is usable locally
  % dune exec ./exe/b.exe
  exe working
  
  # make sure that this library is usable externally
  % rm -rf lib
  % OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  File "exe/dune", line 5, characters 12-15:
  5 |  (libraries foo))
                  ^^^
  Error: Library "foo" not found.
  Hint: try:
    dune external-lib-deps --missing --build-dir _b2 ./exe/b.exe
  [1]
  
  
  mli_only_wrapped_stubs - external - fail
  mli_only_wrapped_stubs - internal - pass
  mli_only_wrapped_no_stubs - external - fail
  mli_only_wrapped_no_stubs - internal - pass
  mli_only_unwrapped_stubs - external - fail
  mli_only_unwrapped_stubs - internal - pass
  mli_only_unwrapped_no_stubs - external - fail
  mli_only_unwrapped_no_stubs - internal - pass
  no_mli_wrapped_stubs - external - fail
  no_mli_wrapped_stubs - internal - pass
  no_mli_wrapped_no_stubs - external - fail
  no_mli_wrapped_no_stubs - internal - pass
  no_mli_unwrapped_stubs - external - fail
  no_mli_unwrapped_stubs - internal - pass
  no_mli_unwrapped_no_stubs - external - fail
  no_mli_unwrapped_no_stubs - internal - pass

