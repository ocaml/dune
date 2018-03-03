open Import
open Jbuild

let to_rules (t : Jbuild.Menhir.t) =
  let module S = String_with_vars in
  let targets n = [n ^ ".ml"; n ^ ".mli"] in
  match t.merge_into with
  | None ->
    List.map t.modules ~f:(fun name ->
      let src = name ^ ".mly" in
      { Jbuild.Rule.
        targets = Static (targets name)
      ; deps    = [Dep_conf.File (S.virt_text __POS__ src)]
      ; action  =
          Chdir
            (S.virt_var __POS__ "ROOT",
             Run (S.virt_text __POS__ "menhir",
                  t.flags @ [S.virt_var __POS__ "<"]))
      ; mode  = t.mode
      ; locks = []
      ; loc = t.loc
      })
  | Some merge_into ->
    let mly m = S.virt_text __POS__ (m ^ ".mly") in
    [{ Rule.
       targets = Static (targets merge_into)
     ; deps    = List.map ~f:(fun m -> Dep_conf.File (mly m)) t.modules
     ; action  =
         Chdir
           (S.virt_var __POS__ "ROOT",
            Run (S.virt_text __POS__ "menhir",
                 List.concat
                   [ [ S.virt_text __POS__ "--base"
                     ; S.virt_var __POS__ ("path-no-dep:" ^ merge_into)
                     ]
                   ; t.flags
                   ; [ S.virt_var __POS__ "^" ]
                   ]))
     ; mode  = t.mode
     ; locks = []
     ; loc = t.loc
     }]
