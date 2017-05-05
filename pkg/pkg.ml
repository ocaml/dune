#!/usr/bin/env ocaml
#use "./topkg-jbuilder"

let () =
  Pkg.describe "jbuilder"
    ~readmes:    [ Pkg.std_file "README.org"     ]
    ~change_logs:[ Pkg.std_file "CHANGES.org"    ]
    ~licenses:   [ Pkg.std_file "LICENSE.txt"    ]
