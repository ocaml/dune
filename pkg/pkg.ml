#use "topfind"
#require "topkg-jbuilder"

open Topkg

let () =
  Topkg_jbuilder.describe ()
    ~readmes: [ Pkg.std_file "README.org"     ]
    ~licenses:[ Pkg.std_file "LICENSE.txt"    ]
