; comment top
(alias
 (name "runtest")
 (action (run %{bin:ocamlformat} -n 1 -i dir1/dir2/ignore_2.ml)))

(executable
 (name aux)
 (c_names cn1 cn2)
 (c_flags cf1 cf2)
 (cxx_names cxxn1 cxxn2)
 (cxx_flags cxxf1 cxxf2))
