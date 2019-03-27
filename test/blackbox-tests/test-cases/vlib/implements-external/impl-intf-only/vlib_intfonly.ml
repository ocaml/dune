module X : Foo.S = struct
  let mli_only = print_endline
end

let implme () = X.mli_only "implemented mli only"; 42
