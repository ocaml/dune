module Fake = struct
  let run () =
    print_endline "defined in lib interface file"
end
module Baz = Baz
