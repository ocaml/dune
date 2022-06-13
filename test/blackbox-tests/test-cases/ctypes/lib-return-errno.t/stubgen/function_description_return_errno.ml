open Ctypes

module Functions ( F : Ctypes.FOREIGN ) = struct
  open F
  let add4 = foreign "example_add4" (int @-> returning int)
end
