module Types (F : Ctypes.TYPE) = struct
  open F

  let qux_version = constant "QUX_VERSION" int

end
