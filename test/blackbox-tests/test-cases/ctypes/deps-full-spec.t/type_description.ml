module Types (F : Ctypes.TYPE) = struct
  open F

  let foo_version = constant "FOO_VERSION" int
  let bar_version = constant "BAR_VERSION" int
  let baz_version = constant "BAZ_VERSION" int
  let qux_version = constant "QUX_VERSION" int

end
