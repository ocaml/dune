include Stdune

include struct
  open Dune_pkg
  module Lock_dir = Lock_dir
  module Opam_repo = Opam_repo
  module Package_version = Package_version
end

include struct
  open Dune_lang
  module Package_name = Package_name
end
