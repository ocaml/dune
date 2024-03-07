include Stdune

include struct
  open Dune_engine
  module Fs_memo = Fs_memo
  module Fs_cache = Fs_cache
  module Process = Process
end

module Variant = Ocaml.Variant
module Mode = Ocaml.Mode

include struct
  open Dune_lang
  module Lib_name = Lib_name
  module Lib_kind = Lib_kind

  module Package = struct
    module Name = Dune_lang.Package_name
  end
end
