
module Gen (S : sig val sctx : Super_context.t end) = struct

  let setup_library_odoc_rules _ ~scope:_ ~modules:_ ~requires:_
        ~dep_graphs:_ = ()

  let init ~modules_by_lib:_ ~mlds_of_dir:_ = ()

  let gen_rules ~dir:_ _ = ()
end
