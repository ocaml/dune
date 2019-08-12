module Dir_contents = Dir_contents (* hack for bootstrappign + ocamldep*)

let setup_rules ~sctx:_ ~dir:_ ~dir_contents:_ _ = []

let install_rules ~sctx:_ ~dir:_ _ = []

let coqpp_rules ~sctx:_ ~build_dir:_ ~dir:_ _ = []
