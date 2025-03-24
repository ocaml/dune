type t = Preprocess.Without_instrumentation.t Preprocess.Per_module.t

let decode = Preprocess.Per_module.decode
let default = Preprocess.Per_module.default ()
let no_lint = default
