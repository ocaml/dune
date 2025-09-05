open Import

val fold
  :  'a Module_name.Per_item.t
  -> init:'b
  -> f:('a -> 'b -> 'b Resolve.Memo.t)
  -> 'b Resolve.Memo.t

val with_instrumentation
  :  Preprocess.With_instrumentation.t Preprocess.Per_module.t
  -> instrumentation_backend:
       (Loc.t * Lib_name.t -> Preprocess.Without_instrumentation.t option Resolve.Memo.t)
  -> Preprocess.Without_instrumentation.t Preprocess.Per_module.t Resolve.Memo.t
