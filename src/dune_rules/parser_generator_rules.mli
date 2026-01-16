open Import

val gen_rules
  :  Super_context.t
  -> dir_contents:Dir_contents.t
  -> dir:Path.Build.t
  -> for_:Parser_generators.for_
  -> unit Memo.t
