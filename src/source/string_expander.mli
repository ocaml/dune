open Import
module Memo : String_with_vars.Expander with type 'a app := 'a Memo.t
