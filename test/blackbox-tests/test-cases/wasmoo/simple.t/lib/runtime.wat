(module
 (import "env" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
 (import "js" "jsPrint" (func $print (param anyref)))
 (func (export "jsPrint") (param $x (ref eq)) (result (ref eq))
    (call $print (call $unwrap (local.get $x)))
    (ref.i31 (i32.const 0)))
)
