(module
 (import "env" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
 (import "js" "jsPrint" (func $jsPrint (param anyref)))
 (func (export "jsPrint") (param $x (ref eq)) (result (ref eq))
    (call $jsPrint (call $unwrap (local.get $x)))
    (ref.i31 (i32.const 0)))
)
