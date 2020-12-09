open! Base
module W = Wasmtime.Wrappers

let multi_wat =
  {|
(module
  (func $f (import "" "f") (param i32 i64) (result i64 i32))

  (func $g (export "g") (param i32 i64) (result i64 i32)
    (call $f (local.get 0) (local.get 1))
  )

  (func $round_trip_many
    (export "round_trip_many")
    (param i64 i64 i64 i64 i64 i64 i64 i64 i64 i64)
    (result i64 i64 i64 i64 i64 i64 i64 i64 i64 i64)

    local.get 0
    local.get 1
    local.get 2
    local.get 3
    local.get 4
    local.get 5
    local.get 6
    local.get 7
    local.get 8
    local.get 9)
)
|}

let%expect_test _ =
  let engine = W.Engine.create () in
  let store = W.Store.create engine in
  let wasm = W.Wasmtime.wat_to_wasm ~wat:(W.Byte_vec.of_string multi_wat) in
  let modl = W.Wasmtime.new_module engine ~wasm in
  let f =
    W.Func.of_func ~args:[ Int32; Int64 ] ~results:[ Int64; Int32 ] store (fun args ->
        match args with
        | [ Int32 i1; Int64 i2 ] -> [ Int64 (i2 * 2); Int32 (i1 + 1) ]
        | _ -> failwith "unexpected argument types")
  in
  let instance = W.Wasmtime.new_instance ~imports:[ W.Extern.func_as f ] store modl in
  let g_func, _rt_func =
    match W.Instance.exports instance with
    | [ g; round_trip_many ] -> W.Extern.as_func g, W.Extern.as_func round_trip_many
    | _ -> failwith "expected two externs to be returned"
  in
  let v1, v2 = W.Wasmtime.func_call2 g_func [ Int32 6; Int64 27 ] in
  Stdio.printf "f returned (%d, %d)\n%!" (W.Val.int_exn v1) (W.Val.int_exn v2);
  [%expect {|
    f returned (54, 7) |}]
