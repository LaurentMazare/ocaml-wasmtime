open! Base
module W = Wasmtime.Wrappers

let hello_wat =
  {|
(module
  (func $hello (import "" "hello"))
  (func (export "run") (call $hello))
)
|}

let%expect_test _ =
  let engine = W.Engine.create () in
  let store = W.Store.create engine in
  let wasm = W.Wasmtime.wat_to_wasm ~wat:(W.Byte_vec.of_string hello_wat) in
  let modl = W.Wasmtime.new_module engine ~wasm in
  let hello_func =
    W.Func.of_func_0_0 store (fun () -> Stdio.printf "Hello World! (from ocaml)\n")
  in
  let instance =
    W.Wasmtime.new_instance ~imports:[ W.Extern.func_as hello_func ] store modl
  in
  let hello_func =
    match W.Instance.exports instance with
    | [ extern ] -> W.Extern.as_func extern
    | _ -> failwith "expected a single extern to be returned"
  in
  W.Wasmtime.func_call0 hello_func [];
  [%expect {|
    Hello World! (from ocaml) |}]
