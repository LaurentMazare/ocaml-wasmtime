open! Base
module W = Wasmtime.Wrappers

(* Test using the low-level API. *)
let gcd_wat =
  {|
(module
  (func $gcd (param i32 i32) (result i32)
    (local i32)
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        br_if 0 (;@2;)
        local.get 1
        local.set 2
        br 1 (;@1;)
      end
      loop  ;; label = @2
        local.get 1
        local.get 0
        local.tee 2
        i32.rem_u
        local.set 0
        local.get 2
        local.set 1
        local.get 0
        br_if 0 (;@2;)
      end
    end
    local.get 2
  )
  (export "gcd" (func $gcd))
)
|}

let%expect_test _ =
  let engine = W.Engine.create () in
  let store = W.Store.create engine in
  let wasm = W.Wasmtime.wat_to_wasm ~wat:(W.Byte_vec.of_string gcd_wat) in
  Stdio.printf "here %d\n%!" (W.Byte_vec.length wasm);
  let modl = W.Wasmtime.new_module engine ~wasm in
  let instance = W.Wasmtime.new_instance store modl in
  let gcd_func =
    match W.Instance.exports instance with
    | [ extern ] -> W.Extern.as_func extern
    | _ -> failwith "expected a single extern to be returned"
  in
  let res = W.Wasmtime.func_call1 gcd_func [ Int32 6; Int32 27 ] in
  Stdio.printf "gcd returned %d\n%!" (W.Val.int_exn res);
  [%expect {|
    here 91
    gcd returned 3 |}]
