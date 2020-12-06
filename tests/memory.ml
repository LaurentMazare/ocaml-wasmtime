open! Base
module W = Wasmtime.Wrappers

let memory_wat =
  {|
(module
  (memory (export "memory") 2 3)

  (func (export "size") (result i32) (memory.size))
  (func (export "load") (param i32) (result i32)
    (i32.load8_s (local.get 0))
  )
  (func (export "store") (param i32 i32)
    (i32.store8 (local.get 0) (local.get 1))
  )

  (data (i32.const 0x1000) "\01\02\03\04")
)
|}

let%expect_test _ =
  let engine = W.Engine.create () in
  let store = W.Store.create engine in
  let wasm = W.Wasmtime.wat_to_wasm ~wat:(W.Byte_vec.of_string memory_wat) in
  let modl = W.Wasmtime.new_module engine ~wasm in
  let instance = W.Wasmtime.new_instance store modl in
  let memory, size_func, load_func, store_func =
    match W.Instance.exports instance with
    | [ memory; func1; func2; func3 ] ->
      ( W.Extern.as_memory memory
      , W.Extern.as_func func1
      , W.Extern.as_func func2
      , W.Extern.as_func func3 )
    | _ -> failwith "expected four externs to be returned"
  in
  let page_size = W.Memory.size_in_pages memory in
  let byte_size = W.Memory.size_in_bytes memory in
  Stdio.printf "memory size %d pages, %d bytes (%d)\n%!" page_size byte_size 0x20000;
  let size = W.Wasmtime.func_call1 size_func [] |> W.Val.int_exn in
  Stdio.printf "size_func returned %d\n%!" size;
  let print_addr addr =
    let value =
      try
        W.Wasmtime.func_call1 load_func [ Int32 addr ] |> W.Val.int_exn |> Int.to_string
      with
      | W.Trap { message = _ } -> "trapped"
    in
    let value_mem =
      try
        let memory = W.Memory.to_string memory ~pos:addr ~len:1 in
        memory.[0] |> Char.to_int |> Int.to_string
      with
      | _ -> "exn"
    in
    Stdio.printf "load %x: %s %s\n" addr value value_mem
  in
  List.iter [ 0; 0x1000; 0x1002; 0x1003; 0x1ffff; 0x20000 ] ~f:print_addr;
  W.Memory.set memory ~pos:0x1002 (Char.of_int_exn 5);
  W.Wasmtime.func_call0 store_func [ Int32 0x1003; Int32 6 ];
  List.iter [ 0x1002; 0x1003 ] ~f:print_addr;
  Stdio.printf "grow %b\n" (W.Memory.grow memory 1);
  let page_size = W.Memory.size_in_pages memory in
  let byte_size = W.Memory.size_in_bytes memory in
  Stdio.printf "memory size %d pages, %d bytes (%d)\n%!" page_size byte_size 0x30000;
  List.iter [ 0; 0x1000; 0x1002; 0x1003; 0x1ffff; 0x20000 ] ~f:print_addr;
  W.Wasmtime.func_call0 store_func [ Int32 0x20000; Int32 42 ];
  print_addr 0x20000;
  [%expect
    {|
    memory size 2 pages, 131072 bytes (131072)
    size_func returned 2
    load 0: 0 0
    load 1000: 1 1
    load 1002: 3 3
    load 1003: 4 4
    load 1ffff: 0 0
    load 20000: trapped exn
    load 1002: 5 5
    load 1003: 6 6
    grow true
    memory size 3 pages, 196608 bytes (196608)
    load 0: 0 0
    load 1000: 1 1
    load 1002: 5 5
    load 1003: 6 6
    load 1ffff: 0 0
    load 20000: 0 0
    load 20000: 42 42 |}]
