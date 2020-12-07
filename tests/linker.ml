open! Base
module W = Wasmtime.Wrappers

let linking1_wat =
  {|
(module
  (import "linking2" "double" (func $double (param i32) (result i32)))
  (import "linking2" "log" (func $log (param i32 i32)))
  (import "linking2" "memory" (memory 1))
  (import "linking2" "memory_offset" (global $offset i32))

  (func (export "run")
    ;; Call into the other module to double our number, and we could print it
    ;; here but for now we just drop it
    i32.const 2
    call $double
    drop

    ;; Our `data` segment initialized our imported memory, so let's print the
    ;; string there now.
    global.get $offset
    i32.const 14
    call $log
  )

  (data (global.get $offset) "Hello, world!\n")
)
|}

let linking2_wat =
  {|
(module
  (type $fd_write_ty (func (param i32 i32 i32 i32) (result i32)))
  (import "wasi_snapshot_preview1" "fd_write" (func $fd_write (type $fd_write_ty)))

  (func (export "double") (param i32) (result i32)
    local.get 0
    i32.const 2
    i32.mul
  )

  (func (export "log") (param i32 i32)
    ;; store the pointer in the first iovec field
    i32.const 4
    local.get 0
    i32.store

    ;; store the length in the first iovec field
    i32.const 4
    local.get 1
    i32.store offset=4

    ;; call the `fd_write` import
    i32.const 1     ;; stdout fd
    i32.const 4     ;; iovs start
    i32.const 1     ;; number of iovs
    i32.const 0     ;; where to write nwritten bytes
    call $fd_write
    drop
  )

  (memory (export "memory") 2)
  (global (export "memory_offset") i32 (i32.const 65536))
)
|}

let%expect_test _ =
  let engine = W.Engine.create () in
  let store = W.Store.create engine in
  let wasi_instance =
    W.Wasi_instance.create
      store
      `wasi_snapshot_preview
      ~inherit_argv:true
      ~inherit_env:true
      ~inherit_stdin:true
      ~inherit_stderr:true
      ~inherit_stdout:true
  in
  let linker = W.Wasmtime.Linker.create store in
  let instantiate ~wat =
    let wasm = W.Wasmtime.wat_to_wasm ~wat in
    W.Wasmtime.new_module engine ~wasm |> W.Wasmtime.Linker.instantiate linker
  in
  W.Wasmtime.Linker.define_wasi linker wasi_instance;
  let name2 = W.Byte_vec.of_string "linking2" in
  let instance2 = instantiate ~wat:(W.Byte_vec.of_string linking2_wat) in
  W.Wasmtime.Linker.define_instance linker ~name:name2 instance2;
  let instance1 = instantiate ~wat:(W.Byte_vec.of_string linking1_wat) in
  let run =
    match W.Instance.exports instance1 with
    | [ run ] -> W.Extern.as_func run
    | l -> Printf.failwithf "expected a single export, got %d" (List.length l) ()
  in
  W.Wasmtime.func_call0 run [];
  [%expect {| Hello, world! |}]
