open! Base
module W = Wasmtime.Wrappers

let wasi_wat =
  {|
(module
    ;; Import the required fd_write WASI function which will write the given io vectors to stdout
    ;; The function signature for fd_write is:
    ;; (File Descriptor, *iovs, iovs_len, nwritten) -> Returns number of bytes written
    (import "wasi_unstable" "fd_write" (func $fd_write (param i32 i32 i32 i32) (result i32)))

    (memory 1)
    (export "memory" (memory 0))

    ;; Write 'hello world\n' to memory at an offset of 8 bytes
    ;; Note the trailing newline which is required for the text to appear
    (data (i32.const 8) "hello world\n")

    (func $main (export "_start")
        ;; Creating a new io vector within linear memory
        (i32.store (i32.const 0) (i32.const 8))  ;; iov.iov_base - This is a pointer to the start of the 'hello world\n' string
        (i32.store (i32.const 4) (i32.const 12))  ;; iov.iov_len - The length of the 'hello world\n' string

        (call $fd_write
            (i32.const 1) ;; file_descriptor - 1 for stdout
            (i32.const 0) ;; *iovs - The pointer to the iov array, which is stored at memory location 0
            (i32.const 1) ;; iovs_len - We're printing 1 string stored in an iov - so one.
            (i32.const 20) ;; nwritten - A place in memory to store the number of bytes written
        )
        drop ;; Discard the number of bytes written from the top of the stack
    )
)
|}

let wasi_instance store =
  let config = W.Wasi_config.create () in
  W.Wasi_config.inherit_argv config;
  W.Wasi_config.inherit_env config;
  W.Wasi_config.inherit_stdin config;
  W.Wasi_config.inherit_stderr config;
  W.Wasi_config.inherit_stdout config;
  W.Wasi_instance.create store `wasi_unstable config

let%expect_test _ =
  let engine = W.Engine.create () in
  let store = W.Store.create engine in
  let wasm = W.Wasmtime.wat_to_wasm ~wat:(W.Byte_vec.of_string wasi_wat) in
  let modl = W.Wasmtime.new_module engine ~wasm in
  let wasi_instance = wasi_instance store in
  let linker = W.Wasmtime.Linker.create store in
  W.Wasmtime.Linker.define_wasi linker wasi_instance;
  let name = W.Byte_vec.of_string "foo" in
  W.Wasmtime.Linker.module_ linker ~name modl;
  let wasi_func = W.Wasmtime.Linker.get_default linker ~name in
  W.Wasmtime.func_call0 wasi_func [];
  [%expect {|
    hello world |}]
