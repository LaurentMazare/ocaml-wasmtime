(* A command line tool taking as input a wasm file and executing it. *)
open! Base
module W = Wasmtime.Wrappers

let run ~filename =
  let engine = W.Engine.create () in
  let store = W.Store.create engine in
  let wasm =
    if String.is_suffix filename ~suffix:".wat"
    then (
      let content = Stdio.In_channel.read_all filename in
      W.Wasmtime.wat_to_wasm ~wat:(W.Byte_vec.of_string content))
    else if String.is_suffix filename ~suffix:".wasm"
    then Stdio.In_channel.read_all filename |> W.Byte_vec.of_string
    else failwith "the filename has to end with .wat or .wasm"
  in
  let modl = W.Wasmtime.new_module engine ~wasm in
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
  W.Wasmtime.Linker.define_wasi linker wasi_instance;
  let name = W.Byte_vec.of_string "foo" in
  W.Wasmtime.Linker.module_ linker ~name modl;
  let wasi_func = W.Wasmtime.Linker.get_default linker ~name in
  W.Wasmtime.func_call0 wasi_func []

let () =
  match Caml.Sys.argv with
  | [| _; filename |] -> run ~filename
  | _ -> Printf.failwithf "usage: %s file.{wasm,wat}" Caml.Sys.argv.(0) ()
