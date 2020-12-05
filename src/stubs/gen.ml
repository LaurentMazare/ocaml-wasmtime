let () =
  let fmt file = Format.formatter_of_out_channel (open_out file) in
  let fmt_c = fmt "wasmtime_stubs.c" in
  let fmt_ml = fmt "wasmtime_generated.ml" in
  Format.fprintf fmt_c "#include <wasm.h>\n";
  Format.fprintf fmt_c "#include <wasi.h>\n";
  Format.fprintf fmt_c "#include <wasmtime.h>\n";
  Cstubs.write_c fmt_c ~prefix:"caml_" (module Bindings.C);
  Cstubs.write_ml fmt_ml ~prefix:"caml_" (module Bindings.C);
  flush_all ()
