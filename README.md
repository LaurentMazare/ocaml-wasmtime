# ocaml-wasmtime
OCaml WebAssembly runtime powered by [Wasmtime](https://wasmtime.dev/)

This library let you run WebAssembly modules within OCaml with some
support for [wasi](https://wasi.dev/) for system calls.
It acts as a low-level and typesafe wrapper around the wasmtime C library,
not all functions are supported but it has enough to run some
basic examples, see the `tests` directory for details.

### Installation

- Download the wasmtime c-api library from the [github repo](https://github.com/bytecodealliance/wasmtime/releases).
- Uncompress the archive and copy the `include` and `lib` directory in the `wasmtime` directory of this repo.
- Run `dune runtest` to run the examples.

This has been tested with wasmtime v0.21.0.
