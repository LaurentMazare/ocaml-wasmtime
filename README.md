# ocaml-wasmtime
OCaml WebAssembly runtime powered by [wasmtime](https://wasmtime.dev/)

This library let you run WebAssembly modules within OCaml with some
support for [wasi](https://wasi.dev/) for system calls.
It acts as a low-level and typesafe wrapper around the wasmtime C library,
only a subset of the functions are supported for now but it should be enough
to run some basic examples, see the `tests` directory for details.

### Installation

#### Using Opam

(upcoming, this simpler install version will be available once the opam package has been released)

#### Manual Install

- Download the wasmtime c-api library from the [github repo](https://github.com/bytecodealliance/wasmtime/releases).
- Uncompress the archive and set the `LIBWASTIME` environment variable to point at them.
- Run `dune runtest` to run the examples.

This can be achieved via the following commands on a linux system:

```bash
wget https://github.com/bytecodealliance/wasmtime/releases/download/v0.21.0/wasmtime-v0.21.0-x86_64-linux-c-api.tar.xz
tar xf wasmtime-v0.21.0-x86_64-linux-c-api.tar.xz
export LIBWASTIME=$PWD/wasmtime-v0.21.0-x86_64-linux-c-api
dune runtest
```


This has been tested with wasmtime v0.21.0 on a linux platform.
