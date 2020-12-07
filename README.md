# ocaml-wasmtime
OCaml WebAssembly runtime powered by [wasmtime](https://wasmtime.dev/)

This library let you run WebAssembly modules within OCaml with some
support for [wasi](https://wasi.dev/) for system calls.
It acts as a low-level and typesafe wrapper around the wasmtime C library,
only a subset of the functions are supported for now but it should be enough
to run some basic examples, see the `tests` directory for details.

### Installation

#### Using Opam

The simplest way to install this library is via opam, this installs both
the C library and the OCcaml bindings.

```bash
opam install wasmtime
```

#### Building from source

First the wasmtime C library has to be installed. This can either be done
via opam.
```bash
opam install libwasmtime
```
Or this can be done manually.
- Download the wasmtime c-api library from the [github repo](https://github.com/bytecodealliance/wasmtime/releases).
- Uncompress the archive and set the `LIBWASTIME` environment variable to point at them.
```bash
wget https://github.com/bytecodealliance/wasmtime/releases/download/v0.21.0/wasmtime-v0.21.0-x86_64-linux-c-api.tar.xz
tar xf wasmtime-v0.21.0-x86_64-linux-c-api.tar.xz
export LIBWASTIME=$PWD/wasmtime-v0.21.0-x86_64-linux-c-api
```

Once the C library has been installed and the repo has been checked out, the
examples can be rune with dune.
```bash
dune runtest
```


This has been tested with wasmtime v0.21.0 on a linux platform.
