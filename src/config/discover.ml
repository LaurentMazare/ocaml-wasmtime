open Base
module C = Configurator.V1

let empty_flags = { C.Pkg_config.cflags = []; libs = [] }

let combine (flags1 : C.Pkg_config.package_conf) (flags2 : C.Pkg_config.package_conf) =
  { C.Pkg_config.cflags = flags1.cflags @ flags2.cflags
  ; libs = flags1.libs @ flags2.libs
  }

let ( /^ ) = Caml.Filename.concat
let file_exists = Caml.Sys.file_exists

let extract_flags c ~package =
  Option.bind (C.Pkg_config.get c) ~f:(C.Pkg_config.query ~package)

let wasmtime_flags () =
  let config ~lib_dir =
    let cflags = [ "-isystem"; Printf.sprintf "%s/include" lib_dir ] in
    let libs = [ Printf.sprintf "%s/lib/libwasmtime.a" lib_dir ] in
    { C.Pkg_config.cflags; libs }
  in
  match Caml.Sys.getenv_opt "LIBWASMTIME" with
  | Some lib_dir -> config ~lib_dir
  | None ->
    (match Caml.Sys.getenv_opt "OPAM_SWITCH_PREFIX" with
    | Some prefix ->
      let lib_dir = prefix /^ "lib" /^ "libwasmtime" in
      if file_exists lib_dir then config ~lib_dir else empty_flags
    | None -> empty_flags)

let () =
  C.main ~name:"wasmtime-config" (fun _c ->
      let wasmtime_flags =
        try wasmtime_flags () with
        | _ -> empty_flags
      in
      C.Flags.write_sexp "c_flags.sexp" wasmtime_flags.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" wasmtime_flags.libs)
