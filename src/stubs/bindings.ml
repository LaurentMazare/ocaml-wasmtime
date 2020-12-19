open! Ctypes

module C (F : Cstubs.FOREIGN) = struct
  open! F

  module Byte_vec = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasm_byte_vec_t"
    let size = field struct_ "size" size_t
    let data = field struct_ "data" (ptr char)
    let () = seal struct_
    let t : t typ = ptr struct_
    let new_ = foreign "wasm_byte_vec_new" (t @-> size_t @-> string @-> returning void)

    let new_uninitialized =
      foreign "wasm_byte_vec_new_uninitialized" (t @-> size_t @-> returning void)

    let copy = foreign "wasm_byte_vec_copy" (t @-> t @-> returning void)
    let delete = foreign "wasm_byte_vec_delete" (t @-> returning void)
  end

  module Config = struct
    type t = unit ptr

    let t : t typ = ptr void
    let new_ = foreign "wasm_config_new" (void @-> returning t)
    let delete = foreign "wasm_config_delete" (t @-> returning void)
  end

  module Engine = struct
    type t = unit ptr

    let t : t typ = ptr void
    let new_ = foreign "wasm_engine_new" (void @-> returning t)
    let new_with_config = foreign "wasm_engine_new_with_config" (Config.t @-> returning t)
    let delete = foreign "wasm_engine_delete" (t @-> returning void)
  end

  module Store = struct
    type t = unit ptr

    let t : t typ = ptr void
    let new_ = foreign "wasm_store_new" (Engine.t @-> returning t)
    let delete = foreign "wasm_store_delete" (t @-> returning void)
  end

  module Trap = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasm_trap_t"
    let t : t typ = ptr struct_
    let new_ = foreign "wasm_trap_new" (Store.t @-> Byte_vec.t @-> returning t)
    let message = foreign "wasm_trap_message" (t @-> Byte_vec.t @-> returning void)
    let delete = foreign "wasm_trap_delete" (t @-> returning void)
  end

  module Val = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr
    type op

    let op_typ : op union typ = union "op"
    let ( -: ) ty label = field op_typ label ty
    let i32 = int32_t -: "i32"
    let i64 = int64_t -: "i64"
    let f32 = float -: "f32"
    let f64 = double -: "f64"
    let ref = ptr void -: "ref"
    let () = seal op_typ
    let struct_ : struct_ typ = structure "wasm_val_t"
    let kind = field struct_ "kind" uint8_t
    let op = field struct_ "op" op_typ
    let () = seal struct_
    let t : t typ = ptr struct_
    let delete = foreign "wasm_val_delete" (t @-> returning void)
    let extern_ref = foreign "wasmtime_externref_new" (ptr void @-> t @-> returning void)

    let extern_ref_with_finalizer =
      foreign
        "wasmtime_externref_new_with_finalizer"
        (ptr void
        @-> static_funptr Ctypes.(ptr void @-> returning void)
        @-> t
        @-> returning void)
  end

  module Val_vec = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasm_val_vec_t"
    let size = field struct_ "size" size_t
    let data = field struct_ "data" (ptr Val.t)
    let () = seal struct_
    let t : t typ = ptr struct_
    let delete = foreign "wasm_val_vec_delete" (t @-> returning void)
  end

  module Val_type = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasm_valtype_t"
    let t : t typ = ptr struct_
    let new_ = foreign "wasm_valtype_new" (uint8_t @-> returning t)
    let delete = foreign "wasm_valtype_delete" (t @-> returning void)
  end

  module Val_type_vec = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasm_valtype_vec_t"
    let size = field struct_ "size" size_t
    let data = field struct_ "data" (ptr Val_type.t)
    let () = seal struct_
    let t : t typ = ptr struct_

    let new_ =
      foreign "wasm_valtype_vec_new" (t @-> size_t @-> ptr Val_type.t @-> returning void)

    let delete = foreign "wasm_valtype_vec_delete" (t @-> returning void)
  end

  module Memory = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasm_memory_t"
    let t : t typ = ptr struct_

    let data =
      (* Note that the returned pointer may be invalidated, e.g. if the
         memory grows and so is moved. *)
      foreign "wasm_memory_data" (t @-> returning (ptr char))

    let grow = foreign "wasm_memory_grow" (t @-> uint32_t @-> returning bool)
    let size = foreign "wasm_memory_size" (t @-> returning size_t)
    let data_size = foreign "wasm_memory_data_size" (t @-> returning size_t)
    let delete = foreign "wasm_memory_delete" (t @-> returning void)
  end

  module Func_type = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasm_functype_t"
    let t : t typ = ptr struct_

    let new_ =
      foreign "wasm_functype_new" (Val_type_vec.t @-> Val_type_vec.t @-> returning t)

    let new_0_0 = foreign "wasm_functype_new_0_0" (void @-> returning t)
    let delete = foreign "wasm_functype_delete" (t @-> returning void)
  end

  module Func = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasm_func_t"
    let t : t typ = ptr struct_

    let new_ =
      foreign
        "wasm_func_new"
        (Store.t
        @-> Func_type.t
        @-> static_funptr Ctypes.(Val.t @-> Val.t @-> returning Trap.t)
        @-> returning t)

    let delete = foreign "wasm_func_delete" (t @-> returning void)
  end

  module Global = struct
    type t = unit ptr

    let t : t typ = ptr void
    let get = foreign "wasm_global_get" (t @-> Val.t @-> returning void)
    let set = foreign "wasm_global_set" (t @-> Val.t @-> returning void)
    let delete = foreign "wasm_global_delete" (t @-> returning void)
  end

  module Ref = struct
    type t = unit ptr

    let t : t typ = ptr void
    let delete = foreign "wasm_ref_delete" (t @-> returning void)
  end

  module Table = struct
    type t = unit ptr

    let t : t typ = ptr void
    let get = foreign "wasm_table_get" (t @-> uint32_t @-> returning Ref.t)
    let set = foreign "wasm_table_set" (t @-> uint32_t @-> Ref.t @-> returning void)
    let size = foreign "wasm_table_size" (t @-> returning uint32_t)
    let delete = foreign "wasm_table_delete" (t @-> returning void)
  end

  module Extern = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasm_extern_t"
    let t : t typ = ptr struct_
    let delete = foreign "wasm_extern_delete" (t @-> returning void)
    let as_func = foreign "wasm_extern_as_func" (t @-> returning Func.t)
    let func_as = foreign "wasm_func_as_extern" (Func.t @-> returning t)
    let as_memory = foreign "wasm_extern_as_memory" (t @-> returning Memory.t)
    let as_table = foreign "wasm_extern_as_table" (t @-> returning Table.t)
    let as_global = foreign "wasm_extern_as_global" (t @-> returning Global.t)
  end

  module Extern_vec = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasm_extern_vec_t"
    let size = field struct_ "size" size_t
    let data = field struct_ "data" (ptr Extern.t)
    let () = seal struct_
    let t : t typ = ptr struct_
    let delete = foreign "wasm_extern_vec_delete" (t @-> returning void)
  end

  module Module = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasm_module_t"
    let t : t typ = ptr struct_
    let delete = foreign "wasm_module_delete" (t @-> returning void)
  end

  module Instance = struct
    type modl
    type struct_ = modl Ctypes.structure
    type t = struct_ ptr

    let struct_ : struct_ typ = structure "wasm_instance_t"
    let t : t typ = ptr struct_
    let delete = foreign "wasm_instance_delete" (t @-> returning void)

    let new_ =
      foreign
        "wasm_instance_new"
        (Store.t @-> Module.t @-> ptr Extern.t @-> ptr Trap.t @-> returning t)

    let exports = foreign "wasm_instance_exports" (t @-> Extern_vec.t @-> returning void)
  end

  (* wasmtime.h specific bits *)
  module Error = struct
    type t = unit ptr

    let t : t typ = ptr void
    let message = foreign "wasmtime_error_message" (t @-> Byte_vec.t @-> returning void)
    let delete = foreign "wasmtime_error_delete" (t @-> returning void)
  end

  module Wasi_config = struct
    type t = unit ptr

    let t : t typ = ptr void
    let new_ = foreign "wasi_config_new" (void @-> returning t)
    let delete = foreign "wasi_config_delete" (t @-> returning void)
    let inherit_argv = foreign "wasi_config_inherit_argv" (t @-> returning void)
    let inherit_env = foreign "wasi_config_inherit_env" (t @-> returning void)
    let inherit_stdin = foreign "wasi_config_inherit_stdin" (t @-> returning void)
    let inherit_stdout = foreign "wasi_config_inherit_stdout" (t @-> returning void)
    let inherit_stderr = foreign "wasi_config_inherit_stderr" (t @-> returning void)

    let preopen_dir =
      foreign "wasi_config_preopen_dir" (t @-> string @-> string @-> returning bool)
  end

  module Wasi_instance = struct
    type t = unit ptr

    let t : t typ = ptr void

    let new_ =
      foreign
        "wasi_instance_new"
        (Store.t @-> string @-> Wasi_config.t @-> ptr Trap.t @-> returning t)

    let delete = foreign "wasi_instance_delete" (t @-> returning void)
  end

  module Wasmtime = struct
    module Linker = struct
      type t = unit ptr

      let t : t typ = ptr void
      let new_ = foreign "wasmtime_linker_new" (Store.t @-> returning t)
      let delete = foreign "wasmtime_linker_delete" (t @-> returning void)

      let define_wasi =
        foreign "wasmtime_linker_define_wasi" (t @-> Wasi_instance.t @-> returning Error.t)

      let define_instance =
        foreign
          "wasmtime_linker_define_instance"
          (t @-> Byte_vec.t @-> Instance.t @-> returning Error.t)

      let instantiate =
        foreign
          "wasmtime_linker_instantiate"
          (t @-> Module.t @-> ptr Instance.t @-> ptr Trap.t @-> returning Error.t)

      let module_ =
        foreign
          "wasmtime_linker_module"
          (t @-> Byte_vec.t @-> Module.t @-> returning Error.t)

      let get_default =
        foreign
          "wasmtime_linker_get_default"
          (t @-> Byte_vec.t @-> ptr Func.t @-> returning Error.t)
    end

    module Config = struct
      let debug_info_set =
        foreign "wasmtime_config_debug_info_set" (Config.t @-> bool @-> returning void)

      let interruptable_set =
        foreign "wasmtime_config_interruptable_set" (Config.t @-> bool @-> returning void)

      let max_wasm_stack_set =
        foreign
          "wasmtime_config_max_wasm_stack_set"
          (Config.t @-> size_t @-> returning void)

      let threads_set =
        foreign "wasmtime_config_wasm_threads_set" (Config.t @-> bool @-> returning void)

      let reference_types_set =
        foreign
          "wasmtime_config_wasm_reference_types_set"
          (Config.t @-> bool @-> returning void)

      let simd_set =
        foreign "wasmtime_config_wasm_simd_set" (Config.t @-> bool @-> returning void)

      let bulk_memory_set =
        foreign
          "wasmtime_config_wasm_bulk_memory_set"
          (Config.t @-> bool @-> returning void)

      let multi_value_set =
        foreign
          "wasmtime_config_wasm_multi_value_set"
          (Config.t @-> bool @-> returning void)

      let static_memory_maximum_size_set =
        foreign
          "wasmtime_config_static_memory_maximum_size_set"
          (Config.t @-> int64_t @-> returning void)

      let static_memory_guard_size_set =
        foreign
          "wasmtime_config_static_memory_guard_size_set"
          (Config.t @-> int64_t @-> returning void)

      let dynamic_memory_guard_size_set =
        foreign
          "wasmtime_config_dynamic_memory_guard_size_set"
          (Config.t @-> int64_t @-> returning void)
    end

    let wat2wasm =
      foreign "wasmtime_wat2wasm" (Byte_vec.t @-> Byte_vec.t @-> returning Error.t)

    let new_module =
      foreign
        "wasmtime_module_new"
        (Engine.t @-> Byte_vec.t @-> ptr Module.t @-> returning Error.t)

    let func_call =
      foreign
        "wasmtime_func_call"
        (Func.t
        @-> ptr Val.struct_
        @-> size_t
        @-> ptr Val.struct_
        @-> size_t
        @-> ptr Trap.t
        @-> returning Error.t)

    let new_instance =
      foreign
        "wasmtime_instance_new"
        (Store.t
        @-> Module.t
        @-> ptr Extern.t
        @-> size_t
        @-> ptr Instance.t
        @-> ptr Trap.t
        @-> returning Error.t)
  end
end
