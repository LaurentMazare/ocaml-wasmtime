(* A low-level but hopefully type safe version of the API. *)
open! Base

exception Trap of { message : string }

module Engine : sig
  type t

  val create
    :  ?debug_info:bool
    -> ?interruptable:bool
    -> ?max_wasm_stack:int
    -> ?reference_types:bool
    -> ?simd:bool
    -> ?bulk_memory:bool
    -> ?multi_value:bool
    -> ?static_memory_maximum_size:int
    -> ?static_memory_guard_size:int
    -> ?dynamic_memory_guard_size:int
    -> unit
    -> t
end

module Store : sig
  type t

  val create : Engine.t -> t
end

module Byte_vec : sig
  type t

  val create : len:int -> t
  val of_string : string -> t
  val to_string : t -> string
  val length : t -> int
end

module Module : sig
  type t
end

module Func : sig
  type t

  val of_func_0_0 : Store.t -> (unit -> unit) -> t

  val of_func
    :  args:'a Val.Kind.tuple
    -> results:'b Val.Kind.tuple
    -> Store.t
    -> ('a -> 'b)
    -> t

  val of_func_list
    :  args:Val.Kind.packed list
    -> results:Val.Kind.packed list
    -> Store.t
    -> (Val.t list -> Val.t list)
    -> t
end

module Memory : sig
  type t

  val size_in_pages : t -> int
  val size_in_bytes : t -> int
  val grow : t -> int -> bool

  (* [to_string] makes a copy of the memory data. *)
  val to_string : t -> pos:int -> len:int -> string
  val get : t -> pos:int -> char
  val set : t -> pos:int -> char -> unit
end

module Extern : sig
  type t

  val func_as : Func.t -> t
  val as_func : t -> Func.t
  val as_memory : t -> Memory.t
end

module Instance : sig
  type t

  val exports : t -> Extern.t list
end

module Wasi_instance : sig
  type t

  val create
    :  ?inherit_argv:bool
    -> ?inherit_env:bool
    -> ?inherit_stdin:bool
    -> ?inherit_stdout:bool
    -> ?inherit_stderr:bool
    -> ?preopen_dirs:(string * string) list
    -> Store.t
    -> [ `wasi_unstable | `wasi_snapshot_preview ]
    -> t
end

module Wasmtime : sig
  module Linker : sig
    type t

    val create : Store.t -> t
    val define_wasi : t -> Wasi_instance.t -> unit
    val define_instance : t -> name:Byte_vec.t -> Instance.t -> unit
    val instantiate : t -> Module.t -> Instance.t
    val module_ : t -> name:Byte_vec.t -> Module.t -> unit
    val get_default : t -> name:Byte_vec.t -> Func.t
  end

  val wat_to_wasm : wat:Byte_vec.t -> Byte_vec.t
  val new_module : Engine.t -> wasm:Byte_vec.t -> Module.t
  val new_instance : ?imports:Extern.t list -> Store.t -> Module.t -> Instance.t
  val func_call0 : Func.t -> Val.t list -> unit
  val func_call1 : Func.t -> Val.t list -> Val.t
  val func_call2 : Func.t -> Val.t list -> Val.t * Val.t

  val func_call
    :  args:'a Val.Kind.tuple
    -> results:'b Val.Kind.tuple
    -> Func.t
    -> 'a
    -> 'b

  val func_call_list : Func.t -> Val.t list -> n_outputs:int -> Val.t list
end
