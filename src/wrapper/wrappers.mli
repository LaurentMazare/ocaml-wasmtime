(* A low-level but hopefully type safe version of the API. *)
open! Base

exception Trap of { message : string }

module Engine : sig
  type t

  val create : unit -> t
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
end

module Memory : sig
  type t

  (* Size in bytes. *)
  val data_size : t -> int
end

module Extern : sig
  type t

  val as_func : t -> Func.t
  val as_memory : t -> Memory.t
end

module Instance : sig
  type t

  val exports : t -> Extern.t list
end

module Val : sig
  type t =
    | Int32 of int
    | Int64 of int
    | Float32 of float
    | Float64 of float

  val int_exn : t -> int
  val float_exn : t -> float

  module Kind : sig
    type t =
      | Int32
      | Int64
      | Float32
      | Float64
      | Any_ref
      | Func_ref
  end

  val kind : t -> Kind.t
end

module Wasi_config : sig
  type t

  val create : unit -> t
  val inherit_argv : t -> unit
  val inherit_env : t -> unit
  val inherit_stdin : t -> unit
  val inherit_stdout : t -> unit
  val inherit_stderr : t -> unit
  val preopen_dir : t -> string -> string -> unit
end

module Wasi_instance : sig
  type t

  val create
    :  Store.t
    -> [ `wasi_unstable | `wasi_snapshot_preview ]
    -> Wasi_config.t
    -> t
end

module Wasmtime : sig
  module Linker : sig
    type t

    val create : Store.t -> t
    val define_wasi : t -> Wasi_instance.t -> unit
    val module_ : t -> name:Byte_vec.t -> Module.t -> unit
    val get_default : t -> name:Byte_vec.t -> Func.t
  end

  val wat_to_wasm : wat:Byte_vec.t -> Byte_vec.t
  val new_module : Engine.t -> wasm:Byte_vec.t -> Module.t
  val new_instance : Store.t -> Module.t -> Instance.t
  val func_call0 : Func.t -> Val.t list -> unit
  val func_call1 : Func.t -> Val.t list -> Val.t
  val func_call2 : Func.t -> Val.t list -> Val.t * Val.t
  val func_call : Func.t -> Val.t list -> n_outputs:int -> Val.t list
end
